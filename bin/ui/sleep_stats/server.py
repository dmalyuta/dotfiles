#!/usr/bin/env python3
"""Local web page of the laptop's sleep and hibernation battery drain, started by bin/sleep_stats.

Serves the page in static/ on 127.0.0.1 and opens it in the browser. The page gets its data from
/api/data, which re-reads everything on each load, so reloading the page picks up new sleeps:
- /var/log/sleep-stats.csv, one row per sleep step, written by scripts/sleep-stats (its header
  documents the columns).
- The battery in /sys/class/power_supply, for its capacity now, to turn Wh into %.
- HibernateDelaySec from systemd's sleep.conf, the delay suspend-then-hibernate uses now.

Only steps on battery throughout count towards the fits: on AC the energy columns show charging,
not drain. Rows written with clock=unsynced have an unsure duration and are left out too.

Each fit is energy used = a + b * duration, robust to outliers (see fit.py), with a >= 0: a is
the fixed cost of one step (going down and coming back), b the steady draw in W.
"""

import argparse
import csv
import glob
import json
import os
import re
import sys
import threading
import webbrowser
from datetime import datetime
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer

from fit import robust_line

HERE = os.path.dirname(os.path.abspath(__file__))
STATIC = os.path.join(HERE, "static")
FILES = {
    "/": ("index.html", "text/html; charset=utf-8"),
    "/index.html": ("index.html", "text/html; charset=utf-8"),
    "/app.js": ("app.js", "text/javascript; charset=utf-8"),
    "/style.css": ("style.css", "text/css; charset=utf-8"),
}
POWER_SUPPLY = "/sys/class/power_supply"


def num(value):
    try:
        return float(value)
    except (TypeError, ValueError):
        return None


def read_sysfs(path):
    try:
        with open(path) as f:
            return f.read().strip()
    except OSError:
        return None


def battery():
    """The first battery's state now, or None on a machine without one."""
    bats = sorted(glob.glob(os.path.join(POWER_SUPPLY, "BAT*")))
    if not bats:
        return None
    bat = bats[0]

    def get(name):
        return read_sysfs(os.path.join(bat, name))

    def micro(name):  # µWh, µW, µV -> Wh, W, V
        v = num(get(name))
        return v / 1e6 if v is not None else None

    acs = sorted(glob.glob(os.path.join(POWER_SUPPLY, "AC*")))
    ac = read_sysfs(os.path.join(acs[0], "online")) if acs else None
    cycles = num(get("cycle_count"))
    return {
        "name": os.path.basename(bat),
        "manufacturer": get("manufacturer"),
        "model": get("model_name"),
        "technology": get("technology"),
        "status": get("status"),
        "ac_online": None if ac is None else ac == "1",
        "capacity_pct": num(get("capacity")),
        "energy_now_wh": micro("energy_now"),
        "energy_full_wh": micro("energy_full"),
        "energy_full_design_wh": micro("energy_full_design"),
        "power_now_w": micro("power_now"),
        "voltage_now_v": micro("voltage_now"),
        "charge_limit_pct": num(get("charge_control_end_threshold")),
        "cycle_count": int(cycles) if cycles else None,  # 0 means not reported here
    }


TIME_UNITS = {
    "us": 1e-6, "usec": 1e-6, "ms": 1e-3, "msec": 1e-3,
    "s": 1, "sec": 1, "second": 1, "seconds": 1,
    "m": 60, "min": 60, "minute": 60, "minutes": 60,
    "h": 3600, "hr": 3600, "hour": 3600, "hours": 3600,
    "d": 86400, "day": 86400, "days": 86400,
    "w": 604800, "week": 604800, "weeks": 604800,
}


def parse_timespan(text):
    """A systemd time span like 1h, 90min or '1h 30min' in seconds; a bare number is seconds."""
    text = text.strip()
    if re.fullmatch(r"\d+(\.\d+)?", text):
        return float(text)
    total, pos = 0.0, 0
    for m in re.finditer(r"\s*(\d+(?:\.\d+)?)\s*([a-z]+)", text):
        if m.start() != pos or m.group(2) not in TIME_UNITS:
            return None
        total += float(m.group(1)) * TIME_UNITS[m.group(2)]
        pos = m.end()
    return total if pos == len(text) and pos else None


def hibernate_delay():
    """HibernateDelaySec as systemd-sleep sees it: sleep.conf, then the drop-ins by file name,
    one in /etc replacing one of the same name in /usr/lib, the last setting winning."""
    dropins = {}
    for d in ("/usr/lib/systemd/sleep.conf.d", "/run/systemd/sleep.conf.d",
              "/etc/systemd/sleep.conf.d"):
        for path in glob.glob(os.path.join(d, "*.conf")):
            dropins[os.path.basename(path)] = path
    found = None
    for path in ["/etc/systemd/sleep.conf"] + [dropins[k] for k in sorted(dropins)]:
        try:
            with open(path) as f:
                for line in f:
                    m = re.match(r"\s*HibernateDelaySec\s*=\s*(.*?)\s*$", line)
                    if m:
                        seconds = parse_timespan(m.group(1))
                        found = (
                            {"seconds": seconds, "value": m.group(1), "file": path}
                            if seconds is not None else None
                        )
        except OSError:
            pass
    return found


def exclusion(row):
    """Why a row can't count towards the drain fits, or None if it can."""
    if row["clock"] != "ok":
        return "clock unsynced"
    if num(row["duration_s"]) is None or num(row["used_wh"]) is None:
        return "unreadable"
    if num(row["duration_s"]) <= 0:
        return "no duration"
    if row["ac_start"] != "0" or row["ac_end"] != "0":
        return "on AC"
    if row["status_start"] != "Discharging" or row["status_end"] != "Discharging":
        return "not discharging"
    if re.search(r"(^|;)AC\d*@", row.get("woke", "")):
        return "charger plugged in or out"
    return None


def kind(phase):
    if phase == "hibernate":
        return "hibernate"
    if phase.startswith("suspend"):  # Also suspend-after-failed-hibernate
        return "suspend"
    return None


def load(csv_path):
    try:
        with open(csv_path, newline="") as f:
            rows = list(csv.DictReader(f))
    except OSError as e:
        return f"Can't read {csv_path}: {e.strerror}", []

    out = []
    for i, r in enumerate(rows):
        k = kind(r.get("phase", ""))
        reason = exclusion(r) if k else "unknown phase"
        out.append({
            "id": i,
            "kind": k,
            "phase": r.get("phase"),
            "operation": r.get("operation"),
            "session": r.get("session"),
            "start": r.get("start"),
            "end": r.get("end"),
            "duration_s": num(r.get("duration_s")),
            "used_wh": num(r.get("used_wh")),
            "avg_w": num(r.get("avg_w")),
            "capacity_start": num(r.get("capacity_start")),
            "capacity_end": num(r.get("capacity_end")),
            "energy_full_wh": num(r.get("energy_full_wh")),
            "hw_sleep_s": num(r.get("hw_sleep_s")),
            "woke": r.get("woke") or "",
            "wakeup_irq": r.get("wakeup_irq") or "",
            "status": f"{r.get('status_start')} → {r.get('status_end')}",
            "excluded": reason,
            "outlier": False,
            "weight": None,
        })
    return None, out


def fit_kind(rows, k):
    used = [r for r in rows if r["kind"] == k and not r["excluded"]]
    fit = robust_line([r["duration_s"] / 3600 for r in used], [r["used_wh"] for r in used])
    if not fit:
        return {"n": len(used)}
    for r, w, o in zip(used, fit.pop("weights"), fit.pop("outlier")):
        r["weight"], r["outlier"] = w, o
    return fit  # x in h, so the slope is in Wh/h = W


def data(csv_path):
    error, rows = load(csv_path)
    bat = battery()
    # % of the battery's full capacity now, as the kernel's capacity is; from the newest row if the
    # battery can't be read.
    full = bat and bat["energy_full_wh"]
    if not full:
        full = next((r["energy_full_wh"] for r in reversed(rows) if r["energy_full_wh"]), None)
    excluded = {}
    for r in rows:
        if r["excluded"]:
            excluded[r["excluded"]] = excluded.get(r["excluded"], 0) + 1
    return {
        "generated": datetime.now().astimezone().isoformat(timespec="seconds"),
        "csv": csv_path,
        "error": error,
        "battery": bat,
        "capacity_wh": full,
        "hibernate_delay": hibernate_delay(),
        "fits": {k: fit_kind(rows, k) for k in ("suspend", "hibernate")},
        "excluded": excluded,
        "rows": rows,
    }


class Handler(BaseHTTPRequestHandler):
    csv_path = None

    def send(self, code, body, content_type):
        self.send_response(code)
        self.send_header("Content-Type", content_type)
        self.send_header("Content-Length", str(len(body)))
        self.send_header("Cache-Control", "no-store")
        self.end_headers()
        self.wfile.write(body)

    def do_GET(self):
        path = self.path.split("?", 1)[0]
        if path == "/api/data":
            body = json.dumps(data(self.csv_path)).encode()
            self.send(200, body, "application/json")
        elif path in FILES:
            name, ctype = FILES[path]
            with open(os.path.join(STATIC, name), "rb") as f:
                self.send(200, f.read(), ctype)
        else:
            self.send(404, b"Not found", "text/plain")

    def log_message(self, *args):
        pass


def main():
    p = argparse.ArgumentParser(
        prog="sleep_stats",
        description="Show the laptop's sleep and hibernation battery drain in the browser.")
    p.add_argument("--port", type=int, default=0, help="port on 127.0.0.1 (default: any free one)")
    p.add_argument("--csv", default="/var/log/sleep-stats.csv", help="the sleep stats log")
    p.add_argument("--no-browser", action="store_true", help="only print the address")
    p.add_argument("--dump", action="store_true", help="print the page's data as JSON and exit")
    args = p.parse_args()

    if args.dump:
        json.dump(data(args.csv), sys.stdout, indent=1)
        print()
        return
    Handler.csv_path = args.csv
    try:
        server = ThreadingHTTPServer(("127.0.0.1", args.port), Handler)
    except OSError as e:
        sys.exit(f"sleep_stats: can't listen on port {args.port}: {e.strerror}")
    url = f"http://127.0.0.1:{server.server_port}/"
    print(f"Sleep stats at {url} (Ctrl+C to stop)", flush=True)
    if not args.no_browser:
        threading.Thread(target=webbrowser.open, args=(url,), daemon=True).start()
    try:
        server.serve_forever()
    except KeyboardInterrupt:
        print()


if __name__ == "__main__":
    main()
