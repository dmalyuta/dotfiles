"""Run a command inside the guest through the QEMU guest agent.

    guest-run.py <domain> <timeout-seconds> <program> [argument ...]

Prints whatever the program wrote to stdout and stderr, and exits with the
program's own exit code. Exits 125 if the agent could not be reached, which
callers use to tell "the guest is not answering" apart from "the command
failed".

This exists because the agent speaks JSON over virsh: a command has to be
packed into a guest-exec request, and its output comes back base64-encoded
from a separate guest-exec-status poll. Doing that in shell means quoting
JSON inside shell inside JSON, which is where the bugs live.

Written by make_windows_vm.sh; edit that instead of this.
"""

import base64
import json
import subprocess
import sys
import time

AGENT_UNREACHABLE = 125


def agent(domain, payload):
    """Send one command to the guest agent and return its "return" value."""
    proc = subprocess.run(
        ["virsh", "--connect", "qemu:///system", "qemu-agent-command",
         domain, json.dumps(payload)],
        capture_output=True, text=True,
    )
    if proc.returncode != 0:
        raise RuntimeError(proc.stderr.strip() or "virsh failed")
    return json.loads(proc.stdout)["return"]


def main():
    if len(sys.argv) < 4:
        sys.exit(__doc__)

    domain, timeout, program = sys.argv[1], float(sys.argv[2]), sys.argv[3]
    args = sys.argv[4:]

    try:
        started = agent(domain, {
            "execute": "guest-exec",
            "arguments": {
                "path": program,
                "arg": args,
                "capture-output": True,
            },
        })
    except Exception as exc:                      # noqa: BLE001
        print(f"guest agent unreachable: {exc}", file=sys.stderr)
        return AGENT_UNREACHABLE

    pid = started["pid"]
    deadline = time.monotonic() + timeout

    while True:
        try:
            status = agent(domain, {
                "execute": "guest-exec-status",
                "arguments": {"pid": pid},
            })
        except Exception as exc:                  # noqa: BLE001
            print(f"guest agent unreachable: {exc}", file=sys.stderr)
            return AGENT_UNREACHABLE

        if status.get("exited"):
            break
        if time.monotonic() > deadline:
            print(f"timed out after {timeout:g}s waiting for {program}",
                  file=sys.stderr)
            return AGENT_UNREACHABLE
        time.sleep(1)

    for key, stream in (("out-data", sys.stdout), ("err-data", sys.stderr)):
        if status.get(key):
            stream.write(base64.b64decode(status[key]).decode("utf-8", "replace"))
            stream.flush()

    # A command killed by a signal reports signal rather than exitcode.
    if "exitcode" in status:
        return status["exitcode"]
    return 0 if status.get("signal") is None else 128 + status["signal"]


if __name__ == "__main__":
    sys.exit(main())
