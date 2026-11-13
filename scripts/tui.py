#!/usr/bin/env python3
"""Run a setup script in a terminal-in-the-terminal, under a progress bar.

Usage: tui.py --script FILE [--dotfiles DIR] [--raw-base URL] [--log FILE] -- COMMAND...

Drawn in place below what the terminal already shows: a spinner and a bar of how far through the
steps the script is, the current step, and a box with the tail of the command's output. The command
runs in a pseudo-terminal the size of that box, so prompts, progress bars and colors work as usual,
and keys go straight to it. pyte keeps the box's screen.

The steps come from `step [WEIGHT] "Description"` lines (see scripts/functions.sh), read in order
from FILE and from the scripts it runs as `bash "$dotfiles"/scripts/NAME`, which are read from DIR,
or from URL before the repo is cloned. WEIGHT is the step's rough share of the time, 1 by default.
At run time, `step` appends "step Description" to the file named by $START_FRESH_STATUS, and
`steps_done` appends "done", and `failed` appends "failed Message", for the list of errors left on
screen at the end. A step that doesn't run is skipped over.

Without a terminal or pyte, it just runs the command.

Author: Danylo Malyuta, 2026.
"""

import argparse
import fcntl
import os
import pty
import re
import select
import signal
import struct
import sys
import tempfile
import termios
import time
import tty
import urllib.request

try:
    import pyte
    import pyte.graphics
except ImportError:
    pyte = None

STEP = re.compile(r'(?:^|&&|\|\||;|\bthen)\s*step\s+(?:(\d+)\s+)?"([^"$`]+)"')
CALL = re.compile(r'\bbash\s+"\$dotfiles"/scripts/([\w./-]+)')
SPINNER = "⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏"
EIGHTHS = " ▏▎▍▌▋▊▉"
FPS = 30

# Colors of the frame: the bar's fill (bright blue, like `say`), its track, the box border.
BAR = "\x1b[94m"
TRACK = "\x1b[48;5;236m"
BORDER = "\x1b[90m"
BOLD = "\x1b[1m"
RED = "\x1b[1;91m"
GREEN = "\x1b[1;92m"
RESET = "\x1b[0m"


# --------------------------------------------------------------------------------------------------
# The plan of steps.
# --------------------------------------------------------------------------------------------------


def read_script(path, raw_base, rel):
    try:
        with open(path) as f:
            return f.read()
    except OSError:
        if not raw_base:
            return ""
        try:
            with urllib.request.urlopen(f"{raw_base}/{rel}", timeout=10) as r:
                return r.read().decode()
        except Exception:
            return ""


def plan_steps(text, dotfiles, raw_base, depth=0):
    """(description, weight) of each step, in the order they run."""
    steps = []
    for line in text.splitlines():
        if line.lstrip().startswith("#"):
            continue
        if m := STEP.search(line):
            steps.append((m[2], int(m[1] or 1)))
        elif (m := CALL.search(line)) and depth < 3:
            rel = f"scripts/{m[1]}"
            sub = read_script(os.path.join(dotfiles, rel), raw_base, rel)
            steps += plan_steps(sub, dotfiles, raw_base, depth + 1)
    return steps


class Progress:
    def __init__(self, steps):
        self.steps = steps
        self.total = sum(w for _, w in steps) or 1
        self.starts = []
        at = 0
        for _, w in steps:
            self.starts.append(at)
            at += w
        self.index = -1
        self.target = 0.0
        self.shown = 0.0
        self.description = "Starting"

    def step(self, description):
        self.description = description
        # The next step with this description, so repeated ones count in order.
        for i in range(self.index + 1, len(self.steps)):
            if self.steps[i][0] == description:
                self.index = i
                self.target = self.starts[i] / self.total
                return

    def finish(self, description):
        self.description = description
        self.target = 1.0

    def animate(self, dt):
        """Glide the shown fraction toward the target."""
        self.shown += (self.target - self.shown) * min(1.0, dt * 6)
        if abs(self.target - self.shown) < 0.0005:
            self.shown = self.target


# --------------------------------------------------------------------------------------------------
# Drawing.
# --------------------------------------------------------------------------------------------------


def color_codes():
    """SGR codes for pyte's color names, and for the hex values of the 16 palette colors, so those
    stay in the terminal's own palette."""
    g = pyte.graphics
    fg = {name: str(code) for code, name in {**g.FG_ANSI, **g.FG_AIXTERM}.items()}
    bg = {name: str(code) for code, name in {**g.BG_ANSI, **g.BG_AIXTERM}.items()}
    palette = {hexval: i for i, hexval in reversed(list(enumerate(g.FG_BG_256[:16])))}
    return fg, bg, palette, {}


def sgr(char, codes):
    key = tuple(char[1:])
    if key not in codes[3]:
        codes[3][key] = make_sgr(char, codes)
    return codes[3][key]


def make_sgr(char, codes):
    fg_names, bg_names, palette, _ = codes
    out = ["0"]
    if char.bold:
        out.append("1")
    if char.italics:
        out.append("3")
    if char.underscore:
        out.append("4")
    if char.blink:
        out.append("5")
    if char.reverse:
        out.append("7")
    if char.strikethrough:
        out.append("9")
    for value, names, base in ((char.fg, fg_names, "38"), (char.bg, bg_names, "48")):
        if value == "default":
            continue
        if value in names:
            out.append(names[value])
        elif value in palette:
            out.append(f"{base};5;{palette[value]}")
        elif re.fullmatch(r"[0-9a-fA-F]{6}", value):
            r, g, b = (int(value[i : i + 2], 16) for i in (0, 2, 4))
            out.append(f"{base};2;{r};{g};{b}")
    return "\x1b[" + ";".join(out) + "m"


def render_row(row, columns, codes):
    """One line of the box's screen, with its colors."""
    out, last = [], None
    for x in range(columns):
        char = row[x]
        if char.data == "":  # The second half of a wide character.
            continue
        style = sgr(char, codes)
        if style != last:
            out.append(style)
            last = style
        out.append(char.data)
    out.append(RESET)
    return "".join(out)


def bar(fraction, width):
    """A bar `width` cells wide, filled to `fraction` in eighths of a cell."""
    eighths = round(fraction * width * 8)
    full, part = divmod(eighths, 8)
    full = min(full, width)
    cells = "█" * full
    if full < width:
        cells += EIGHTHS[part] + " " * (width - full - 1)
    return f"{BAR}{TRACK}{cells}{RESET}"


class Layout:
    """Where everything goes on a terminal of `rows` x `cols`."""

    def __init__(self, rows, cols):
        self.rows, self.cols = rows, cols
        self.box_top = 2  # 0-based: bar, description, then the box.
        self.box_height = max(5, min(rows * 3 // 4, rows - self.box_top))
        self.inner_lines = self.box_height - 2
        self.inner_cols = max(10, cols - 4)


def header(progress, layout, frame, state):
    cols = layout.cols
    if state == "running":
        icon = f"{BAR}{SPINNER[frame % len(SPINNER)]}{RESET}"
    elif state == "done":
        icon = f"{GREEN}✓{RESET}"
    else:
        icon = f"{RED}✗{RESET}"
    percent = int(progress.shown * 100)
    width = max(5, cols - 10)
    line1 = f" {icon} {bar(progress.shown, width)} {percent:3d}%"
    description = progress.description[: cols - 4]
    style = RED if state == "failed" else BOLD
    line2 = f"   {style}{description}{RESET}"
    return line1, line2


class Rows:
    """The box's screen lines, rendered again only once pyte marks them changed."""

    def __init__(self, screen, codes):
        self.screen, self.codes, self.cache = screen, codes, {}

    def get(self, y):
        screen = self.screen
        if screen.dirty:
            for d in screen.dirty:
                self.cache.pop(d, None)
            screen.dirty.clear()
        if y not in self.cache:
            self.cache[y] = render_row(screen.buffer[y], screen.columns, self.codes)
        return self.cache[y]

    def clear(self):
        self.cache.clear()


def box_lines(rows, layout, title, lines=None):
    width = layout.cols - 2
    label = f"─ {title} " if len(title) + 4 < width else ""
    top = f"{BORDER}╭{label}{'─' * (width - len(label))}╮{RESET}"
    bottom = f"{BORDER}╰{'─' * width}╯{RESET}"
    side = f"{BORDER}│{RESET}"
    rows_range = range(layout.inner_lines) if lines is None else range(lines)
    body = [f"{side} {rows.get(y)} {side}" for y in rows_range]
    return [top, *body, bottom]


# --------------------------------------------------------------------------------------------------
# Running the command.
# --------------------------------------------------------------------------------------------------


def terminal_size():
    try:
        cols, rows = os.get_terminal_size(sys.stdout.fileno())
    except OSError:
        cols, rows = 80, 24
    return rows, cols


def set_size(fd, rows, cols):
    fcntl.ioctl(fd, termios.TIOCSWINSZ, struct.pack("HHHH", rows, cols, 0, 0))


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--script", required=True)
    parser.add_argument("--dotfiles", default="")
    parser.add_argument("--raw-base", default="")
    parser.add_argument("--log", default=os.path.expanduser("~/.cache/start_fresh.log"))
    parser.add_argument("--title", default="start_fresh")
    parser.add_argument("command", nargs="+")
    args = parser.parse_args()

    if pyte is None or not (sys.stdin.isatty() and sys.stdout.isatty()):
        os.execvp(args.command[0], args.command)

    with open(args.script) as f:
        progress = Progress(plan_steps(f.read(), args.dotfiles, args.raw_base))

    status_fd, status_path = tempfile.mkstemp(prefix="start_fresh-status-")
    os.close(status_fd)
    os.makedirs(os.path.dirname(args.log), exist_ok=True)
    # The output as it showed, in plain text: each line once it leaves the box's screen, and at the
    # end the lines still on it. Only for the user: it has the answers to the script's questions.
    log = open(os.open(args.log, os.O_WRONLY | os.O_CREAT | os.O_TRUNC, 0o600), "w")
    os.chmod(args.log, 0o600)

    def log_row(row, columns):
        log.write("".join(row[x].data for x in range(columns)).rstrip() + "\n")

    layout = Layout(*terminal_size())
    pid, master = pty.fork()
    if pid == 0:
        set_size(0, layout.inner_lines, layout.inner_cols)
        os.environ["START_FRESH_STATUS"] = status_path
        # What pyte emulates, rather than the outer terminal, whose own sequences it may not know.
        os.environ["TERM"] = "xterm-256color"
        # No Ctrl+Z: it would stop the command with no shell around to resume it.
        attrs = termios.tcgetattr(0)
        attrs[6][termios.VSUSP] = 0
        termios.tcsetattr(0, termios.TCSANOW, attrs)
        try:
            os.execvp(args.command[0], args.command)
        finally:
            os._exit(127)

    class Screen(pyte.Screen):
        # Answers to the command's queries, like the cursor position.
        def write_process_input(self, data):
            os.write(master, data.encode())

        # A new line at the bottom scrolls the top one off the screen.
        def index(self):
            top, bottom = self.margins or (0, self.lines - 1)
            if self.cursor.y == bottom and top == 0:
                log_row(self.buffer[0], self.columns)
            super().index()

        # A shorter screen drops lines from the top.
        def resize(self, lines=None, columns=None):
            for y in range(max(0, self.lines - (lines or self.lines))):
                log_row(self.buffer[y], self.columns)
            super().resize(lines, columns)
            # pyte leaves the cursor on its old line, which may now be below the screen: the next
            # line would then overwrite the last instead of scrolling.
            self.cursor.y = min(self.cursor.y, self.lines - 1)

        def used_lines(self):
            """Down to the last line with something on it."""
            return max((y + 1 for y in range(self.lines)
                        if "".join(c.data for c in self.buffer[y].values()).strip()), default=0)

    screen = Screen(layout.inner_cols, layout.inner_lines)
    stream = pyte.ByteStream(screen)
    rows = Rows(screen, color_codes())
    set_size(master, layout.inner_lines, layout.inner_cols)

    resized = False

    def on_winch(*_):
        nonlocal resized
        resized = True

    signal.signal(signal.SIGWINCH, on_winch)
    # Closing the terminal or killing this hangs up the command too, as it would without this.
    def on_hangup(sig, _):
        try:
            os.kill(pid, signal.SIGHUP)
        except ProcessLookupError:
            pass
        sys.exit(128 + sig)

    for sig in (signal.SIGHUP, signal.SIGTERM):
        signal.signal(sig, on_hangup)

    stdin = sys.stdin.fileno()
    saved = termios.tcgetattr(stdin)
    out = sys.stdout.buffer

    def write(text):
        out.write(text.encode())
        out.flush()

    status = open(status_path)
    partial = ""
    failures = []

    def read_status():
        nonlocal partial
        partial += status.read()
        *lines, partial = partial.split("\n")
        for line in lines:
            if line.startswith("step "):
                progress.step(line[5:])
            elif line.startswith("failed "):
                failures.append(line[7:])
            elif line == "done":
                progress.finish("Done")

    # The display is drawn in place, below the terminal's earlier output, so it moves the cursor
    # only relative to where it left it: `at` lines below the display's first.
    at = 0

    def to_top():
        return (f"\x1b[{at}A" if at else "") + "\r"

    def draw(frame, state, with_box):
        """The header, and the box too if `with_box`, as the box only changes with output. The
        first draw has the box, and makes room for it by scrolling the terminal as needed."""
        nonlocal at
        line1, line2 = header(progress, layout, frame, state)
        lines = [line1, line2]
        if with_box:
            lines += box_lines(rows, layout, args.title)
        text = "\x1b[?2026h\x1b[?25l" + to_top()
        text += "\r\n".join(f"{line}\x1b[K" for line in lines)
        if with_box:
            text += "\x1b[J"
        at = len(lines) - 1
        if state == "running" and not screen.cursor.hidden:
            y = layout.box_top + 1 + screen.cursor.y
            x = 2 + min(screen.cursor.x, layout.inner_cols - 1) + 1
            if y != at:
                text += f"\x1b[{abs(y - at)}{'B' if y > at else 'A'}"
            text += f"\x1b[{x}G\x1b[?25h"
            at = y
        write(text + "\x1b[?2026l")

    exit_code = 1
    try:
        tty.setraw(stdin)
        write("\r")
        frame, last_draw, last_tick = 0, 0.0, time.monotonic()
        wait_status = None
        box_changed = True
        inputs = [stdin, master]
        while wait_status is None:
            if resized:
                resized = False
                layout = Layout(*terminal_size())
                screen.resize(layout.inner_lines, layout.inner_cols)
                rows.clear()
                set_size(master, layout.inner_lines, layout.inner_cols)
                write(to_top() + "\x1b[J")
                at = 0
                box_changed = True
            try:
                ready, _, _ = select.select(inputs, [], [], 1 / FPS)
            except InterruptedError:
                continue
            if stdin in ready:
                data = os.read(stdin, 4096)
                if data:
                    try:
                        os.write(master, data)
                    except OSError:  # The command just exited.
                        pass
                else:  # The terminal is gone.
                    inputs.remove(stdin)
            if master in ready:
                try:
                    data = os.read(master, 65536)
                except OSError:
                    data = b""
                if data:
                    stream.feed(data)
                    box_changed = True
            done_pid, done_status = os.waitpid(pid, os.WNOHANG)
            if done_pid:
                # Its last output, but not what a process it left running writes later.
                while select.select([master], [], [], 0.05)[0]:
                    try:
                        data = os.read(master, 65536)
                    except OSError:
                        break
                    if not data:
                        break
                    stream.feed(data)
                wait_status = done_status
            read_status()
            now = time.monotonic()
            progress.animate(now - last_tick)
            last_tick = now
            if now - last_draw >= 1 / FPS:
                frame = int(now * 12)
                draw(frame, "running", box_changed)
                box_changed = False
                last_draw = now

        exit_code = os.waitstatus_to_exitcode(wait_status)
        read_status()
        if exit_code == 0:
            progress.finish("Done")
            state = "done"
        else:
            if exit_code < 0:  # Killed by a signal: report it as a shell would.
                why = "Stopped by Ctrl+C" if -exit_code == signal.SIGINT else "Killed"
                exit_code = 128 - exit_code
            else:
                why = f"Failed (exit status {exit_code})"
            progress.description = f"{why}: {progress.description}"
            state = "failed"
        progress.shown = progress.target
    finally:
        termios.tcsetattr(stdin, termios.TCSAFLUSH, saved)
        write("\x1b[?25h")
        for y in range(screen.used_lines()):
            log_row(screen.buffer[y], screen.columns)
        log.close()
        status.close()
        os.unlink(status_path)

    # Once done, clear the display and leave only how it went and where the output is. After a
    # failure that stopped it, leave the last view too, with the box down to its last line of
    # output, as that is where the error is.
    lines = []
    if state == "failed":
        used = screen.used_lines()
        line1, line2 = header(progress, layout, 0, state)
        lines = [line1, line2] + box_lines(rows, layout, args.title, used)
    if failures:
        lines.append(f" {RED}✗ Encountered errors:{RESET}")
        lines += [f"   - {failure}" for failure in failures]
    elif state == "done":
        lines.append(f" {GREEN}✓{RESET} {BOLD}Finished successfully{RESET}")
    log_path = args.log.replace(os.path.expanduser("~"), "~", 1)
    lines.append(f"Full output: {log_path}")
    write(to_top() + "\x1b[J" + "\r\n".join(lines) + "\r\n")
    sys.exit(exit_code)


if __name__ == "__main__":
    main()
