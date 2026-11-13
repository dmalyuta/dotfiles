# Sleep and hibernation debug log

Finds the cause the first time a sleep or a hibernation goes wrong on the laptop:

- a hibernation that hangs at power-off (screen black, power LED on) and drains the battery,
- a sleep that doesn't reach the SoC's deepest state, or keeps waking up,
- a sleep that never comes back.

`setup on [trace]` installs it, `setup off` removes it (start_fresh asks). Read it with
`journalctl -t sleep-debug` and the files in `/var/log/sleep-debug`.
[CS35L41_hang_summary.md](CS35L41_hang_summary.md) is the write-up of the hang it was built to find.

| Part | What it is |
| --- | --- |
| `sleep-hooks/sleep-debug` | The log itself: a systemd sleep hook, also run at boot by `sleep-debug.service`. |
| `systemd/sleep-debug-wake.service` | Disarms the hibernation watchdog after each wake, in case the hook didn't run. |
| `dracut/hibernate-trace/` | Initramfs module: saves a pm_trace before the resume discards it, powers off after a watchdog fired, and notes each resume it starts. |
| `bin/sleep-debug-alert` | Shows each problem found in a window in the desktop session, once. |
| `bin/hibernate-loop` | Hibernates and wakes in a loop, to reproduce a hang. |
| `bin/pm-trace-decode` | Decodes a pm_trace long after the hang. |

## What it logs

- **Before each step**, a snapshot of the devices' power state (`<time>-<step>.txt`). Only sysfs and
  procfs reads, which don't wake a powered-off device (lspci and lsusb can), and none of a driver's
  own files: `/proc/driver/nvidia` blocks from nvidia-suspend/-hibernate.service until the resume.
  The hook waits for the snapshot 10 s at most, since systemd sleeps without a hook that is still
  running after a timeout.
- **After each step**, the battery used, and for a suspend, the share of it in hardware sleep (the
  SoC's deepest state) and the wakeup sources that fired. A suspend under 90% in hardware sleep is
  flagged, with the AMD PMC's debug state in `<time>-pmc.txt`. A hibernation is flagged as probably
  hung if it drew over 1 W on average for 20 min or more, or lost 5% or more (a working one is off).
- **For every sleep**, the kernel's debug messages (`pm_debug_messages`: the wakeup source, the AMD
  PMC's view of what kept the SoC awake), in the journal. Not `pm_print_times`: two lines per device
  and step overflow the 256 KB kernel log while journald is frozen, and push out the other messages.
- **The slow devices of every sleep**: each driver callback that took 0.3 s or more to put its
  device to sleep or wake it, from the kernel's power tracepoints (`slow devices:` in the journal).
  For a hibernation, that covers the freeze before the image and the restore at the resume, not
  the power-off in between, which no log survives.
- **A marker while asleep.** At boot, a leftover one means the last boot ended in a sleep it never
  came back from (a hang, or the battery died).
- **Each resume from hibernation**, noted by the initramfs in an EFI variable just before it: the
  time, and the kernel that loads the image (the boot loader's default, which after a kernel update
  isn't the hibernated one). After a good resume it goes to the journal. After a boot that ended in
  a hibernation, it tells a resume that hung (the laptop had powered off, and hung loading the image
  or waking the devices, as on 2026-11-12) from one that never powered off. A hung resume leaves no
  log by itself, but the image's softdog is still armed: left alone for 6 min, it crashes the
  resumed kernel if its timers run, and the next boot saves that log.
- **Every problem found** is also an alert: a file in `/var/log/sleep-debug/alerts`, which
  `sleep-debug-alert` shows in a window, so that a hang at night isn't missed when the session
  resumes in the morning.

## The hibernation watchdogs

During each hibernation, armed just before and disarmed first thing after the resume:

- **softdog, 5 min.** If the power-off hangs, it crashes the kernel, which saves its log to EFI
  variables (pstore, with `efi_pstore.pstore_disable=0` on the kernel command line) and reboots.
  The initramfs then sees that crash log next to a waiting image and powers off instead of resuming,
  so the battery isn't drained by a session resumed with nobody there. The next power-on resumes
  it, and the hook moves the log to `<time>-crash.txt`: the end of the kernel's log, then the stacks
  of the tasks stuck waiting. A CPU stuck in the kernel crashes it at once instead (a soft or hard
  lockup), with that CPU's stack.
- **The chipset's watchdog (sp5100_tco), 10 min**, as the backstop: it resets the laptop even when
  the CPU is stuck in the firmware or with interrupts off, where nothing in the kernel runs. No
  crash log then, but the image and the pm_trace are kept. Its driver tells at the next boot that
  it fired, which the initramfs saves to an EFI variable (as the resume discards that kernel)
  before powering off. It isn't set to power off by itself: that clears what the driver reads
  (tested 2026-10-30), so it would go unnoticed.

Both keep running if the hibernation falls back to a plain power-off (`kernel_power_off`), which by
default stops every watchdog before shutting down the drivers, where the 2026-10-30 hang was: the
kernel command line option `watchdog.stop_on_reboot=0` keeps them. With the kernel keeping them
through a reboot, systemd's own reboot watchdog is turned off (`RebootWatchdogSec=off`), so that it
isn't left running into another operating system.

While the log is on, the laptop also hibernates on AC: a hibernation that hangs at power-off
then doesn't drain the battery flat, which would reset the RTC and lose the pm_trace in it.

## pm_trace

Only if turned on (`setup trace on`), during each hibernation: before each device's callback, the
kernel writes a hash of the device and of the callback's file:line into the RTC, which survives a
forced power-off, and handles the devices one at a time so that the last one traced is the one that
hung. The next boot's kernel decodes it ("Magic number", "hash matches"), but resuming the image
discards that kernel's log, so the initramfs saves it to an EFI variable first, and the hook moves
it to `<time>-trace.txt`.

Decode it with `bin/pm-trace-decode`, from the "RTC time" line in the trace file, the time from the
hang to that boot (from the journal), and the device names in the hibernation's snapshot. Not with
the kernel's own decode: the RTC keeps ticking, so it is only right if the laptop was powered on
within about 3 minutes, and it is wrong whenever the year written is 00.

It is off unless turned on because it sets the RTC to a made-up date, so after it the kernel
ignores the RTC and the clock resumes behind by the time spent hibernated. The hook writes that
time back to the RTC and restarts chronyd, which steps the clock once online. Offline, the clock
stays behind (by 27 h after a 2026-11-06 hibernation). The crash log found the 2026-10-31 hang's
cause without it, so it only adds something for a hang no crash log covers (the chipset's watchdog
fired). Only the hibernation is traced: during a suspend, the RTC also holds the alarm that wakes
the laptop to hibernate, which must stay right.

### Reading a trace

After a hung hibernation, against those of working ones. The image is written, then the kernel
powers off through ACPI S4 (`hibernation_platform_enter`, kernel/power/hibernate.c).

- A working one (kernel 7.2.12, 2026-10-28) leaves the end of `device_suspend_noirq`
  (drivers/base/power/main.c:1560, `TRACE_SUSPEND(error)`) on device "faux", the last one powered
  off.
- The start of a suspend callback (`device_suspend*`, its `TRACE_SUSPEND(0)`) of some device: that
  device's driver hung powering off.
- The same as a working one: all devices were off, and the firmware's S4 entry (ACPI `_PTS` or the
  S4 write) hung or returned, after which the kernel spins forever.
- The end of a resume pass (main.c:1186 `TRACE_RESUME(error)`, on the newest device): the thaw
  after the image was created, so it hung after it, writing the image or in the power-off's prepare
  step (`device_prepare`, untraced). The 2026-10-30 and 2026-10-31 hangs: the speaker amps' driver
  deadlocked there (scripts/sleep-hooks/cs35l41-hibernate), not, as first read, a failed S4 entry.
- Another resume callback: the S4 entry failed and the kernel restored the devices, then fell back
  to a plain power-off (`kernel_power_off`), which can hang in a driver's shutdown (untraced).
