# Hibernation hang on the ProArt PX13: the CS35L41 speaker amp deadlock

Found 2026-10-31 from the first crash log of a hung hibernation, after a week of hangs. This file
is for whoever debugs the next hang on this laptop, this one or another: what was found, the
evidence, the wrong turns, and how the debug tooling in this folder works and misleads.

## Summary

Now and then a hibernation hung after writing its image: screen black, power LED on, ~13 W, until
forced off (one drained the battery flat overnight). The image stayed valid, so the next boot
resumed.

Cause: a deadlock in the driver for the Cirrus Logic CS35L41 speaker amplifiers,
`snd_hda_scodec_cs35l41`. It reloads the amps' DSP firmware at the thaw right after the image is
created. If a firmware file isn't cached, the read can't complete, because btrfs's worker threads
are frozen by then. The power-off's first step then waits forever on the lock the reload holds.

Workaround: `scripts/cs35l41-hibernate`, a systemd-sleep hook installed by
`scripts/configure_hardware.sh`. It turns the amps' firmware off before a hibernation and back on
after the resume, so the thaw has nothing to load.

Not NVIDIA, though that was the first suspect.

Reported upstream 2026-10-31 by email to the Cirrus maintainers and linux-sound (the draft:
`~/Downloads/cirrus_bug_email.txt`). The evidence is kept in `~/Documents/cs35l41-hibernation-bug/`,
since the hook prunes snapshots after 300 and the journal is capped at 256 MB:
- the crash logs and traces of the 10-31 hang, test 1 and test 2;
- the 10-30 hang's snapshot and trace;
- journal excerpts around both hangs;
- `sleep-stats.csv`, this file, the hook and the email.

## The machine

- ASUS ProArt PX13 (board HN7306WU, BIOS HN7306WU.318 05/04/2026), AMD APU with its iGPU, NVIDIA
  RTX 4050 (driver 615.78.08, `NVreg_UseKernelSuspendNotifiers=0`), managed by cardwire in Hybrid
  mode.
- Fedora 44, kernel 7.2.13-200.fc44, root and swap file on btrfs, SELinux enforcing.
- Dual boot with Windows: the RTC is in local time.
- Speakers: two CS35L41 amps (`CSC3551:00-cs35l41-hda.0` and `.1`, ALSA card 2 "Generic_1",
  controls `L0 DSP1 Firmware Load` numid 2 and `R0 DSP1 Firmware Load` numid 5). Firmware files are
  in `/usr/lib/firmware/cirrus/` (package `cirrus-audio-firmware`):
  `cs35l41-dsp1-spk-prot-10431ed3*`.
- Sleep is suspend-then-hibernate: `HibernateDelaySec=1h`, `HibernateMode` platform (S4), also on AC
  while the sleep-debug log is on. See `scripts/configure_hardware.sh`.

## The hangs

| When (local) | Context | Evidence | Reading |
|---|---|---|---|
| 2026-10-27 23:51 | battery | none (before the tooling) | unknown |
| 2026-10-29 12:24 | battery, suspend-then-hibernate from the Plasma menu | battery died, RTC reset to the BIOS date, so the trace was lost | unknown, likely the same |
| 2026-10-30 10:10 | battery, suspend-then-hibernate | pm_trace: `main.c:1186 TRACE_RESUME(error)` on the newest device (hash 273: lo/tty10/usb1-port1), elapsed 13m03s; forced off after ~14 min at 12.9 W | first misread as a failed S4 entry (see Wrong turns); it is the same signature as 10-31 |
| 2026-10-31 11:16:46 | battery, `systemctl hibernate` 44 s after `start_fresh.sh` upgraded PipeWire with dnf | crash log + pm_trace `main.c:1186 TRACE_RESUME(error)` (devices wakeup33/watchdog1, the newest), elapsed 5m15s | the deadlock, below |

Hang rate: about 1 in 20 hibernations (2 of 38 logged by `/var/log/sleep-stats.csv` before 10-31).
Both logged hangs were on battery, versus 0 of 19 on AC: suggestive but not significant (p ≈ 0.24).
About 40 good hibernations were logged, ~15 of them by `hibernate-loop` on AC.

## The crash log (2026-10-31)

Saved as `/var/log/sleep-debug/20261031-114607-crash.txt` (91,641 bytes, 55 efi-pstore parts). It is
the hung kernel's log, from the software watchdog (`softdog`, 300 s) crashing it. The last
hibernation in it (uptime in seconds):

```
[ 8713.793] PM: hibernation: hibernation entry
[ 8717.062] PM: hibernation: Allocated 3097473 pages for snapshot
            Allocated 12389892 kbytes in 3.18 seconds (3896.19 MB/s)     <- ~1.3 s in earlier ones
[ 8722.572] PM: hibernation: Need to copy 3512658 pages
[ 8723.263] PM: thaw of devices complete after 802.387 msecs              <- the thaw: amps reload
[ 8723.267] PM: hibernation: Writing hibernation image.
[ 8734.387] PM: Image saving done                                         <- 4.4 GB compressed
[ 8734.736] printk: Suspending console(s)
[ 8791.69x] GPIO 8 is active: 0x10141b00   (x6, pinctrl-amd wake IRQ debug, a minute later)
[ 9012.280] softdog: Initiating panic                                     <- 300 s after arming
```

The stuck tasks (from `panic_sys_info=blocked_tasks`):

- `systemd-sleep` (the hibernating task), state D:
  `__mutex_lock` ← `cs35l41_system_suspend_prep` ← `device_prepare` ← `dpm_prepare` ←
  `dpm_suspend_start` ← `hibernation_platform_enter` ← `hibernate` ← `state_store` (the write to
  `/sys/power/state`). The power-off's prepare step, waiting on the amp driver's `fw_mutex`.
- `kworker/0:2` (events: `cs35l41_fw_load_work`), state D:
  `bit_wait_io` ← `read_extent_buffer_pages` ← `btrfs_read_extent_buffer` ← `btrfs_search_slot` ←
  `btrfs_lookup_dir_item` ← `btrfs_lookup` ← `lookup_slow` ← `path_openat` ←
  `kernel_read_file_from_path_initns` ← `fw_get_filesystem_firmware` ← `_request_firmware` ←
  `firmware_request_nowarn` ← `cs35l41_request_firmware_file` ← `cs35l41_init_dsp` ←
  `cs35l41_smart_amp` ← `cs35l41_fw_load_work`. A btrfs metadata read (a directory lookup on the
  firmware path) that never completes.
- `kworker/0:0` (the other amp's `cs35l41_fw_load_work`), state D: `d_alloc_parallel` in the same
  lookup, waiting for the first.
- Every CPU idle (`swapper` in `cpuidle_enter_state`): a deadlock, not a spin.

## The mechanism

Kernel 7.2.13, `sound/hda/codecs/side-codecs/cs35l41_hda.c` (it was `sound/pci/hda/` before 6.17):

- `cs35l41_hda_pm_ops` (line 2125): `.prepare = cs35l41_system_suspend_prep` and
  `SYSTEM_SLEEP_PM_OPS(cs35l41_system_suspend, cs35l41_system_resume)`. That macro uses the resume
  callback for thaw and restore too.
- `cs35l41_system_resume` (line 1010) ends with: if `request_fw_load` and no load in progress,
  `schedule_work(&cs35l41->fw_load_work)` (lines 1049–1051). This runs at every resume, and at the
  thaw after the hibernation image is created.
- `cs35l41_fw_load_work` (line 1254) takes `fw_mutex` (line 1260) and loads the firmware with
  `request_firmware` variants (several candidate names, so lookups of names that don't exist too).
- `cs35l41_system_suspend_prep` (line 929) takes `fw_mutex` (line 940). It runs as the first step of
  the power-off after the image is written (`hibernation_platform_enter` → `dpm_suspend_start` →
  `dpm_prepare`).
- Between the freeze and the end of the hibernation, freezable tasks and workqueues are frozen.
  btrfs's workqueues (read completion among them) are `WQ_FREEZABLE`. The image itself is written to
  the swap file by direct block I/O, which doesn't need them. A firmware read that misses the page
  cache, or a lookup that misses the dentry cache, waits on frozen btrfs workers forever. It holds
  `fw_mutex` while it waits, so the prepare step waits forever too.

Why it is rare: the amps reload their firmware at every resume (`Firmware Loaded - Type: spk-prot`
in the kernel log), so the files and lookups are usually cached. Setting aside memory for the image
(`hibernate_preallocate_memory`) reclaims page cache and slab. When memory is full, it can evict
them. On 10-31 that took 3.18 s versus ~1.3 s for the hibernations just before, right after a dnf
transaction.

Why no pm_trace points at it: `device_prepare` has no `TRACE_DEVICE`/`TRACE_SUSPEND`. The last trace
written is the end of the thaw (`device_resume`'s `TRACE_RESUME(error)`, main.c:1186) on the last
device resumed, which is the newest device. So the trace reads as "a resume pass ended" and the hang
is somewhere after it: the image write or the prepare step.

## The workaround

`scripts/cs35l41-hibernate` (systemd-sleep hook, `/usr/lib/systemd/system-sleep/cs35l41-hibernate`):

- `pre` of a hibernate or hybrid-sleep step (`SYSTEMD_SLEEP_ACTION`, so the hibernate step of
  suspend-then-hibernate): for each `... DSP1 Firmware Load` ALSA control that is on, set it off and
  remember it in `/run/cs35l41-hibernate`. The driver's put handler sets `request_fw_load = 0` and
  queues an unload, which reads no file. With `request_fw_load` off, `cs35l41_system_resume`
  schedules nothing at the thaw or the restore.
- `post` (any step): set the remembered controls back on. The firmware reloads in normal context,
  in about 10–20 s.
- The put handler returns EBUSY while a load or unload runs, or during playback. The hook retries
  for 10 s. If `pre` still fails (sound playing throughout), it logs to `journalctl -t
  cs35l41-hibernate` "this hibernation can hang".
- Side effect: the speakers have no DSP from just before the hibernation until ~20 s after the
  resume.

Proper fix, upstream: don't load firmware at the thaw, since the image is about to be written and
the machine powered off. Use separate `.freeze`/`.thaw`/`.poweroff`/`.restore` callbacks instead of
`SYSTEM_SLEEP_PM_OPS`. Alternatively, flush or cancel `fw_load_work` without holding `fw_mutex`
before taking it in prepare, or don't depend on the filesystem there. Not reported upstream as of
2026-10-31. No match found on LKML or alsa-devel; the closest are CVE-2026-64481 (fw_load_work
teardown on unbind) and the 2022 suspend/hibernate series. Cirrus maintainers:
patches@opensource.cirrus.com, list alsa-devel / linux-sound.

## Verification

- Done: the hook's pre and post toggle both amps (tested by hand 2026-10-31); `Firmware Loaded`
  follows each re-enable.
- Proof test, proposed 2026-10-31: drop the caches right before hibernating, so that the thaw's
  reload must read the disk.
  `sync; echo 3 | sudo tee /proc/sys/vm/drop_caches; systemctl hibernate`
  Expected: without the hook (`chmod -x` on it), a hang, recovered by the watchdog. With the hook, a
  normal hibernation.
  Result, test 1 (hook off, 2026-10-31 11:53): hung, reproduced on the first try. The crash log is
  `/var/log/sleep-debug/20261031-115333-crash.txt`. It shows the same stacks:
  - `systemd-sleep` in `cs35l41_system_suspend_prep`;
  - both `cs35l41_fw_load_work` workers in `btrfs_lookup_dir_item` under `_request_firmware`;
  - and also `btrfs-transaction` stuck in `btrfs_read_extent_buffer`, so no btrfs read completes
    at that point.

  Test 2 (hook on, same command, 2026-10-31 12:03): powered off normally in ~45 s, resumed, no
  alert. The trace was the good one: magic 4:659:526, the same as other good runs, which
  `pm-trace-decode` gives as main.c:1560 on `faux`. The amps' `Firmware Loaded` lines come only
  after `PM: hibernation: hibernation exit`, from the hook's post, with none at the thaw.

  Conclusion: the deadlock is confirmed, and the hook prevents it, even with the caches dropped.

## Wrong turns (don't repeat)

- **NVIDIA.** The obvious suspect, and its driver's shutdown is known to hang in some states (see
  `configure_hardware.sh`). But the card was awake (`active D0`) at every hibernation, hangs
  included, because `nvidia-hibernate.service` wakes it to save its memory. No crash stack involved
  it.
- **USB controller c5:00.4 in D3cold** at hibernation entry (10-29 hang): ruled out. The 10-30 hang
  had it in D0, and good runs had it in D3cold.
- **"A resume trace = the S4 entry failed and the fallback `kernel_power_off` hung"** (the 10-30
  reading, from the trace plus timing). The thaw after the image is created is also a resume pass,
  and that is what the trace showed. The timing (last trace write ~53 s after entry, versus 56–73 s
  for good runs) fits stopping right after the image write. It also led to a real but separate
  finding: `kernel_power_off` stops all watchdogs first (`kernel/reboot.c:303–310`, the reboot
  notifiers, `drivers/watchdog/watchdog_core.c:159–177`). So `watchdog.stop_on_reboot=0` is now
  set (see below).

## The debug tooling in this folder, and its traps

Everything below is installed by `setup.sh on` (and removed by `setup.sh off`, keeping the logs).

- **`sleep-debug`**, the systemd-sleep hook (plus `sleep-debug.service` at boot and
  `sleep-debug-wake.service` after each wake). It writes:
  - `/var/log/sleep-debug/<time>-<step>.txt`: snapshots (battery, devices' power states, wakeup
    sources, modules, kernel log tail, and for a hibernation the `## devices` name list for
    decoding).
  - `<time>-trace.txt`: the pm_trace.
  - `<time>-crash.txt`: crash logs.
  - The journal: `journalctl -t sleep-debug`, with drain, hardware-sleep share and wake sources.
  - `/var/log/sleep-stats.csv`, from `scripts/sleep-stats`: one row per step.
- **Watchdogs during each hibernation only**, armed in pre and disarmed in post:
  - softdog at 300 s with `soft_panic=1`, plus `softlockup_panic`, `hardlockup_panic`,
    `panic_sys_info=blocked_tasks` and `panic=5`. A hang with the kernel alive becomes a panic.
    efi-pstore saves the log (`efi_pstore.pstore_disable=0 pstore.kmsg_bytes=32768`; the size
    counts compressed bytes, ~95 KB of log) and the kernel reboots. The initramfs (`hibernate-trace/`)
    sees the crash next to the image and powers off. The next power-on resumes, and the hook saves
    the log.
  - The chipset watchdog `sp5100_tco` at 600 s with `action=0` (reset): the backstop for hangs with
    nothing in the kernel running. It is reset, not power-off, because a power-off clears the chip's
    "fired" flag (`bootstatus` 32). After a reset the initramfs reads the flag, notes it
    (`HibernateTcoFired-…` EFI variable) and powers off.
  - `watchdog.stop_on_reboot=0`, so `kernel_power_off` doesn't stop them, and
    `RebootWatchdogSec=off`, so systemd doesn't leave the chipset watchdog running into Windows.
- **Alerts:** every problem found is also a file in `/var/log/sleep-debug/alerts/`, shown once in a
  window at login by `sleep-debug-alert` (a user service).
- **pm_trace** (hibernations only; the RTC holds the suspend's wake alarm): the kernel hashes each
  device and callback site into the RTC.
  - Decode with `pm-trace-decode --rtc '<RTC time line>' --elapsed MIN-MAX --main-c main.c --devices
    names.txt`, using `main.c` from the stable tree at the running kernel's tag
    (`https://git.kernel.org/pub/scm/linux/kernel/git/stable/linux.git/plain/drivers/base/power/main.c?h=v7.2.13`).
  - Never trust the kernel's own "Magic number"/"hash matches" decode. The RTC keeps ticking (right
    only within ~3 min), and year 2000 is mis-decoded.
  - Readings: a good hibernation leaves `main.c:1560` (end of `device_suspend_noirq`) on `faux`. A
    `TRACE_SUSPEND(0)` start on a device means that driver hung. `main.c:1186` on the newest device
    means after the thaw, as above.
  - The hook sets the clock back after the resume (`hwclock`, chronyd).
- **Traps:**
  - The journal lacks the hung kernel's last messages: journald is frozen during a hibernation.
    Only the crash log has them.
  - After a resume, pstorefs listed none of the crash's EFI variables (unknown why; it worked at a
    fresh boot). The hook now decodes the `dump-type0-<part>-<count>-<time>-C-cfc8fc79-…` variables
    itself: drop the 4-byte attribute header, then raw deflate (`zlib.decompress(data, -15)`); the
    highest part is the oldest. Read them with `cat`: `cp` fails on efivarfs ("Illegal seek").
    `sleep-debug collect` saves a leftover one by hand.
  - At a fresh boot, `systemd-pstore.service` moves crash logs to `/var/lib/systemd/pstore` first.
    The hook joins the new ones; `.pstore-seen` marks what it has seen.
  - Never read `/proc/driver/nvidia` from a sleep hook: it blocks while nvidia-suspend/-hibernate
    runs.
  - cardwire's Smart and Integrated modes hide the NVIDIA card from unapproved processes, the hook
    included. Its snapshot then lacks the card entirely.
  - `/tmp` is wiped at boot.
- **`hibernate-loop start|stop|status`**: repeated suspend-then-hibernate, waking by RTC alarm. AC
  only: on battery the alarm doesn't wake it from hibernation. It keeps pm_trace off, since that
  rewrites the RTC, and stops on any alert.

## Reading a new hang

1. The alert window says which kind: a crash log, the chipset watchdog, or a sleep that never came
   back.
2. Crash log: read the end. First the panic reason (softdog = a hang with the kernel alive; a soft
   or hard lockup = a CPU stuck), then the `task:` stacks in state D. The hibernating task is
   `systemd-sleep`; its stack says which PM step it is in, and which driver's callback. Then look
   for what that callback waits on, among the other D tasks. Look back through the log for the
   PM phase lines (`PM: ... complete`, `Image saving done`, errors like `failed to ...`).
3. Trace: decode it as above, with `--elapsed` from the journal's "hibernation entry" to the next
   boot, minus the time the watchdog took if it fired.
4. Compare with this hang: if the stuck prepare is `cs35l41_system_suspend_prep` again, check
   `journalctl -t cs35l41-hibernate` for a failed toggle (sound playing?).

## Open questions

- Whether the proof test hangs without the hook.
- Why pstorefs didn't list the efi-pstore records after the resume.
- The 10-27 and 10-29 hangs have no trace; they are probably the same.
- Whether other drivers reload firmware from disk at the thaw (mt7925 wifi, amdgpu, ...): a drop-caches
  test would show any.

## Turning the debug tooling off later

Once hibernation has run cleanly for a while (the fix was confirmed 2026-10-31), the debug tooling
can go. The amp fix, `scripts/cs35l41-hibernate`, is separate: `configure_hardware.sh` installs it,
and it stays.

1. Run `scripts/sleep-debug/setup.sh off`, as your user: it asks for sudo itself. It removes:
   - the sleep-debug hook and its boot and wake services;
   - the alert window's user units and `/usr/local/bin/sleep-debug-alert`;
   - the `hibernate-trace` initramfs module and its dracut config, and rebuilds the initramfs;
   - the sleep.conf drop-in, so it no longer hibernates on AC (back to `HibernateOnACPower=no`
     from `configure_hardware.sh`);
   - the systemd drop-in (`RebootWatchdogSec=off`) and the sp5100_tco settings (the driver is
     reloaded with its defaults);
   - the kernel options `efi_pstore.pstore_disable=0 pstore.kmsg_bytes=32768
     watchdog.stop_on_reboot=0` (with grubby);
   - the hook's EFI variables, and `pm_trace` and `pm_debug_messages` for the running kernel.

   It keeps the logs in `/var/log/sleep-debug` (snapshots, traces, crash logs, alerts) and
   `/var/log/sleep-stats.csv`, which `scripts/sleep-stats` keeps writing. It exits with status 2,
   meaning a reboot is needed.
2. Reboot. The kernel options and the systemd setting only change at boot. Until then, the kernel
   still keeps watchdogs running through a reboot.
3. Check after the reboot:
   - `cat /proc/cmdline`: no `efi_pstore`, `pstore.` or `watchdog.stop_on_reboot` options;
   - `ls /usr/lib/systemd/system-sleep/`: no `sleep-debug`, but `cs35l41-hibernate` still there;
   - `systemctl show -p RebootWatchdogUSec`: back to 10min, systemd's default.
4. `start_fresh.sh` asks about it on the laptop ("Turn on the sleep and hibernation debug log?").
   It defaults to the last answer, saved in `.start_fresh_answers` (git-ignored) as `sleep_debug`,
   and with `-c` it reuses that answer without asking. So answer `n` the next time, or it gets
   turned back on. Answering `n` also runs `setup.sh off` by itself.

Optional cleanups (none are needed):

- `sudo rm -rf /var/log/sleep-debug`: the logs, including the crash logs of this hang (keep
  `20261031-114607-crash.txt` and `20261031-115333-crash.txt` if an upstream report is still
  pending).
- `sudo rm -f /var/lib/systemd/pstore/dmesg-*`: the parts of the 2026-10-30 test crash, archived by
  systemd-pstore.
- `rm -rf ~/.local/state/sleep-debug-alert`: the alert window's list of what it showed.

To turn it all back on for a new hang: `scripts/sleep-debug/setup.sh on`, then reboot.
`hibernate-loop` and `pm-trace-decode` aren't installed anywhere; they run from this folder.
