# Hibernation hang: the CS35L41 speaker amp deadlock

The hang that the debug tooling in this folder was built to find, on an ASUS ProArt PX13 (two
CS35L41 amps, root and swap file on btrfs). Kept for whoever debugs the next one: what was found,
how it showed, and the wrong turns.

## Summary

About one hibernation in twenty hung after writing its image: screen black, power LED on, drawing
power until forced off. The image stayed valid, so the next boot resumed.

Cause: a deadlock in the driver for the Cirrus Logic CS35L41 speaker amplifiers,
`snd_hda_scodec_cs35l41`. It reloads the amps' DSP firmware at the thaw right after the image is
created. If a firmware file isn't cached, the read can't complete, because btrfs's worker threads
are frozen by then. The power-off's first step then waits forever on the lock the reload holds.

Workaround: `scripts/sleep-hooks/cs35l41-hibernate`, installed by `scripts/configure_hardware`.
It turns the amps' firmware off before a hibernation and back on after the resume, so the thaw has
nothing to load.

## The crash log

The hung kernel's log, from the software watchdog crashing it. The hibernation gets as far as
`PM: Image saving done` and `printk: Suspending console(s)`, then nothing until `softdog:
Initiating panic`. The stuck tasks:

- `systemd-sleep` (the hibernating task), state D:
  `__mutex_lock` ← `cs35l41_system_suspend_prep` ← `device_prepare` ← `dpm_prepare` ←
  `dpm_suspend_start` ← `hibernation_platform_enter` ← `hibernate` ← `state_store`. The
  power-off's prepare step, waiting on the amp driver's `fw_mutex`.
- A `kworker` running `cs35l41_fw_load_work`, state D:
  `bit_wait_io` ← `read_extent_buffer_pages` ← `btrfs_read_extent_buffer` ← `btrfs_search_slot` ←
  `btrfs_lookup_dir_item` ← ... ← `kernel_read_file_from_path_initns` ← `_request_firmware` ←
  `cs35l41_request_firmware_file` ← `cs35l41_init_dsp` ← `cs35l41_fw_load_work`. A btrfs metadata
  read (a directory lookup on the firmware path) that never completes.
- The other amp's `cs35l41_fw_load_work`, state D: `d_alloc_parallel` in the same lookup, waiting
  for the first.
- Every CPU idle: a deadlock, not a spin.

## The mechanism

In `sound/hda/codecs/side-codecs/cs35l41_hda.c`:

- `cs35l41_hda_pm_ops` uses `SYSTEM_SLEEP_PM_OPS(cs35l41_system_suspend, cs35l41_system_resume)`,
  which uses the resume callback for thaw and restore too, with `.prepare =
  cs35l41_system_suspend_prep`.
- `cs35l41_system_resume` ends by scheduling `fw_load_work` if the firmware is wanted. So it runs
  at every resume, and at the thaw after the hibernation image is created.
- `cs35l41_fw_load_work` takes `fw_mutex` and loads the firmware with `request_firmware` variants,
  trying several file names.
- `cs35l41_system_suspend_prep` takes `fw_mutex` too. It runs as the first step of the power-off
  after the image is written.
- Between the freeze and the end of the hibernation, freezable workqueues are frozen, btrfs's read
  completion among them. (The image itself goes to the swap file by direct block I/O, which doesn't
  need them.) A firmware read that misses the page cache, or a lookup that misses the dentry cache,
  waits on those workers forever, holding `fw_mutex`.

Why it is rare: the amps reload their firmware at every resume, so the files and lookups are
usually cached. Making room for the image reclaims page cache and slab, and when memory is full it
can evict them.

Why no pm_trace points at it: `device_prepare` isn't traced. The last trace written is the end of
the thaw on the newest device, which reads as "a resume pass ended"; the hang is somewhere after
it.

The proper fix belongs upstream: don't load firmware at the thaw (separate `.freeze`, `.thaw`,
`.poweroff` and `.restore` callbacks instead of `SYSTEM_SLEEP_PM_OPS`), or don't hold `fw_mutex`
across a filesystem read that the prepare step can end up waiting for.

## The workaround

`scripts/sleep-hooks/cs35l41-hibernate`:

- Before a hibernation, it sets each amp's `DSP1 Firmware Load` ALSA control off. The driver then
  unloads the firmware, which reads no file, and schedules nothing at the thaw or the restore.
- After it, it sets them back on, and the firmware reloads in normal context.
- The driver refuses with EBUSY while a load or unload runs, or during playback, so the hook
  retries for 10 s. If it still can't, it logs "this hibernation can hang" to `journalctl -t
  cs35l41-hibernate`.
- The speakers have no DSP from just before the hibernation until some seconds after the resume.

Verified by dropping the caches right before hibernating, so that the thaw's reload must read the
disk: `sync; echo 3 | sudo tee /proc/sys/vm/drop_caches; systemctl hibernate`. Without the hook it
hung on the first try, with the same stacks. With the hook it powered off normally, and the amps'
`Firmware Loaded` lines came only after the resume.

## Wrong turns

- **NVIDIA.** The obvious suspect, and its driver's shutdown is known to hang in some states. But
  the card was awake at every hibernation, hangs included, because `nvidia-hibernate.service` wakes
  it to save its memory, and no crash stack involved it.
- **A USB controller in D3cold** at one hang's start: other hangs had it in D0, and good runs had
  it in D3cold.
- **"A resume trace means the S4 entry failed and the fallback power-off hung."** The thaw after
  the image is created is also a resume pass, and that is what the trace showed. It did lead to a
  real, separate finding: a plain power-off stops all watchdogs first, hence
  `watchdog.stop_on_reboot=0` (see the README).

## Reading a new hang

1. The alert window says which kind: a crash log, the chipset watchdog, or a sleep that never came
   back.
2. Crash log: read the end. First the panic reason (softdog: a hang with the kernel alive; a soft
   or hard lockup: a CPU stuck), then the `task:` stacks in state D. The hibernating task is
   `systemd-sleep`: its stack says which PM step it is in, and which driver's callback. Then look
   among the other D tasks for what that callback waits on, and back through the log for the PM
   phase lines (`PM: ... complete`, `Image saving done`, errors).
3. Trace: decode it as the README says.
4. If the stuck prepare is `cs35l41_system_suspend_prep` again, check `journalctl -t
   cs35l41-hibernate` for a failed toggle (sound playing?).

Still open: whether other drivers reload firmware from disk at the thaw (Wi-Fi, amdgpu). The
drop-caches test above would show any.
