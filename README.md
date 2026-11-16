# Danylo's dotfiles

Fedora KDE Plasma setup after a fresh install.

## Fresh machine

On a brand new install, run:

```bash
curl -fsSL https://raw.githubusercontent.com/dmalyuta/dotfiles/fedora/provision | bash
```

This clones this repo to `~/Projects/dotfiles`, symlinks the dotfiles into the
home directory, and installs the software, including the NVIDIA driver. It asks
its questions up front, like whether to install the printer driver, and then
runs unattended.

With Secure Boot on, it asks for a one-time password. On the next boot, a blue
*MOK Manager* screen waits 10 seconds for a key press: press any key, pick
*Enroll MOK*, enter that password and reboot. This lets the NVIDIA driver load.
Hibernation needs Secure Boot off, since Fedora's kernel turns it off otherwise.

It is also safe to re-run (aka *idempotent*): every step checks for what it
installs and skips it if it is already there. So you can always re-run it, from
anywhere (it is on `PATH`, like the other commands in `bin/`):

```bash
provision
```

Re-runs reuse the last run's answers to its questions, asking only new ones. To
answer them all again:

```bash
provision -u
```

The answers are kept in `.provision_answers`, which git ignores, so that what is private stays
out of this repo, like the paths of your projects. What is specific to a machine isn't asked but
found on it: the printer on the network, the webcam's microphone, which USB devices are mice.

## Layout

- `provision`: sets up a machine, see above, `provision -h` and `man provision`.
- `packages.toml`: what it installs with dnf and Flatpak, and for which machines. To install
  something else, add its name there and run `provision` again.
- `man/`: its manual page, linked into `~/.local/share/man`.
- `home/`, `config/`: dotfiles, symlinked into `~` and `~/.config`.
- `bin/`: commands, on `PATH` as `~/.bin`.
- `scripts/`: system setup, run by `provision`.
  - `configure_hardware`, `configure_kde`: the hardware and desktop setup. Like `provision`,
    each is just its steps in the order they run; the functions that do the work are in
    `lib/<script>/`, one file per topic.
  - `functions.sh`: the helpers they all share. `tui.py`: the progress display that `provision`
    runs under.
  - What they install or run, one folder per kind: `bin/` (commands, most installed to
    `/usr/local/bin`), `sleep-hooks/` (to `/usr/lib/systemd/system-sleep`), `systemd/` (units),
    `udev/` (rules), `yum/` (package repositories), `hid-bpf/` (HID-BPF device fixes, built on the
    machine), `dracut/` (initramfs modules), `grub/`, `keyd/`, `patches/` (patched builds of
    Fedora's libinput, KWin, Plasma and KIdleTime, built on the machine), `wine/` (and of Wine's
    COM library), `touchpad/` (tools to record and replay what the touchpad does).
  - `sleep-debug/`: the hibernation debug log, with its own `README.md` and `setup`, the same
    folders and a dracut module in `dracut/`.

Scripts you run have no extension; only files that are sourced end in `.sh` (and dracut's
`module-setup.sh`, whose name dracut sets).

## License

The code is available under the [MIT license](LICENSE).
