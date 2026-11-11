# Danylo's dotfiles

Fedora KDE Plasma setup after a fresh install.

## Fresh machine

On a brand new install, run:

```bash
curl -fsSL https://raw.githubusercontent.com/dmalyuta/dotfiles/fedora/start_fresh | bash
```

This clones this repo to `~/Projects/dotfiles`, symlinks the dotfiles into the
home directory, and installs the software, including the NVIDIA driver. It is
interactive and will ask if you want to install OpenRGB, the printer driver and
so on.

With Secure Boot on, it asks for a one-time password. On the next boot, a blue
*MOK Manager* screen waits 10 seconds for a key press: press any key, pick
*Enroll MOK*, enter that password and reboot. This lets the NVIDIA driver load.
Hibernation needs Secure Boot off, since Fedora's kernel turns it off otherwise.

It is also safe to re-run (aka *idempotent*): every step checks for what it
installs and skips it if it is already there. So you can always re-run using:

```bash
~/Projects/dotfiles/start_fresh
```

To re-run with the last run's answers to its questions:

```bash
~/Projects/dotfiles/start_fresh -c
```

The answers are kept in `.start_fresh_answers`, which git ignores, so that what is private stays
out of this repo, like the paths of your projects. What is specific to a machine isn't asked but
found on it: the printer on the network, the webcam's microphone, which USB devices are mice.

## Layout

- `start_fresh`: sets up a machine, see above, `start_fresh -h` and `man start_fresh`.
- `man/`: its manual page, linked into `~/.local/share/man`.
- `home/`, `config/`: dotfiles, symlinked into `~` and `~/.config`.
- `bin/`: commands, on `PATH` as `~/.bin`.
- `scripts/`: system setup, run by `start_fresh`.
  - `configure_hardware`, `configure_kde`: the setup steps; `functions.sh`, their shared helpers;
    `tui.py`, the progress display that `start_fresh` runs them under.
  - What they install or run, one folder per kind: `bin/` (commands, most installed to
    `/usr/local/bin`), `sleep-hooks/` (to `/usr/lib/systemd/system-sleep`), `systemd/` (units),
    `udev/` (rules), `hid-bpf/` (HID-BPF device fixes, built on the machine), `keyd/`,
    `patches/` (patched builds of Fedora's libinput, KWin, Plasma and KIdleTime, built on the
    machine), `wine/` (and of Wine's COM library), `touchpad/` (tools to record and replay what
    the touchpad does).
  - `sleep-debug/`: the hibernation debug log, with its own `README.md` and `setup`, the same
    folders and a dracut module in `dracut/`.

Scripts you run have no extension; only files that are sourced end in `.sh` (and dracut's
`module-setup.sh`, whose name dracut sets).

## License

The code is available under the [MIT license](LICENSE).
