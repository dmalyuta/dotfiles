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

Do re-run using the more recent run's answers to user questions:

```bash
~/Projects/dotfiles/start_fresh -c
```

## Layout

- `start_fresh`: sets up a machine, see above.
- `home/`, `config/`: dotfiles, symlinked into `~` and `~/.config`.
- `bin/`: commands, on `PATH` as `~/.bin`.
- `scripts/`: system setup, run by `start_fresh`.
  - `configure_hardware`, `configure_kde`: the setup steps; `functions.sh`, their shared helpers.
  - What they install or run, one folder per kind: `bin/` (commands, most installed to
    `/usr/local/bin`), `sleep-hooks/` (to `/usr/lib/systemd/system-sleep`), `systemd/` (units),
    `udev/` (rules), `keyd/`.
  - `sleep-debug/`: the hibernation debug log, with its own `setup`, the same folders and a dracut
    module in `dracut/`.

Scripts you run have no extension; only files that are sourced end in `.sh` (and dracut's
`module-setup.sh`, whose name dracut sets).

## License

The code is available under the [MIT license](https://github.com/dmalyuta/dotfiles/blob/master/LICENSE).
