# Danylo's dotfiles

CachyOS (KDE Plasma) setup after a fresh install.

## Fresh machine

On a brand new install, run:
```bash
curl -fsSL https://raw.githubusercontent.com/dmalyuta/dotfiles/cachyos/start_fresh.sh | bash
```

This clones this repo to `~/sw/dotfiles`, symlinks the dotfiles into the home
directory, and installs the software. It is interactive and will ask if you want
to install OpenRGB, the printer driver and so on.

It is also safe to re-run (aka *idempotent*): every step checks for what it
installs and skips it if it is already there. So you can always re-run using:
```bash
~/sw/dotfiles/start_fresh.sh
```

## License

The code is available under the [MIT license](
https://github.com/dmalyuta/dotfiles/blob/master/LICENSE).
