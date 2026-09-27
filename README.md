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

Do re-run using the more recent run's answers to user questions:

```bash
~/sw/dotfiles/start_fresh.sh -c
```

## Hotkeys

The script installs a keyboard-driven window management workflow using `KWin`
and `keyd`.

| Keys | Action |
| --- | --- |
| `Meta+B` / `Meta+Shift+B` | Brave: focus / new window |
| `Meta+W` / `Meta+Shift+W` | kitty: focus / new window |
| `Meta+E` / `Meta+Shift+E` | Dolphin: focus / new window |
| `Meta+S` | FSearch |
| `Meta+R` | PureRef |
| `Meta+O` | Obsidian |
| `Meta+I` | Inkscape |
| `Meta+N` | SpeedCrunch |
| `Meta+P` | PDF-XChange Editor |
| `Meta+C` | VS Code. Press again within a second to pick among its windows |
| `Meta+C`, `Meta+<key>` | VS Code on a workspace (keep Meta held): `D` to open dotfiles, plus any you specify |
| `Meta+Tab` / `Meta+Ctrl+Tab` | Window switcher / Overview |
| `Ctrl+Shift+I` | Invert screen colors |
| `Caps Lock`, `Print` | Flameshot capture |
| `Meta+/` | Context menu at the text cursor |

`Caps Lock` and `Meta+/` are remapped by [keyd](scripts/keyd.conf), below the
display server.

## License

The code is available under the [MIT license](https://github.com/dmalyuta/dotfiles/blob/master/LICENSE).
