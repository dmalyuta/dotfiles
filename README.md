# Danylo's dotfiles

Ubuntu 26.04 LTS setup after a fresh install.

## Fresh machine

On a machine with nothing set up yet, run
```
curl -fsSL https://raw.githubusercontent.com/dmalyuta/dotfiles/master/start_fresh.sh | bash
```
This clones this repo to `~/sw/dotfiles`, symlinks the dotfiles into the home
directory, and installs the software. It is interactive (it asks about MATLAB,
OpenRGB, the printer driver and so on), and it needs `bash`, not `sh`.

It is also safe to re-run: every step checks for what it installs and skips it
if it is already there. From an existing clone, run it directly instead of
curling it:
```
~/sw/dotfiles/start_fresh.sh
```

## License

The code is available under the [MIT license](
https://github.com/dmalyuta/dotfiles/blob/master/LICENSE).
