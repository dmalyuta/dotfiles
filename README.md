# Danylo's dotfiles

Ubuntu 26.04 LTS[^1] setup after a fresh install.

## Fresh machine

On a machine with nothing set up yet, run
```
wget -qO- https://raw.githubusercontent.com/dmalyuta/dotfiles/master/start_fresh.sh | bash
```

> ℹ️ Note: if you prefer to user `curl`, run:
> ```
> sudo apt install -y curl
> curl -fsSL https://raw.githubusercontent.com/dmalyuta/dotfiles/master/start_fresh.sh | bash
> ```

This clones this repo to `~/sw/dotfiles`, symlinks the dotfiles into the home
directory, and installs the software. It is interactive and will ask if you want
to install MATLAB, OpenRGB, the printer driver and so on.

It is also safe to re-run (aka *idempotent*): every step checks for what it installs and skips it
if it is already there. From an existing clone, you can run it directly instead:
```
~/sw/dotfiles/start_fresh.sh
```

## License

The code is available under the [MIT license](
https://github.com/dmalyuta/dotfiles/blob/master/LICENSE).

---

[^1]: Standard version that comes with the Gnome desktop.
