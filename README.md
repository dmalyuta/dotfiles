# Danylo Malyuta's Dotfiles

## Description

A set of dotfiles and a setup script to hit the ground running on a vanilla Ubuntu-based Linux installation for C/C++ developers. The objective is to have a no-questions-asked, robust and fully automated way of taking you from a fresh install to a fully configured system on which you can immediately get to work.

![screenshot](http://i.imgur.com/N3oP2Mb.png)

## Features

* [Emacs](https://www.gnu.org/software/emacs/) configuration suitable for C/C++ and LaTeX
* [Powerline](https://github.com/powerline/powerline) for bash
* [i3](http://i3wm.org/) tiling window manager
* [Rofi](https://github.com/DaveDavenport/rofi) application launcher
* [Automated backup](https://github.com/danyloM/dotfiles/blob/master/.bin/make_snapshot.sh) using `rsync`, `cron` and `anacron` with support for grandfather-father-son backup scheme
* [Numix](https://github.com/numixproject/numix-icon-theme) theme and icon set
* [Google Chrome](https://www.google.com/chrome/) web browser
* [Foxit Reader](https://www.foxitsoftware.com/products/pdf-reader/) software for standards-compliant reading and annotation of PDFs

## Contributing

Pull requests are welcome!

If you are having problems, please submit an issue - I will respond right away!

## Installation

### Requirements

You should be running a Ubuntu-based Linux operating system. You can run the installation either on a completely fresh or an already used OS. You should also be connected to the Internet throughout the installation, since the install script downloads a few files. Note that the script is smart enough to backup any existing dotfiles to a directory `~/dotfile_backup_ddmmYYYY_THHMMSS/` (where the second half of the folder name is the current date and time) before overwriting any dotfile with one from this repository.

### Supported Distributions

- Linux Mint 18 MATE 64-bit
- Ubuntu 14.04.5 Desktop 64-bit

### Instructions

1. Clone this repository into any directory `DIR` you wish with `git clone https://github.com/danyloM/dotfiles DIR`;
2. Move into `DIR` with `cd DIR`;
3. To view a dry-run simulation of the installation (without actually doing anything), use `sudo .setup/setup_new_machine.sh -d`. Note that since folders aren't copied or moved in this simulation, you will see some "couldn't find folder" warnings that will not be present in the actual installation;
4. To run the installation, use `sudo .setup/setup_new_machine.sh`. The install script will prompt you for what specific programs and to install and will give you instructions when manual work is needed (such as during the Foxit PDF Reader installation, if you choose to install it). The aim is to safely deploy this repository partially or fully to your computer. You are recommended to reboot after the installation is finished. If any errors come up and the installation halts, fix them and simply run the installation again.

> A recording of a sample setup on a vanilla install of Linux Mint 18 MATE 64-bit can be found in `.setup/sample/`. You can either view the whole output at once using `cat sample_setup_output.log` or play back the installation as it happened using `scriptreplay --timing=sample_setup_time.txt --divisor=<number> sample_setup_output.log` where `<number>` speeds up the playback by *number* of times (use `--divisor=1` for real time).

> **Advice**: to build confidence in the install script's functionality for your specific OS, use either the dry-run simulation and/or install a copy of you OS in [VirtualBox](https://www.virtualbox.org/wiki/Downloads) and run the full installation there.

Note that the automated backup solution needs you to configure `cron` and `anacron` manually. See the starting comment block of the [backup script](https://github.com/danyloM/dotfiles/blob/master/.bin/make_snapshot.sh) to learn how. It would not be good to automate this step as it should be your choice on how, if at all, to run backups.

When you open Emacs for the first time, all required packages will be automatically downloaded thanks to [use-package](https://github.com/jwiegley/use-package). You will need to run `M-x irony-install-server` after opening a C/C++ file for the very first time inside Emacs.

## Known issues

 1. When in its first run Emacs auto-installs [pdf-tools](https://github.com/politza/pdf-tools), the error ``Error (use-package): pdf-tools :config: No executable `epdfinfo' found`` might be thrown. Then you just need to restart Emacs. The error will never appear again.
 2. Sometimes weird errors come up in the first run of Emacs when all packages are being auto-installed (e.g. sometimes I get an `Assertion failed: (or (= (buffer-size tar-data-buffer) (buffer-size)) ...)`). Then just restart Emacs - the error will go away and the auto-installation will proceed.
 3. For Ubuntu 14.04: an error may be thrown during installation is `apt-get update` fails due to a "Hash Sum mismatch". The solution is to run `sudo rm -vf /var/lib/apt/lists/* && sudo apt-get clean && sudo apt-get update` (possible several times until the "Hash Sum mismatch" warning disappears), then running the installation script again.

## TODO

* Test install script on more Linux distributions (priority: Ubuntu-based distributions)
* Create an uninstall script to revert changes made by `.setup/setup_new_machine.sh` install script

## License

The code is available under the [MIT license](https://github.com/danyloM/dotfiles/blob/master/LICENSE).

Everything here is put with the best of my intentions. I am not a lawyer, however, so if there are any legal issues with parts of this repository please submit an issue and I will correct the problem!
