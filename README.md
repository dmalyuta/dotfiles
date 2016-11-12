# Danylo Malyuta's Dotfiles

## Description

A set of dotfiles and a setup script to hit the ground running on a vanilla Ubuntu-based Linux installation for C/C++ developers. The objective is to have a no-questions-asked, robust and fully automated way of taking you from a fresh install to a fully configured system on which you can immediately get to work.

![screenshot](http://i.imgur.com/N3oP2Mb.png)

## Features

* [Emacs](https://www.gnu.org/software/emacs/) configuration suitable for C/C++ and LaTeX
* [Powerline](https://github.com/powerline/powerline) for bash
* [i3](http://i3wm.org/) tiling window manager
* [Arc theme](https://github.com/horst3180/arc-theme) and [Numix](https://github.com/numixproject/numix-icon-theme) icon set
* [Google Chrome](https://www.google.com/chrome/) web browser
* [Rofi](https://github.com/DaveDavenport/rofi) application launcher
* [Automated backup](https://github.com/danyloM/dotfiles/blob/master/.bin/make_snapshot.sh) using `rsync`, `cron` and `anacron` with support for grandfather-father-son backup scheme

## Contributing

Pull requests are welcome!

If you are having problems, please submit an issue - I will respond right away!

## Installation

#### Requirements

You should be running a Ubuntu-based Linux operating system. So far, however, only Linux Mint 18 MATE 64-bit has been tested. You should also be connected to the Internet, since the install script downloads a few files.

Note that the script is smart enough to backup any existing dotfiles to a directory `~/dotfile_backup_ddmmYYYY_THHMMSS/` (where the second half of the folder name is the current date and time) before overwriting any dotfile with one from this repository.

#### Instructions

1. Clone this repository into any directory `DIR` you wish with `git clone https://github.com/danyloM/dotfiles DIR`;
2. Move into `DIR` with `cd DIR`;
3. To view a dry-run simulation of the installation (without actually doing anything), use `sudo ./.setup-new-machine.sh -d`. Note that since folders aren't copied or moved in this simulation, you will see some "couldn't find folder" warnings that will not be present in the actual installation;
4. To run the installation, use `sudo ./.setup-new-machine.sh`. You are recommended to reinstall after the installation is finished. If any errors come up and the installation halts, fix them and run the installation again.

Note that the automated backup solution needs you to configure `cron` and `anacron` manually. See the starting comment block of the [backup script](https://github.com/danyloM/dotfiles/blob/master/.bin/make_snapshot.sh) to learn how. It would not be good to automate this step as it should be your choice on how, if at all, to run backups.

When you open Emacs for the first time, all required packages will be automatically downloaded thanks to [use-package](https://github.com/jwiegley/use-package). You will need to run `M-x irony-install-server` after opening a C/C++ file for the very first time inside Emacs.

## Known issues

 1. When in its first run Emacs auto-installs [pdf-tools](https://github.com/politza/pdf-tools), the error ``Error (use-package): pdf-tools :config: No executable `epdfinfo' found`` might be thrown. Then you just need to restart Emacs. The error will never appear again.
 2. Sometimes weird errors come up in the first run of Emacs when all packages are being auto-installed (e.g. sometimes I get an `Assertion failed: (or (= (buffer-size tar-data-buffer) (buffer-size)) ...)`). Then just restart Emacs - the error will go away and the auto-installation will proceed.

## TODO

* Test install script on other Linux distributions (priority: Ubuntu-based distributions)
* Modularize the setup script such that each application is self-contained and is merely "plugged" into the installation process. Thus give the option to install only parts of the dotfiles (e.g. omit i3 installation if user does not want to install this window manager).

## License

The code is available under the [MIT license](https://github.com/danyloM/dotfiles/blob/master/LICENSE).

Everything here is put with the best of my intentions. I am not a lawyer, however, so if there are any legal issues with parts of this repository please submit an issue and I will correct the problem!
