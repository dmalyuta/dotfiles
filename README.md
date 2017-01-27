# Danylo Malyuta's Dotfiles

## Description

A set of dotfiles and a setup script to hit the ground running on a vanilla Ubuntu-based Linux installation for C/C++ developers. The objective is to have a no-questions-asked, robust and fully automated way of taking you from a fresh install to a fully configured system on which you can immediately get to work.

![screenshot](http://i.imgur.com/2tAtrGW.jpg)

## Features

* [Emacs 24.5](https://www.gnu.org/software/emacs/), configured for C/C++ and LaTeX
* Custom bash prompt showing the current user, current directory, non-zero exit code of last command and current Git branch
* [Automated backup](https://github.com/danyloM/dotfiles/blob/master/.bin/make_snapshot.sh) using `rsync`, `cron` and `anacron` with support for grandfather-father-son backup scheme
* [Google Chrome](https://www.google.com/chrome/) web browser
* [Foxit Reader](https://www.foxitsoftware.com/products/pdf-reader/) software for standards-compliant reading and annotation of PDFs

## Contributing

Pull requests are welcome!

If you are having problems, please submit an issue - I will respond right away!

## Installation

### Requirements

You should be running a Ubuntu-based Linux operating system. You can run the installation either on a completely fresh or an already used OS. You should also be connected to the Internet throughout the installation, since the install script downloads a few files. Note that the script is smart enough to backup any existing dotfiles to a directory `~/dotfile_backup_ddmmYYYY_THHMMSS/` (where the second half of the folder name is the current date and time) before overwriting any dotfile with one from this repository.

### Supported Distributions

- Ubuntu 14.04.5 Desktop 64-bit (*official*)
- Linux Mint 18 MATE 64-bit (*used in the past*)

Other distributions may work, but I did not explicitly test them yet. If you try this repository on an OS that is not in the above list, please report what you find and submit pull requests with tweaks for that OS, if you can.

### Instructions

1. Clone this repository into any directory `DIR` you wish with `git clone https://github.com/danyloM/dotfiles DIR`;
2. Move into `DIR` with `cd DIR`;
3. To view a dry-run simulation of the installation (without actually doing anything), use `sudo ./setup_new_machine.sh -d`. Note that since folders aren't copied or moved in this simulation, you will see some "couldn't find folder" warnings that will not be present in the actual installation;
4. If you wish to have your dotfiles symlinked to this Git repo's dotfiles (such that you can auto-update your dotfiles via `git pull`), install with `sudo ./setup_new_machine.sh -s`. Otherwise, install with `sudo ./setup_new_machine.sh`. The install script will prompt you for what specific parts of this repository you want to install and will give you instructions when manual work is needed (such as during the Foxit PDF Reader installation, if you choose to install it). The aim is to safely deploy this repository partially or fully to your computer. You are recommended to reboot after the installation is finished. If any errors come up and the installation halts, fix them and simply run the installation again.
  - If you subsequenly move `DIR` to another place, the symlinks from the dotfiles in `$HOME` to those in `DIR` will break. Simply `cd DIR && ./symlinks_update.sh -s` to fix this.
  - If you ever wish to unlink the dotfiles in your `$HOME` from the ones in `DIR`, run `cd DIR && ./symlinks_update.sh`.
5. (Optional) If you want to open remote files with TRAMP using C-x C-f directly from an ansi-term ssh session, then put [.bash_local](https://github.com/dmalyuta/dotfiles/blob/master/.bash_profile) in the $HOME of the remote machine (courtesy of [Enigma Curry](http://www.enigmacurry.com/2008/12/26/emacs-ansi-term-tricks/)).

> **Advice**: to build confidence in the install script's functionality for your specific OS, use the dry-run option and/or install a copy of you OS in [VirtualBox](https://www.virtualbox.org/wiki/Downloads) and run the full installation there.

Note that the automated backup solution needs you to configure `cron` and `anacron` manually. See the starting comment block of the [backup script](https://github.com/danyloM/dotfiles/blob/master/.bin/make_snapshot.sh) to learn how. It would not be good to automate this step as it should be your choice on how, if at all, to run backups.

When you open Emacs for the first time, all required packages will be automatically downloaded thanks to [use-package](https://github.com/jwiegley/use-package). You will need to run `M-x irony-install-server` after opening a C/C++ file for the very first time inside Emacs.

## Known issues

 1. In case weird errors come up in the first run of Emacs when all packages are being auto-installed, just restart Emacs - the error will go away and the auto-installation will proceed.
 3. For Ubuntu 14.04: an error may be thrown during installation is `apt-get update` fails due to a "Hash Sum mismatch". The solution is to run `sudo rm -vf /var/lib/apt/lists/* && sudo apt-get clean && sudo apt-get update` (possible several times until the "Hash Sum mismatch" warning disappears), then running the installation script again.

## TODO

* Create an uninstall script to revert changes made by `.setup/setup_new_machine.sh` install script
* Test install script on more Linux distributions (priority: Ubuntu-based distributions)

## License

The code is available under the [MIT license](https://github.com/danyloM/dotfiles/blob/master/LICENSE).

Everything here is put with the best of my intentions. I am not a lawyer, however, so if there are any legal issues with parts of this repository please submit an issue and I will correct the problem!
