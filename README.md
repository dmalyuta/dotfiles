# Ubuntu dotfiles

Ubuntu 18.04.4 LTS auto-configuration.

## Necessary manual steps

The script `setup_new_machine.sh` does most of the work for you. You can also
use `symlink_update.sh` to link files back to this repository (use the `-s`
options to keep the files as symbolic links). Here are some remaining necessary
manual steps.

Synapse app launcher setup:

* Open Synapse and in Preferences, set:
  - Startup on login
  - Activate: `<Alt>a`
  
Window management:

* Open Tweaks:
  - In Windows, set Window Action Key to Alt
  - In Workspaces, select Static Workspaces and set number to 4
  - In Appearance, set Applications to Numix and set Icons to Numix-Circle
  
Emacs:

* Navigate to `~/.emacs.d/` and:
  - MATLAB setup:
	```
	$ git clone https://github.com/dmalyuta/matlab-mode
	$ git clone https://github.com/dmalyuta/matlab-emacs
	$ cd matlab-emacs
	$ make
	```
  - Font setup: `M-x all-the-icons-install-fonts`
  - C++: with a C/C++ file open, run `irony-install-server`

## License

The code is available under the [MIT
license](https://github.com/dmalyuta/dotfiles/blob/master/LICENSE).
