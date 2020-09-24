# Ubuntu dotfiles

Ubuntu 20.04.1 LTS auto-configuration.

## Automatic install script

Use the script `setup.sh` to interact with the install program. To setup a new
machine:
```
./setup.sh setup -? # -? shows the possible options
```

To update symlinks from dotfiles back to this repository (effectively making
them version-controlled):
```
./setup.sh symlink -? # -? shows the possible options
```

## Manual steps

Synapse app launcher setup:

* Open Synapse and in Preferences, set:
  - Startup on login
  - Activate: `<Alt>a`
  
Desktop experience:

* Open Tweaks:
  - In Windows, set Window Action Key to Alt
  - In Windows, unset Attach Modal Dialogues
  - In Workspaces, select Static Workspaces and set number to 4
  - In Appearance, set Applications to Numix and set Icons to Numix-Circle
* Open Settings:
  - In Devices -> Mouse & Touchpad, turn off Natural Scrolling
  
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

## License

The code is available under the [MIT
license](https://github.com/dmalyuta/dotfiles/blob/master/LICENSE).
