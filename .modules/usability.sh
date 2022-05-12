#!/bin/bash
#
# General usability tools and theming.
#
# Author: Danylo Malyuta, 2020.

# ..:: Shell type ::..

sudo usermod -s /bin/bash "$USERNAME"

# ..:: Terminal emulator ::..

if not_installed kitty; then
    # Instructions: https://sw.kovidgoyal.net/kitty/binary/
    curl -L https://sw.kovidgoyal.net/kitty/installer.sh | sh /dev/stdin launch=n

    # Create desktop entry for Kitty
    ln -s ~/.local/kitty.app/bin/kitty ~/.local/bin/
    cp ~/.local/kitty.app/share/applications/kitty.desktop ~/.local/share/applications/
    cp ~/.local/kitty.app/share/applications/kitty-open.desktop ~/.local/share/applications/
    sed -i "s|Icon=kitty|Icon=/home/$USER/.local/kitty.app/share/icons/hicolor/256x256/apps/kitty.png|g" \
        ~/.local/share/applications/kitty*.desktop
    sed -i "s|Exec=kitty|Exec=/home/$USER/.local/kitty.app/bin/kitty|g" \
        ~/.local/share/applications/kitty*.desktop
    sudo desktop-file-install ~/.local/share/applications/kitty*.desktop
    sudo update-desktop-database

    # Make Kitty the new terminal!
    if ! not_installed kitty; then
        sudo apt-get purge -y gnome-terminal
        sudo ln -sf ~/.local/bin/kitty /usr/bin/gnome-terminal
        sudo update-alternatives --install /usr/bin/x-terminal-emulator x-terminal-emulator ~/.local/bin/kitty 100
        sudo update-alternatives --set x-terminal-emulator ~/.local/bin/kitty
    fi
fi

# Fix bug that Ctrl-Alt-T creates a new icon sidebar
gsettings set org.gnome.desktop.default-applications.terminal exec "x-terminal-emulator"

# Open Nautilus directory in terminal
python -c "import nautilus_open_any_terminal" > /dev/null 2>&1
if [ $? -ne 0 ]; then
    sudo apt-get -y install python3-nautilus
    pip3 install --user nautilus-open-any-terminal
    mkdir -p ~/.local/share/glib-2.0/schemas/
    # WARN this is fragile with respect to the Python version
    cp anaconda3/envs/py3104/lib/python3.10/site-packages/nautilus_open_any_terminal/schemas/* \
        ~/.local/share/glib-2.0/schemas/
    glib-compile-schemas ~/.local/share/glib-2.0/schemas/
    gsettings set com.github.stunkymonkey.nautilus-open-any-terminal terminal x-terminal-emulator
fi

# ..:: Tmux ::..
# Terminal multiplexer

if not_installed tmux; then
    # Install needed packages
    sudo apt-get install -y bison \
        libevent-dev \
        libncurses-dev \
        autotools-dev \
        automake

    git clone https://github.com/tmux/tmux.git /tmp/tmux
    ( cd /tmp/tmux && \
      sh autogen.sh && \
      ./configure && \
      make && \
      sudo make install )
fi

if [ ! -d ~/.tmux/plugins/tpm ]; then
    # Tmux Plugin Manager
    mkdir -p ~/.tmux/plugins
    git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
fi

# ..:: Mouse cursor ::..

mkdir -p ~/.icons

if [ ! -d ~/.icons/GoogleDot ]; then
    wget -4 https://github.com/ful1e5/Google_Cursor/releases/download/v1.0.0/GoogleDot.tar.gz \
	 -O /tmp/GoogleDot.tar.gz
    tar -xvf /tmp/GoogleDot.tar.gz -C /tmp
    mv /tmp/GoogleDot ~/.icons/
fi

# ..:: Gnome desktop customization ::..

# In order to save current extension settings into these dotfiles:
# (source: https://askubuntu.com/a/1178587)
#
#   $ cd <DOTFILES_BASE_DIRECTORY>
#   $ dconf dump /org/gnome/shell/extensions/<EXT_NAME>/ > .config/dconf/<EXT_NAME>.dconf
#
# Note also that you can easily check which dconf settings are changed when
# using GUI, by opening a terminal and running:
#
#   $ dconf watch /
#
# This will print any settings that are changed, every time you change something in GUI.

sudo apt-get -y install dconf-cli gettext

# Gnome shell Chrom install integration
# Visit https://extensions.gnome.org/ to install extensions directly
sudo apt-get -y install chrome-gnome-shell

mkdir -p ~/.local/share/gnome-shell/extensions

# Application launchers in the panel
if [ ! -d ~/.local/share/gnome-shell/extensions/dash-to-panel@jderose9.github.com ]; then
    rm -rf /tmp/dash-to-panel
    git clone https://github.com/home-sweet-gnome/dash-to-panel.git /tmp/dash-to-panel
    (cd /tmp/dash-to-panel && make install)

    # Link to my own settings
    dconf load /org/gnome/shell/extensions/dash-to-panel/ < \
	  "$DIR"/.config/dconf/dash-to-panel.dconf
fi

# Traditional menu
if [ ! -d ~/.local/share/gnome-shell/extensions/arcmenu@arcmenu.com ]; then
    rm -rf /tmp/arcmenu
    git clone --single-branch --branch master \
        https://gitlab.com/arcmenu/ArcMenu.git /tmp/arcmenu
    (cd /tmp/arcmenu && make install)

    # Link to my own settings
    dconf load /org/gnome/shell/extensions/arcmenu/ < \
	  "$DIR"/.config/dconf/arcmenu.dconf
fi

# Focus window without "Window is ready" notification
EXTDIR="$HOME/.local/share/gnome-shell/extensions/focus-my-window@varianto25.com"
if [ ! -d "$EXTDIR" ]; then
    mkdir -p "$EXTDIR"
    git clone https://github.com/v-dimitrov/gnome-shell-extension-stealmyfocus /tmp/stealfocus
    mv /tmp/stealfocus/* "$EXTDIR"
fi

# Workspaces on all displays
dconf write /org/gnome/mutter/workspaces-only-on-primary false

# >> Now update the Gnome configuration <<

# Restart the Gnome shell, to make sure everything is up-to-date
busctl --user call org.gnome.Shell /org/gnome/Shell org.gnome.Shell \
       Eval s 'Meta.restart("Restarting Gnome Shell...")'

# No desktop icons
gsettings set org.gnome.desktop.background show-desktop-icons false
gsettings set org.gnome.shell.extensions.ding show-home false

# Show battery percentage
gsettings set org.gnome.desktop.interface show-battery-percentage true

# Use GoogleDot cursor theme
gsettings set org.gnome.desktop.interface cursor-theme 'GoogleDot'

# Mouse cursor size
gsettings set org.gnome.desktop.interface cursor-size 22

# Enable the extensions
gnome-extensions disable ubuntu-dock@ubuntu.com
gsettings set org.gnome.shell enabled-extensions \
	  "['dash-to-panel@jderose9.github.com', 'arcmenu@arcmenu.com', \
            'focus-my-window@varianto25.com']"

# Use Alt as window action key (e.g. to drag window around)
gsettings set org.gnome.desktop.wm.preferences mouse-button-modifier '<Alt>'

# Detach modal dialogs from parent window (better "Save As" window)
gsettings set org.gnome.mutter attach-modal-dialogs false

# Four static workspaces
gsettings set org.gnome.mutter dynamic-workspaces false
gsettings set org.gnome.desktop.wm.preferences num-workspaces 4

# Turn off natural scrolling
gsettings set org.gnome.desktop.peripherals.mouse natural-scroll false

# Make Capslock the Hyper key, which I use in Emacs
gsettings set org.gnome.desktop.input-sources xkb-options "['caps:hyper']"

# File browser
# Allow to unfold folders
gsettings set org.gnome.nautilus.list-view use-tree-view true
# Put folders above files
gsettings set org.gtk.Settings.FileChooser sort-directories-first true

# ..:: Input device driver ::..

sudo apt-get -y install xserver-xorg-input-all xserver-xorg-input-synaptics
sudo adduser "$USER" input

# Enable two-finger horizontal scroll with the touchpad
synclient HorizTwoFingerScroll=1

# ..:: Autokey ::..
# A desktop automation utility for Linux and X11.

if not_installed autokey; then
    # Instructions: https://www.makeuseof.com/use-autokey-to-automate-repetitive-tasks-on-linux/
    sudo apt-get -y install autokey-gtk

    # Link configuration from these dotfiles
    mkdir -p ~/.config/autokey/data
    rm -rf ~/.config/autokey/data
    ln -sf "$DIR"/.config/autokey/data ~/.config/autokey/data
    ln -sf "$DIR"/.config/autokey/autokey.json ~/.config/autokey/autokey.json
fi

# ..:: Search ::..

sudo apt-get -y install recoll \
     pdfgrep \
     exo-utils \
     tree \
     fd-find

# ..:: Rofi launcher ::..

if not_installed rofi; then
    # Install Rofi itself
    sudo apt-get -y install rofi

    # Install themes for Rofi
    git clone --depth=1 https://github.com/adi1090x/rofi.git /tmp/rofi-themes
    ( cd /tmp/rofi-themes && chmod +x setup.sh && ./setup.sh )

    # Select themes
    # Application launcher
    LAUNCHER_FILE="$HOME/.config/rofi/launchers/colorful/launcher.sh"
    sed -i -e '/theme=\"style_/s/[0-9]/5/' "$LAUNCHER_FILE"
    sed -i -e '/^themes=/s/^/#/g' "$LAUNCHER_FILE"
    sed -i -e '/^theme=.*RANDOM/s/^/#/g' "$LAUNCHER_FILE"
fi

# ..:: Wine ::..
# Windows program emulation

if not_installed wine; then
    # Instructions:
    # https://www.omgubuntu.co.uk/2021/01/wine-6-0-released-how-to-install-on-ubuntu
    # https://linuxize.com/post/how-to-install-wine-on-ubuntu-20-04/
    # https://askubuntu.com/questions/219791/improve-gui-appearance-of-wine-applications

    wget -qO- https://dl.winehq.org/wine-builds/winehq.key | sudo apt-key add -
    sudo add-apt-repository -y 'deb https://dl.winehq.org/wine-builds/ubuntu/ focal main'
    sudo apt-get -y --install-recommends install winehq-staging
    sudo apt-get -y install winetricks

    # Install fonts
    winetricks allfonts
fi

# ..:: Flatpak ::..

if not_installed flatpak; then
    sudo apt-get -y install flatpak
    flatpak remote-add --if-not-exists flathub https://flathub.org/repo/flathub.flatpakrepo
fi

# ..:: Other ::..

# Disable the message "Sorry, Ubuntu xy.zw has experienced an internal error"
sudo sed -i -e '/enabled=/s/1/0/' /etc/default/apport

sudo apt-get -y install xcalib \
     compizconfig-settings-manager \
     gnome-tweaks \
     pdftk \
     unrar \
     xdotool

# Increase inotify to make sure Evince updates on PDF update
# Source: https://superuser.com/a/1387905/512940
echo fs.inotify.max_user_watches=524288 | sudo tee -a /etc/sysctl.conf
sudo sysctl -p
