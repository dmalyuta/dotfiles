#!/bin/bash
#
# The VS code text editor and associated tools.
#
# Author: Danylo Malyuta, 2021.

# ..:: VS Code ::..

if not_installed code; then
    wget -qO- https://packages.microsoft.com/keys/microsoft.asc | gpg --dearmor > packages.microsoft.gpg

    sudo install -o root -g root -m 644 packages.microsoft.gpg /etc/apt/trusted.gpg.d/

    sudo sh -c 'echo "deb [arch=amd64,arm64,armhf signed-by=/etc/apt/trusted.gpg.d/packages.microsoft.gpg] https://packages.microsoft.com/repos/code stable main" > /etc/apt/sources.list.d/vscode.list'

    rm -f packages.microsoft.gpg

    sudo apt-get -y install apt-transport-https
    sudo apt-get update
    sudo apt-get -y install code

    # Set as default editor
    xdg-mime default code.desktop text/plain
fi

# ..:: Configuration ::..

mkdir -p ~/.vscode
ln -sf "$DIR"/.vscode/settings.json ~/.config/Code/User
ln -sf "$DIR"/.vscode/keybindings.json ~/.config/Code/User
ln -sf "$DIR"/.vscode/snippets ~/.config/Code/User

# ..:: Install extensions ::..

# To save current extensions into dotfiles, do:
# code --list-extensions > "$DIR"/.vscode/extensions.txt

# Get extension lists...
# ... that are not currently installed
VSCODE_INSTALLED_EXTENSIONS=/tmp/vscode_installed_extensions.txt
code --list-extensions > "$VSCODE_INSTALLED_EXTENSIONS"
# ... that are wanted to be installed
VSCODE_WANTED_EXTENSIONS="$DIR"/.vscode/extensions.txt
# ... that are to be removed (because they are not in the wanted extensions list)
VSCODE_EXTENSIONS_TO_REMOVE=/tmp/vscode_extensions_to_remove.txt
comm -23 <(sort -d "$VSCODE_INSTALLED_EXTENSIONS") <(sort -d "$VSCODE_WANTED_EXTENSIONS") > \
     "$VSCODE_EXTENSIONS_TO_REMOVE"
# ... that are to be installed (because they are in the wanted extensions list, but
# are not currently installed)
VSCODE_EXTENSIONS_TO_INSTALL=/tmp/vscode_extensions_to_install.txt
comm -13 <(sort -d "$VSCODE_INSTALLED_EXTENSIONS") <(sort -d "$VSCODE_WANTED_EXTENSIONS") > \
     "$VSCODE_EXTENSIONS_TO_INSTALL"

# Remove extensions that are no longer wanted
cat "$VSCODE_EXTENSIONS_TO_REMOVE" | xargs -L 1 code --uninstall-extension

# Install extensions that are not already installed
cat "$VSCODE_EXTENSIONS_TO_INSTALL" | xargs -L 1 code --install-extension
