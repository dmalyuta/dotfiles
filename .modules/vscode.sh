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

cat "$DIR"/.vscode/extensions.txt | xargs -L 1 code --install-extension
