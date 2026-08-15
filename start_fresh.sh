#!/bin/bash
#
# Run this script to set up a newly installed Ubuntu. Nothing has to be checked
# out first: it clones the dotfiles repo itself. On a fresh machine, run
#
#   curl -fsSL https://raw.githubusercontent.com/dmalyuta/dotfiles/master/start_fresh.sh | bash
#
# and on a machine that already has the repo, run ~/sw/dotfiles/start_fresh.sh.
#
# Safe to re-run on the same machine: every step checks for what it installs and
# skips if it is already there, so a second run does not re-download, reinstall,
# or re-append anything to a config file.
#
# Author: Danylo Malyuta, 2026.

repo_ssh=git@github.com:dmalyuta/dotfiles.git
raw_url=https://raw.githubusercontent.com/dmalyuta/dotfiles/master/start_fresh.sh

# ---------------------------------------------------------------------------
# Bootstrap.
# ---------------------------------------------------------------------------

# Under `curl ... | bash` the script itself is what is on stdin, so every `read`
# below would swallow the next lines of the script instead of waiting for the
# user. Reattaching stdin to the terminal here would not help either: bash has
# not read the rest of the script yet and would start reading it from the
# terminal. So fetch a real copy, and re-exec that with the terminal on stdin.
if [ ! -t 0 ] && [ -z "${DOTFILES_BOOTSTRAP+x}" ]; then
  # Opening it is the test: /dev/tty is readable by anyone, but opening it
  # fails when the process has no controlling terminal.
  if ! { : </dev/tty; } 2>/dev/null; then
    echo "This script is interactive and needs a terminal." >&2
    echo "Run it from a terminal, or download it and run it directly." >&2
    exit 1
  fi
  self=$(mktemp)
  if command -v curl >/dev/null 2>&1; then
    curl -fsSL "$raw_url" -o "$self" </dev/tty || exit
  elif command -v wget >/dev/null 2>&1; then
    wget -qO "$self" "$raw_url" </dev/tty || exit
  else
    # Neither downloader, yet we got here somehow (piped from a file?).
    sudo apt update </dev/tty &&
      sudo apt install -y curl </dev/tty &&
      curl -fsSL "$raw_url" -o "$self" </dev/tty || exit
  fi
  DOTFILES_BOOTSTRAP=$self exec bash "$self" "$@" </dev/tty
fi
# Clean up the copy the block above left in /tmp.
if [ -n "${DOTFILES_BOOTSTRAP:-}" ]; then
  trap 'rm -f "$DOTFILES_BOOTSTRAP"' EXIT
fi

# Where the repo lives. If this script is running out of a checkout already,
# that checkout is the one that gets linked into the home directory; otherwise
# the repo is cloned to ~/sw/dotfiles further down. Resolved before the cd
# below, since $0 may be a relative path.
dotfiles=~/sw/dotfiles
self_dir=$(cd "$(dirname "${BASH_SOURCE[0]}")" >/dev/null 2>&1 && pwd)
if [ -f "$self_dir/.bash_aliases" ] && [ -d "$self_dir/.git" ]; then
  dotfiles=$self_dir
fi

downloads=~/Downloads
mkdir -p "$downloads"
cd "$downloads" || exit

# kitty and oh-my-posh install into ~/.local/bin. Ubuntu's ~/.bashrc returns
# early in a non-interactive shell, so sourcing it would not pick them up.
export PATH="$HOME/.local/bin:$PATH"

# ---------------------------------------------------------------------------
# Helpers.
# ---------------------------------------------------------------------------

# Note that a step is being skipped.
skip() { echo "== $* already present, skipping."; }

# True if the command is on PATH.
have() { command -v "$1" >/dev/null 2>&1; }

# True if the named apt package is installed.
pkg_installed() {
  dpkg-query -W -f='${Status}' "$1" 2>/dev/null | grep -q 'ok installed'
}

# Download <url> to <file>, unless the file is already there.
fetch() {
  local file=$1 url=$2
  if [ -s "$file" ]; then
    skip "$file"
  else
    wget -O "$file" "$url"
  fi
}

# Install apt packages, skipping the ones already installed.
apt_install() {
  local pkg
  for pkg in "$@"; do
    pkg_installed "$pkg" || sudo apt install -y "$pkg"
  done
}

# Download and install a .deb. The package name is read out of the .deb itself
# rather than guessed, so a re-run skips the install.
install_deb() {
  local file=$1 url=$2 pkg
  fetch "$file" "$url" || return
  pkg=$(dpkg-deb -f "$file" Package 2>/dev/null)
  if [ -n "$pkg" ] && pkg_installed "$pkg"; then
    skip "$pkg"
    return
  fi
  sudo apt install -y "./$file"
}

# Add a PPA unless it is already in the apt sources. add-apt-repository is
# itself idempotent, but it runs an apt update every time, which is the slow bit.
add_ppa() {
  local ppa=$1
  if grep -rqs "${ppa#ppa:}" /etc/apt/sources.list.d/; then
    skip "$ppa"
  else
    sudo add-apt-repository -y "$ppa"
  fi
}

# Install Flatpak apps, skipping the ones already installed.
flatpak_install() {
  local app
  for app in "$@"; do
    if flatpak info "$app" >/dev/null 2>&1; then
      skip "$app"
    else
      flatpak install -y flathub "$app"
    fi
  done
}

# Install snaps, skipping the ones already installed.
snap_install() {
  local pkg
  for pkg in "$@"; do
    if snap list "$pkg" >/dev/null 2>&1; then
      skip "$pkg"
    else
      sudo snap install "$pkg"
    fi
  done
}

# Download an AppImage and hand it to Gear Lever, unless the .desktop file it
# creates is already there.
# Usage: gearlever_integrate <desktop file name> <appimage> <url>
gearlever_integrate() {
  local desktop=$1 file=$2 url=$3
  if [ -f ~/.local/share/applications/"$desktop" ]; then
    skip "$desktop"
    return
  fi
  fetch "$file" "$url" || return
  flatpak run it.mijorus.gearlever --integrate "$file"
}

# Write stdin to a root-owned file. Returns 0 only when the content actually
# changed, so callers can skip the expensive follow-up work (initramfs rebuild,
# daemon reload, udev reload) on a re-run.
write_root_file() {
  local dest=$1 tmp
  tmp=$(mktemp)
  cat >"$tmp"
  if sudo cmp -s "$tmp" "$dest" 2>/dev/null; then
    rm -f "$tmp"
    return 1
  fi
  sudo mkdir -p "$(dirname "$dest")"
  sudo install -m 644 "$tmp" "$dest"
  rm -f "$tmp"
}

# Sets a key in a .desktop file's [Desktop Entry] group, replacing the value if
# the key is already there. Inserts right after the group header rather than at
# the end of the file, since some of these have a trailing [Desktop Action ...].
set_desktop_key() {
  local file=$1 key=$2 value=$3
  local sudo_cmd=()
  if [ ! -f "$file" ]; then
    echo "set_desktop_key: $file not found, skipping."
    return
  fi
  [ -w "$file" ] || sudo_cmd=(sudo)
  if grep -q "^${key}=" "$file"; then
    "${sudo_cmd[@]}" sed -i "s|^${key}=.*|${key}=${value}|" "$file"
  else
    "${sudo_cmd[@]}" sed -i "/^\[Desktop Entry\]/a ${key}=${value}" "$file"
  fi
}

# ---------------------------------------------------------------------------
# Install.
# ---------------------------------------------------------------------------

# Upgrade.
sudo apt update
sudo apt upgrade -y
sudo apt autoremove --purge -y

# The rest of the script downloads, clones and unzips things, so get those out
# of the way first: a minimal Ubuntu install has none of them guaranteed.
apt_install ca-certificates curl wget git unzip

# System monitoring.
apt_install bat btop htop

# Navigation in the command line.
apt_install tree

# Password manager.
install_deb proton_pass.deb "https://proton.me/download/pass/linux/proton-pass_1.38.1_amd64.deb"

# Gnome configuration.
install_deb gnome-shell-extension-manager gnome-tweaks

# Note taking.
install_deb obsidian.deb "https://github.com/obsidianmd/obsidian-releases/releases/download/v1.13.4/obsidian_1.13.4_amd64.deb"

# Brave browser.
apt_install curl fzf
if pkg_installed brave-browser; then
  skip "Brave"
else
  curl -fsS https://dl.brave.com/install.sh | sh
fi

# VS Code editor.
apt_install shfmt
install_deb code.deb "https://code.visualstudio.com/sha/download?build=stable&os=linux-deb-x64"

# Github SSH.
if [ -n "$(git config --global user.name)" ]; then
  skip "git user.name"
else
  read -p "Git name: " -r user_answer
  git config --global user.name "$user_answer"
fi
if [ -n "$(git config --global user.email)" ]; then
  skip "git user.email"
else
  read -p "Git email: " -r user_answer
  git config --global user.email "$user_answer"
fi
# Never regenerate over an existing key, that would lock you out of anything
# already using it.
if [ -f ~/.ssh/id_ed25519 ]; then
  skip "SSH key"
else
  ssh-keygen -t ed25519 -C "$(git config --global user.email)"
fi
eval "$(ssh-agent -s)"
ssh-add ~/.ssh/id_ed25519

# Pre-seed github.com's host key, so the clone below does not stop on an
# interactive "are you sure you want to continue connecting?" prompt.
mkdir -p ~/.ssh
chmod 700 ~/.ssh
if ssh-keygen -F github.com >/dev/null 2>&1; then
  skip "github.com host key"
else
  ssh-keyscan github.com >>~/.ssh/known_hosts 2>/dev/null
fi

# Dotfiles repo.
mkdir -p ~/sw
if [ -d "$dotfiles" ]; then
  skip "dotfiles repo"
else
  # The clone is over SSH, so the key has to be on GitHub before it can work.
  until ssh -T git@github.com </dev/null 2>&1 | grep -q 'successfully authenticated'; do
    echo
    echo "Add this public key to https://github.com/settings/keys:"
    echo
    cat ~/.ssh/id_ed25519.pub
    echo
    read -p "Press Enter once it is added... " -r
  done
  git clone "$repo_ssh" "$dotfiles"
fi

# Dotfiles install. Every ln -sf and mkdir -p here is already idempotent.
cd "$dotfiles" || exit
DIR=$dotfiles
ln -sf "$DIR"/.bash_aliases ~
ln -sf "$DIR"/.local.bashrc ~
if [ ! -f ./.bin/colorizer/Library/colorizer.sh ]; then
  git submodule update --init --recursive
fi
ln -sf "$DIR"/.bin ~
mkdir -p ~/.config/kitty
ln -sf "$DIR"/.dircolors ~
ln -sf "$DIR"/.config/kitty/kitty.conf ~/.config/kitty
ln -sf "$DIR"/.config/kitty/default-kitty ~/.config/kitty
ln -sf "$DIR"/.config/kitty/resize_split.py ~/.config/kitty
ln -sf "$DIR"/.wezterm.lua ~
ln -sf "$DIR"/.alacritty.toml ~
mkdir -p ~/.config/tmux-powerline/themes
ln -sf "$DIR"/.tmux.conf ~
ln -sf "$DIR"/.config/tmux-powerline/config.sh \
  ~/.config/tmux-powerline/config.sh
ln -sf "$DIR"/.config/tmux-powerline/themes/danylo-theme.sh \
  ~/.config/tmux-powerline/themes/danylo-theme.sh
ln -sf "$DIR"/.config/.blue-owl-custom.omp.json ~/.blue-owl-custom.omp.json
cd "$downloads" || exit

# Kitty terminal. The cp/sed/echo below all overwrite, so they are re-runnable.
if [ -d ~/.local/kitty.app ]; then
  skip "kitty"
else
  curl -L https://sw.kovidgoyal.net/kitty/installer.sh | sh /dev/stdin
fi
mkdir -p ~/.local/bin ~/.local/share/applications
ln -sf ~/.local/kitty.app/bin/kitty ~/.local/kitty.app/bin/kitten ~/.local/bin/
cp ~/.local/kitty.app/share/applications/kitty.desktop ~/.local/share/applications/
cp ~/.local/kitty.app/share/applications/kitty-open.desktop ~/.local/share/applications/
sed -i "s|Icon=kitty|Icon=$(readlink -f ~)/.local/kitty.app/share/icons/hicolor/256x256/apps/kitty.png|g" ~/.local/share/applications/kitty*.desktop
sed -i "s|Exec=kitty|Exec=$(readlink -f ~)/.local/kitty.app/bin/kitty|g" ~/.local/share/applications/kitty*.desktop
echo 'kitty.desktop' >~/.config/xdg-terminals.list

# Fan control.
if pkg_installed coolercontrol; then
  skip "Cooler Control"
else
  read -p "Install Cooler Control? [yN] " -r user_answer
  if [[ "$user_answer" =~ ^[Yy]$ ]]; then
    apt_install curl apt-transport-https
    curl -1sLf 'https://dl.cloudsmith.io/public/coolercontrol/coolercontrol/setup.deb.sh' | sudo -E bash
    sudo apt install -y coolercontrol
    sudo systemctl enable --now coolercontrold

 		read -p "Try to find fans on desktop? [yN] " -r user_answer
 		if [[ "$user_answer" =~ ^[Yy]$ ]]; then
 			sudo modprobe nct6775
 			echo "nct6775" | write_root_file /etc/modules-load.d/nct6775.conf
 		fi

 		read -p "Restore Cooler Control settings from backup? [yN] " -r user_answer
 		if [[ "$user_answer" =~ ^[Yy]$ ]]; then
 			while true; do
 				read -e -p "Path to the Cooler Control backup file: " -r cc_backup
 				cc_backup="${cc_backup/#\~/$HOME}"
 				[ -f "$cc_backup" ] && break
 				echo "$cc_backup not found, try again."
 			done

 			sudo systemctl stop coolercontrold
 			sudo tar -xvf "$cc_backup" -C /
 			sudo systemctl start coolercontrold
 		fi
 	fi
fi

# OpenRazer.
sudo gpasswd -a "$USER" plugdev
apt_install software-properties-gtk
add_ppa ppa:openrazer/stable
add_ppa ppa:polychromatic/stable
apt_install openrazer-meta polychromatic

# Flathub + apps.
apt_install flatpak gnome-software-plugin-flatpak
flatpak remote-add --if-not-exists flathub https://dl.flathub.org/repo/flathub.flatpakrepo
flatpak_install it.mijorus.gearlever com.github.tchx84.Flatseal io.github.tanaybhomia.Whisp
# Flatpak's export dirs are only added to XDG_DATA_DIRS at login, so pull them
# in now to silence the warning for the rest of this script.
. /etc/profile.d/flatpak.sh
if grep -rqs universe /etc/apt/sources.list.d/ /etc/apt/sources.list; then
  skip "universe"
else
  sudo add-apt-repository -y universe
fi
apt_install libfuse2t64

# Now move apps into Gear Lever.
gearlever_integrate nextcloud_desktop.desktop nextcloud.AppImage \
  "https://github.com/nextcloud-releases/desktop/releases/download/v34.0.1/Nextcloud-34.0.1-x86_64.AppImage"
gearlever_integrate gnu_image_manipulation_program.desktop GIMP.AppImage \
  "https://download.gimp.org/gimp/v3.2/linux/GIMP-3.2.4-x86_64.AppImage"

# Install MATLAB
if [ -d /usr/local/MATLAB ]; then
  skip "MATLAB"
else
  read -p "Install MATLAB? [yN] " -r user_answer
  if [[ "$user_answer" =~ ^[Yy]$ ]]; then
    while [ ! -f matlab_R2026a_Linux.zip ]; do
      echo "matlab_R2026a_Linux.zip not found in $(pwd)."
      echo "Download MATLAB from https://www.mathworks.com/downloads/ and put the zip here."
      read -p "Press Enter to try again... " -r
    done
    [ -d matlab_R2026a ] || unzip matlab_R2026a_Linux.zip -d matlab_R2026a
    cd matlab_R2026a || exit
    xhost +SI:localuser:root
    sudo -H ./install
    xhost -SI:localuser:root
    cd "$downloads" || exit
  fi
fi
[ -d /usr/local/MATLAB ] && apt_install matlab-support

# Download OpenRGB.
if [ -f ~/.local/share/applications/openrgb.desktop ]; then
  skip "OpenRGB"
else
  read -p "Install OpenRGB? [yN] " -r user_answer
  if [[ "$user_answer" =~ ^[Yy]$ ]]; then
    fetch openrgb.zip "https://gitlab.com/CalcProgrammer1/OpenRGB/-/jobs/artifacts/master/download?job=Linux%20amd64%20AppImage"
    unzip -o openrgb.zip -d openrgb
    # Copy the rules from the downloaded openrgb zip folder for the latest build
    sudo cp openrgb/60-openrgb.rules /etc/udev/rules.d/
    sudo udevadm control --reload-rules
    sudo udevadm trigger
    # Add user permissions
    getent group i2c >/dev/null || sudo groupadd --system i2c
    sudo usermod "$USER" -aG i2c
    sudo modprobe i2c-dev
    sudo modprobe i2c-piix4
    echo i2c-dev | write_root_file /etc/modules-load.d/i2c-dev.conf
    echo i2c-piix4 | write_root_file /etc/modules-load.d/i2c-piix4.conf
    flatpak run it.mijorus.gearlever --integrate openrgb/OpenRGB-x86_64.AppImage
    # Link settings.
    mkdir -p ~/.config/OpenRGB/profiles
    mkdir -p ~/.config/OpenRGB/plugins
    ln -sf "$DIR"/.config/OpenRGB/Configuration.json ~/.config/OpenRGB/
    ln -sf "$DIR"/.config/OpenRGB/OpenRGB.json ~/.config/OpenRGB/
    ln -sf "$DIR"/.config/OpenRGB/profiles/blue.json ~/.config/OpenRGB/profiles/
    ln -sf "$DIR"/.config/OpenRGB/profiles/off.json ~/.config/OpenRGB/profiles/
  fi
fi

# Rust compiler.
if [ -d ~/.cargo ]; then
  skip "Rust"
else
  curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
fi

# Tmux.
tmux_version=3.7b
if [ "$(tmux -V 2>/dev/null)" = "tmux $tmux_version" ]; then
  skip "tmux $tmux_version"
else
  fetch tmux.tar.gz "https://github.com/tmux/tmux/releases/download/$tmux_version/tmux-$tmux_version.tar.gz"
  tar xvf tmux.tar.gz
  apt_install cmake g++ pkg-config libfontconfig1-dev libxcb-xfixes0-dev \
    libxkbcommon-dev python3 libevent-dev libncurses-dev bison
  cd "tmux-$tmux_version" || exit
  ./configure
  make
  sudo make install
  cd "$downloads" || exit
fi
if [ -d ~/.tmux/plugins/tpm ]; then
  skip "tpm"
else
  git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
fi

# Bashrc setup. Keyed off the oh-my-posh line so a re-run does not append the
# whole block a second time.
if grep -qF 'oh-my-posh init bash --config ~/.blue-owl-custom.omp.json' ~/.bashrc; then
  skip "bashrc block"
else
  cat >>~/.bashrc <<'EOF'

# Binaries.
export PATH=$PATH:~/.local/bin
export PATH=$PATH:~/.local/bin/envycontrol
export PATH=$PATH:~/.bin/git-custom-commands

# Enable fzf commands
eval "$(fzf --bash)"

# Oh-my-posh
eval "$(oh-my-posh init bash --config ~/.blue-owl-custom.omp.json)"

# Aliases.
if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# Custom bashrc setup.
if [ -f ~/.local.bashrc ]; then
    . ~/.local.bashrc
fi
EOF
fi

# Oh-my-posh prompt.
if have oh-my-posh; then
  skip "oh-my-posh"
else
  curl -s https://ohmyposh.dev/install.sh | bash -s
fi
if fc-list 2>/dev/null | grep -qi caskaydia; then
  skip "CascadiaCode font"
else
  oh-my-posh font install CascadiaCode
fi

# Search tools.
add_ppa ppa:christian-boxdoerfer/fsearch-stable
apt_install fsearch fd-find ripgrep

# Inkscape.
add_ppa ppa:inkscape.dev/stable
apt_install inkscape

# OBS.
add_ppa ppa:obsproject/obs-studio
apt_install obs-studio

# VLC.
snap_install vlc

# PureRef.
if pkg_installed pureref; then
  skip "PureRef"
else
  while [ ! -f PureRef-2.1.3_x64.deb ]; do
    echo "Download PureRef from https://www.pureref.com/download.php into $downloads"
    read -p "Press Enter to try again... " -r
  done
  sudo apt install -y ./PureRef-2.1.3_x64.deb
fi

# Calculators.
snap_install qalculate
apt_install qalc
install_deb speedcrunch.deb "https://bitbucket.org/heldercorreia/speedcrunch/downloads/SpeedCrunch-0.12-linux64.deb"

# Anaconda Python.
if [ -d ~/anaconda3 ]; then
  skip "Anaconda"
else
  fetch anaconda.sh "https://repo.anaconda.com/archive/Anaconda3-2026.07-1-Linux-x86_64.sh"
  bash anaconda.sh
fi

# NordVPN.
if have nordvpn; then
  skip "NordVPN"
else
  sh <(wget -qO - https://downloads.nordcdn.com/apps/linux/install.sh) -p nordvpn-gui
fi
getent group nordvpn >/dev/null || sudo groupadd nordvpn
sudo usermod -aG nordvpn "$USER"

# Node.js.
if [ -d ~/.nvm ]; then
  skip "nvm"
else
  curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.40.6/install.sh | bash
fi
\. "$HOME/.nvm/nvm.sh"
nvm install 26
node -v # Should print "v26.7.0".
npm -v  # Should print "11.19.0".

# Neovim.
if [ -d ~/.config/nvim ]; then
  skip "LazyVim"
else
  sudo apt install -y neovim
  # My lazyvim configuration.
  ln -s "$DIR"/.config/nvim ~/.config/nvim
fi

# Flameshot screenshot.
if pkg_installed flameshot; then
  skip "flameshot"
else
  fetch flameshot.zip "https://github.com/flameshot-org/flameshot/releases/download/v14.0.0/flameshot-v14.0+git0.da6121bd-artifact-ubuntu-24.04-amd64.zip"
  unzip -o flameshot.zip -d flameshot
  sudo apt install -y ./flameshot/flameshot-14.0.0-1.ubuntu-24.04.amd64.deb
fi

# mt76 WiFi driver.
if [ -d ~/sw/mt76 ]; then
  skip "mt76"
else
  read -p "Install mt76 WiFi driver? [yN] " -r user_answer
  if [[ "$user_answer" =~ ^[Yy]$ ]]; then
    git clone https://github.com/morrownr/mt76 ~/sw/mt76
    cd ~/sw/mt76 || exit
    sudo sh install-driver.sh
    cd "$downloads" || exit
  fi
fi

# asusctl for laptop.
if [ -d ~/sw/asusctl ]; then
  skip "asusctl"
else
  read -p "Install asusctl? [yN] " -r user_answer
  if [[ "$user_answer" =~ ^[Yy]$ ]]; then
    git clone https://github.com/OpenGamingCollective/asusctl ~/sw/asusctl
    cd ~/sw/asusctl || exit
    sudo apt install -y make cargo gcc pkg-config openssl libasound2-dev cmake build-essential \
      python3 libfreetype6-dev libexpat1-dev libxcb-composite0-dev libssl-dev libx11-dev \
      libfontconfig1-dev curl libclang-dev libudev-dev checkinstall libseat-dev libinput-dev \
      libxkbcommon-dev libgbm-dev
    make
    sudo make install
    systemctl daemon-reload
    systemctl enable asusd
    systemctl start asusd
    cd "$downloads" || exit
  fi
fi

# Brother printer driver.
if pkg_installed mfcj805dwpdrv; then
  skip "Brother printer driver"
else
  read -p "Install Brother printer driver? [yN] " -r user_answer
  if [[ "$user_answer" =~ ^[Yy]$ ]]; then
    fetch brother.gz "https://download.brother.com/welcome/dlf006893/linux-brprinter-installer-2.2.6-0.gz"
    # -k so the .gz survives and the next run does not re-download it.
    [ -f brother ] || gunzip -k brother.gz
    sudo -H bash brother MFC-J805DW
  fi
fi

# Grafana.
apt_install apt-transport-https wget gnupg
if pkg_installed grafana-enterprise; then
  skip "Grafana"
else
  sudo mkdir -p /etc/apt/keyrings
  [ -s /etc/apt/keyrings/grafana.asc ] ||
    sudo wget -O /etc/apt/keyrings/grafana.asc https://apt.grafana.com/gpg-full.key
  sudo chmod 644 /etc/apt/keyrings/grafana.asc
  # A plain write, not an append: appending duplicates the line every run.
  echo "deb [signed-by=/etc/apt/keyrings/grafana.asc] https://apt.grafana.com stable main" |
    write_root_file /etc/apt/sources.list.d/grafana.list
  sudo apt update
  sudo apt install -y grafana-enterprise
fi
sudo systemctl enable --now grafana-server
# Make sure you do NOT use chmod -R (that'll apply to all directories and has to be undone manually)
chmod o+rx "$HOME"
# Fix Grafana not seeing /home. This writes the same drop-in that
# `systemctl edit grafana-server` would create interactively.
if write_root_file /etc/systemd/system/grafana-server.service.d/override.conf <<'EOF'; then
[Service]
ProtectHome=false
EOF
  sudo systemctl daemon-reload
  sudo systemctl restart grafana-server
fi

# Don't wake up system from mouse or keyboard.
udev_changed=0
echo 'ACTION=="add", SUBSYSTEM=="usb", DRIVERS=="usb", ATTRS{idVendor}=="1532", ATTRS{idProduct}=="00cc", ATTR{power/wakeup}="disabled"' |
  write_root_file /etc/udev/rules.d/razer-mouse.rules && udev_changed=1
echo 'ACTION=="add", SUBSYSTEM=="usb", DRIVERS=="usb", ATTRS{idVendor}=="3434", ATTRS{idProduct}=="0230", ATTR{power/wakeup}="disabled"' |
  write_root_file /etc/udev/rules.d/keychron-keyboard.rules && udev_changed=1
if [ "$udev_changed" -eq 1 ]; then
  sudo udevadm control --reload
fi

# Fix Nvidia wake-up.
if echo 'options nvidia NVreg_PreserveVideoMemoryAllocations=1' |
  write_root_file /etc/modprobe.d/zz-nvidia-local.conf; then
  # Only rebuild the initramfs when the option actually changed.
  sudo update-initramfs -u
fi

# Fix icons.
apps=~/.local/share/applications

set_desktop_key "$apps"/gnu_image_manipulation_program.desktop StartupWMClass "gimp"
set_desktop_key "$apps"/gnu_image_manipulation_program.desktop X-AppImage-Name "GNU Image Manipulation Program"

set_desktop_key "$apps"/nextcloud_desktop.desktop StartupWMClass "Nextcloud"
set_desktop_key "$apps"/nextcloud_desktop.desktop X-AppImage-Name "Nextcloud Desktop"

set_desktop_key "$apps"/openrgb.desktop StartupWMClass "AppRun.wrapped"
set_desktop_key "$apps"/openrgb.desktop X-AppImage-Name "OpenRGB"

set_desktop_key /usr/share/applications/matlab.desktop StartupWMClass "MATLAB R2026a Update 4"
set_desktop_key /usr/share/applications/matlab.desktop X-AppImage-Name "MATLAB R2026a"

# ---------------------------------------------------------------------------
# Gnome desktop configuration.
# ---------------------------------------------------------------------------

if ! have gsettings; then
  echo "gsettings not found, skipping Gnome configuration."
else
  # Add a path to the custom-keybindings list, unless it is already there.
  add_custom_keybinding_path() {
    local path=$1 current new
    current=$(gsettings get org.gnome.settings-daemon.plugins.media-keys custom-keybindings)
    [[ "$current" == *"'$path'"* ]] && return
    if [ "$current" = "@as []" ]; then
      new="['$path']"
    else
      new="${current%]}, '$path']"
    fi
    gsettings set org.gnome.settings-daemon.plugins.media-keys custom-keybindings "$new"
  }

  # Define (or redefine) a custom shortcut that runs a command.
  set_custom_shortcut() {
    local slug=$1 name=$2 command=$3 binding=$4
    local path="/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/$slug/"
    local schema="org.gnome.settings-daemon.plugins.media-keys.custom-keybinding:$path"
    gsettings set "$schema" name "$name"
    gsettings set "$schema" command "$command"
    gsettings set "$schema" binding "$binding"
    add_custom_keybinding_path "$path"
  }

  # Key repeat: 150 ms before the first repeat, then one every 20 ms. Same as
  # Settings > Accessibility > Typing > Repeat Keys.
  gsettings set org.gnome.desktop.peripherals.keyboard repeat true
  gsettings set org.gnome.desktop.peripherals.keyboard delay "uint32 150"
  gsettings set org.gnome.desktop.peripherals.keyboard repeat-interval "uint32 20"

  # Pointer speed, on the [-1, 1] scale of the slider in Settings > Mouse &
  # Touchpad. A bit below the middle.
  gsettings set org.gnome.desktop.peripherals.mouse speed -0.325

  # Traditional scrolling on a laptop: moving the fingers down scrolls the
  # content down. Same as Settings > Mouse & Touchpad > Touchpad > Scrolling
  # Direction. Desktops have no touchpad, so there is nothing to set there.
  if grep -qi touchpad /proc/bus/input/devices 2>/dev/null; then
    gsettings set org.gnome.desktop.peripherals.touchpad natural-scroll false
  else
    echo "No touchpad found, skipping touchpad settings."
  fi

  # Keep the screen at the brightness it is set to, instead of letting the
  # ambient light sensor change it. Same as turning off Settings > Power >
  # Automatic Brightness, which only shows up on a built-in panel.
  if compgen -G '/sys/class/backlight/*' >/dev/null; then
    gsettings set org.gnome.settings-daemon.plugins.power ambient-enabled false
  else
    echo "No internal backlight found, skipping adaptive brightness setting."
  fi

  # Window management.
  gsettings set org.gnome.desktop.wm.keybindings minimize "['<Super>h']"
  gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-left "['<Control><Super>Left']"
  gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-right "['<Control><Super>Right']"
  gsettings set org.gnome.desktop.wm.keybindings move-to-workspace-left "['<Shift><Control><Super>Left']"
  gsettings set org.gnome.desktop.wm.keybindings move-to-workspace-right "['<Shift><Control><Super>Right']"

  # App launchers.
  set_custom_shortcut flameshot Flameshot "flameshot gui" "<Shift><Control><Alt>p"
  set_custom_shortcut speedcrunch SpeedCrunch speedcrunch "<Shift><Control><Alt>n"
  set_custom_shortcut brave Brave brave-browser "<Shift><Control><Alt>b"
  set_custom_shortcut pureref PureRef PureRef "<Shift><Control><Alt>r"
  set_custom_shortcut obsidian Obsidian obsidian "<Shift><Control><Alt>o"
fi
