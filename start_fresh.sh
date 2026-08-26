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

# Install the Flatpak GL extension matching the installed Nvidia driver, so
# Flatpak apps get hardware-accelerated rendering under Wayland instead of
# falling back to broken software GL (which shows up as GTK's
# "Error 71 (Protocol error) dispatching to Wayland display").
flatpak_install_nvidia_gl() {
	local version ref
	version=$(modinfo -F version nvidia 2>/dev/null) || return
	[ -n "$version" ] || return
	ref="org.freedesktop.Platform.GL.nvidia-${version//./-}"
	if flatpak info "$ref" >/dev/null 2>&1; then
		skip "$ref"
	else
		flatpak install -y flathub "$ref"
	fi
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
	flatpak run it.mijorus.gearlever --integrate "$file" -y
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

# Nvidia driver.
apt_install nvidia-driver-610-open

# The rest of the script downloads, clones and unzips things, so get those out
# of the way first: a minimal Ubuntu install has none of them guaranteed.
apt_install ca-certificates curl wget git unzip

# System monitoring.
apt_install bat btop htop

# Navigation in the command line.
apt_install tree

# Password manager.
install_deb proton_pass.deb "https://proton.me/download/PassDesktop/linux/x64/ProtonPass.deb"

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
		apt_install coolercontrol
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
flatpak_install_nvidia_gl
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
		flatpak run it.mijorus.gearlever --integrate openrgb/OpenRGB-x86_64.AppImage -y
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
	apt_install neovim
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
		apt_install make cargo gcc pkg-config openssl libasound2-dev cmake build-essential \
			python3 libfreetype6-dev libexpat1-dev libxcb-composite0-dev libssl-dev libx11-dev \
			libfontconfig1-dev curl libclang-dev libudev-dev checkinstall libseat-dev libinput-dev \
			libxkbcommon-dev libgbm-dev gettext
		make
		sudo make install
		systemctl daemon-reload
		systemctl enable asusd
		systemctl start asusd
		cd "$downloads" || exit
	fi
fi

# envycontrol hybrid GPU switching for laptop.
if [ -d ~/sw/envycontrol ]; then
	skip "envycontrol"
else
	read -p "Install envycontrol hybrid GPU switching? [yN] " -r user_answer
	if [[ "$user_answer" =~ ^[Yy]$ ]]; then
		git clone https://github.com/bayasdev/envycontrol ~/sw/envycontrol
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
	apt_install grafana-enterprise
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

# Wine.
if pkg_installed winehq-devel; then
	skip "wine"
else
	sudo dpkg --add-architecture i386
	wget -qO- https://dl.winehq.org/wine-builds/winehq.key | sudo gpg --dearmor -o /etc/apt/keyrings/winehq-archive.key
	sudo wget -NP /etc/apt/sources.list.d/ https://dl.winehq.org/wine-builds/ubuntu/dists/resolute/winehq-resolute.sources
	sudo apt update
	sudo apt install --install-recommends -y winehq-devel
	apt_install winetricks
	winetricks corefonts
fi

# PDF-XChange editor.
read -p "Install PDF-XChange Editor? [yN] " -r user_answer
if [[ "$user_answer" =~ ^[Yy]$ ]]; then
	while [ ! -f EditorV11.x64.msi ]; do
		echo "Download PDF-XChange Editor Plus 64-bit MSI installer from https://www.pdf-xchange.com/product/downloads into $downloads"
		read -p "Press Enter to try again... " -r
	done
	wine msiexec /i EditorV11.x64.msi
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
if [ -f /etc/modprobe.d/zz-nvidia-local.conf ]; then
	skip "Nvidia sleep fix"
else
	read -p "Attempt to fix sleep issues with Nvidia GPU? [yN] " -r user_answer
	if [[ "$user_answer" =~ ^[Yy]$ ]]; then
		sudo systemctl enable nvidia-suspend.service
		sudo systemctl enable nvidia-hibernate.service
		sudo systemctl enable nvidia-resume.service

		if write_root_file /etc/modprobe.d/zz-nvidia-local.conf <<'EOF'; then
options nvidia NVreg_PreserveVideoMemoryAllocations=1
options nvidia NVreg_TemporaryFilePath=/var/tmp
EOF
			# Only rebuild the initramfs when the option actually changed.
			sudo update-initramfs -u
		fi
	fi
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
# Desktop configuration shared values.
# ---------------------------------------------------------------------------
# Used by both the Gnome and KDE branches below, so a future tweak only has
# to be made in one place.

# Key repeat: how long before the first repeat, and how long between the
# repeats after that.
kbd_repeat_delay_ms=150
kbd_repeat_interval_ms=20

# Pointer speed, on the [-1, 1] scale both desktops use for their speed
# slider. A bit below the middle.
pointer_speed=-0.325

# App launchers: slug, display name, command, and the letter combined with
# Shift+Control+Alt below.
app_launchers=(
	"flameshot|Flameshot|flameshot gui|p"
	"speedcrunch|SpeedCrunch|speedcrunch|n"
	"brave|Brave|brave-browser|b"
	"pureref|PureRef|PureRef|r"
	"obsidian|Obsidian|obsidian|o"
)

# ---------------------------------------------------------------------------
# Gnome desktop configuration.
# ---------------------------------------------------------------------------

if [[ ${XDG_CURRENT_DESKTOP,,} != *gnome* ]]; then
	# gsettings itself is not a reliable check: it (and the schemas it reads)
	# get pulled in as a dependency of plenty of non-Gnome apps.
	echo "Not running Gnome, skipping Gnome configuration."
elif ! have gsettings; then
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
	gsettings set org.gnome.desktop.peripherals.keyboard delay "uint32 $kbd_repeat_delay_ms"
	gsettings set org.gnome.desktop.peripherals.keyboard repeat-interval "uint32 $kbd_repeat_interval_ms"

	# Pointer speed, on the [-1, 1] scale of the slider in Settings > Mouse &
	# Touchpad. A bit below the middle.
	gsettings set org.gnome.desktop.peripherals.mouse speed "$pointer_speed"

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
	gsettings set org.gnome.desktop.wm.keybindings maximize "['<Super>Up']"
	gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-left "['<Control><Super>Left']"
	gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-right "['<Control><Super>Right']"
	gsettings set org.gnome.desktop.wm.keybindings move-to-workspace-left "['<Shift><Control><Super>Left']"
	gsettings set org.gnome.desktop.wm.keybindings move-to-workspace-right "['<Shift><Control><Super>Right']"

	# App launchers.
	for entry in "${app_launchers[@]}"; do
		IFS='|' read -r slug name command key <<<"$entry"
		set_custom_shortcut "$slug" "$name" "$command" "<Shift><Control><Alt>$key"
	done
fi

# ---------------------------------------------------------------------------
# KDE desktop configuration.
# ---------------------------------------------------------------------------

if [[ ${XDG_CURRENT_DESKTOP,,} != *kde* ]]; then
	echo "Not running KDE, skipping KDE configuration."
elif ! have kwriteconfig6 || ! have gdbus; then
	echo "kwriteconfig6 or gdbus not found, skipping KDE configuration."
else
	# kwriteconfig6 with change notification, so running programs reload the
	# setting instead of keeping their stale in-memory copy.
	kconf() { kwriteconfig6 --notify "$@"; }

	# The Qt key code of a "Mod+Mod+Key" binding, which is how kglobalaccel's
	# DBus API takes shortcuts. Only the keys used below are covered.
	qt_keycode() {
		local part code=0 parts
		IFS='+' read -ra parts <<<"$1"
		for part in "${parts[@]}"; do
			case "$part" in
			Shift) ((code += 0x02000000)) ;;
			Ctrl) ((code += 0x04000000)) ;;
			Alt) ((code += 0x08000000)) ;;
			Meta) ((code += 0x10000000)) ;;
			Left) ((code += 0x01000012)) ;;
			Up) ((code += 0x01000013)) ;;
			Right) ((code += 0x01000014)) ;;
			Down) ((code += 0x01000015)) ;;
			Del) ((code += 0x01000007)) ;;
			None) ;;
			[A-Za-z]) ((code += $(printf '%d' "'${part^^}"))) ;;
			*)
				echo "qt_keycode: unknown key '$part'" >&2
				return 1
				;;
			esac
		done
		echo "$code"
	}

	# Bind a global shortcut through the kglobalaccel DBus API. Editing
	# kglobalshortcutsrc does not work: the daemon (KWin itself on Wayland)
	# keeps shortcuts in memory, never re-reads the file while running, and
	# writes its stale copy back over any edits. Changes made through the API
	# take effect immediately and the daemon persists them itself.
	set_kde_shortcut() {
		local component=$1 comp_name=$2 action=$3 action_name=$4 binding=$5
		local id="['$component','$action','$comp_name','$action_name']" code
		# Resolve the key code first: passing an empty key list to the API would
		# unbind the action rather than leave it alone.
		code=$(qt_keycode "$binding") || return
		gdbus call --session --dest org.kde.kglobalaccel --object-path /kglobalaccel \
			--method org.kde.KGlobalAccel.doRegister "$id" >/dev/null
		gdbus call --session --dest org.kde.kglobalaccel --object-path /kglobalaccel \
			--method org.kde.KGlobalAccel.setForeignShortcut "$id" "[$code]" >/dev/null
	}

	# Key repeat: 150 ms before the first repeat, then one every 20 ms (a rate
	# of 50/s). Same as System Settings > Keyboard > Advanced.
	kconf --file kcminputrc --group Keyboard --key RepeatDelay "$kbd_repeat_delay_ms"
	kconf --file kcminputrc --group Keyboard --key RepeatRate "$((1000 / kbd_repeat_interval_ms))"

	# Pointer speed for mice (same [-1, 1] scale as the Gnome slider above) and
	# traditional scrolling for touchpads, matching the Gnome branch. KDE has
	# no "all mice" setting the way Gnome does: it keys libinput settings per
	# device, as nested kcminputrc groups [Libinput][vendor][product][name]
	# with the IDs in decimal. So walk /proc/bus/input/devices and write every
	# pointer device - the ones with a mouseN handler - individually.
	found_mouse=0 found_touchpad=0
	vendor="" product="" name="" handlers=""
	while IFS= read -r line; do
		case "$line" in
		I:*)
			[[ "$line" =~ Vendor=([0-9a-f]+)\ Product=([0-9a-f]+) ]] &&
				vendor=$((16#${BASH_REMATCH[1]})) product=$((16#${BASH_REMATCH[2]}))
			;;
		N:*)
			name=${line#*\"}
			name=${name%\"}
			;;
		H:*)
			handlers=${line#*=}
			;;
		"")
			if [ -n "$name" ] && [[ "$handlers" == *mouse* ]]; then
				if [[ "${name,,}" == *touchpad* ]]; then
					kconf --file kcminputrc --group Libinput --group "$vendor" \
						--group "$product" --group "$name" --key NaturalScroll false
					found_touchpad=1
				else
					# "--" so the negative value is not parsed as more options.
					kconf --file kcminputrc --group Libinput --group "$vendor" \
						--group "$product" --group "$name" --key PointerAcceleration -- "$pointer_speed"
					found_mouse=1
				fi
			fi
			vendor="" product="" name="" handlers=""
			;;
		esac
	done < <(
		cat /proc/bus/input/devices 2>/dev/null
		echo
	)
	[ "$found_mouse" -eq 1 ] || echo "No mouse found, skipping pointer speed."
	[ "$found_touchpad" -eq 1 ] || echo "No touchpad found, skipping touchpad settings."

	# Ambient-light auto-brightness only landed in Plasma 6.6, with no stable
	# config-file switch yet, so this can only point at the GUI.
	if compgen -G '/sys/class/backlight/*' >/dev/null; then
		echo "Internal backlight found: if System Settings > Display has an auto-brightness toggle, turn it off there."
	else
		echo "No internal backlight found, skipping adaptive brightness setting."
	fi

	# Window management. Quick Tile Top holds Meta+Up by default and has to
	# give it up first, or the daemon refuses the conflicting Maximize bind
	# and leaves Maximize with no shortcut at all.
	while IFS='|' read -r action action_name binding; do
		set_kde_shortcut kwin KWin "$action" "$action_name" "$binding"
	done <<'EOF'
Window Minimize|Minimize Window|Meta+Del
Window Quick Tile Top|Quick Tile Window to the Top|None
Window Maximize|Maximize Window|Meta+Up
Switch One Desktop to the Left|Switch One Desktop to the Left|Meta+Ctrl+Left
Switch One Desktop to the Right|Switch One Desktop to the Right|Meta+Ctrl+Right
Window One Desktop to the Left|Window One Desktop to the Left|Meta+Ctrl+Shift+Left
Window One Desktop to the Right|Window One Desktop to the Right|Meta+Ctrl+Shift+Right
EOF

	# App launchers. Plasma 6 dropped the "Custom Shortcuts" KCM; the
	# replacement for a run-a-command shortcut is a hidden .desktop launcher
	# that the shortcut points at. The sycoca rebuild is what lets kglobalaccel
	# resolve the new launchers, so it has to happen before they are bound.
	for entry in "${app_launchers[@]}"; do
		IFS='|' read -r slug name command key <<<"$entry"
		file=~/.local/share/applications/"$slug".desktop
		[ -f "$file" ] || printf '[Desktop Entry]\n' >"$file"
		set_desktop_key "$file" Type Application
		set_desktop_key "$file" Name "$name"
		set_desktop_key "$file" Exec "$command"
		set_desktop_key "$file" NoDisplay true
		set_desktop_key "$file" StartupNotify false
		set_desktop_key "$file" X-KDE-GlobalAccel-CommandShortcut true
	done
	have kbuildsycoca6 && kbuildsycoca6 >/dev/null 2>&1
	for entry in "${app_launchers[@]}"; do
		IFS='|' read -r slug name command key <<<"$entry"
		set_kde_shortcut "$slug.desktop" "$name" _launch "$name" "Shift+Ctrl+Alt+${key^^}"
	done

	# Ctrl+Alt+T opens a terminal by default, bound to Konsole; rebind it to
	# kitty (already given its own .desktop entry earlier in this script).
	set_kde_shortcut org.kde.konsole.desktop Konsole _launch Konsole None
	set_kde_shortcut kitty.desktop kitty _launch kitty Ctrl+Alt+T

	# Dark theme.
	if [ "$(kreadconfig6 --file kdeglobals --group KDE --key LookAndFeelPackage)" = "org.kde.breezedark.desktop" ]; then
		skip "Dark theme"
	else
		plasma-apply-lookandfeel --apply org.kde.breezedark.desktop
	fi

	# Remove animations. The duration factor turns every fixed-duration
	# animation instant; the effects below animate or deform regardless, so
	# disable them outright:
	#   wobblywindows, magiclamp   - drag deformation and minimize physics
	#   translucency               - windows fading translucent while dragged
	#   squash                     - minimize-to-taskbar animation
	#   scale, fade, glide         - window open/close/hide transitions
	#   maximize, fullscreen       - maximize/fullscreen transitions
	#   slide, fadedesktop         - transitions between workspaces
	kconf --file kdeglobals --group KDE --key AnimationDurationFactor 0
	kde_disabled_effects=(wobblywindows magiclamp translucency squash
		scale fade glide maximize fullscreen slide fadedesktop)
	for effect in "${kde_disabled_effects[@]}"; do
		kconf --file kwinrc --group Plugins --key "${effect}Enabled" false
	done
	# KWin only re-reads kwinrc when told to, and a reconfigure still does not
	# unload already-running effects, so kick those out directly (unloading an
	# effect that is not loaded is a harmless no-op).
	gdbus call --session --dest org.kde.KWin --object-path /KWin \
		--method org.kde.KWin.reconfigure >/dev/null
	for effect in "${kde_disabled_effects[@]}"; do
		gdbus call --session --dest org.kde.KWin --object-path /Effects \
			--method org.kde.kwin.Effects.unloadEffect "$effect" >/dev/null
	done
fi

# ---------------------------------------------------------------------------
# Windows 11 virtual machine.
# ---------------------------------------------------------------------------

# Last on purpose: this is the only part of the script that needs the machine
# rebooted part way through. The integrated GPU has to be handed to vfio-pci on
# the kernel command line before a guest can use it, and a kernel command line
# only takes effect at boot. Everything above has finished by the time this
# asks, so the reboot costs nothing.
#
# make_windows_vm.sh stands on its own and every phase is re-runnable, so
# answering no here costs nothing but running it later by hand. It works out
# which phases are already done and picks up from there, which is also how it
# continues after the reboot it asks for.
if [ -x "$dotfiles/make_windows_vm.sh" ]; then
	read -p "Set up the Windows 11 virtual machine? [yN] " -r user_answer
	if [[ "$user_answer" =~ ^[Yy]$ ]]; then
		"$dotfiles/make_windows_vm.sh"
	fi
fi
