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
this_script_dir=$(cd "$(dirname "${BASH_SOURCE[0]}")" >/dev/null 2>&1 && pwd)
if [ -f "$this_script_dir/home/.bash_aliases" ] && [ -d "$this_script_dir/.git" ]; then
	dotfiles=$this_script_dir
fi
scripts_dir=$dotfiles/scripts

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

# Install one of the files from scripts/ to where it belongs on the system.
# Returns 0 only when the content actually changed, so callers can skip the
# expensive follow-up work on a re-run.
install_device_file() {
	local name=$1 dest=$2 mode=${3:-644} src
	src=$scripts_dir/$name
	[ -f "$src" ] || die "missing $src"
	if sudo cmp -s "$src" "$dest" 2>/dev/null; then
		return 1
	fi
	sudo mkdir -p "$(dirname "$dest")"
	sudo install -m "$mode" "$src" "$dest" || die "could not install $dest"
	info "installed $dest"
}

# ---------------------------------------------------------------------------
# Install.
# ---------------------------------------------------------------------------

# Upgrade.
sudo apt update
sudo apt full-upgrade -y
sudo apt autoremove --purge -y

# The rest of the script downloads, clones and unzips things, so get those out
# of the way first: a minimal Ubuntu install has none of them guaranteed.
apt_install ca-certificates curl wget git unzip

# System monitoring.
apt_install bat btop htop
ln -s /usr/bin/batcat ~/.local/bin/bat

# Navigation in the command line.
apt_install tree

# Bash fuzzy finder.
apt_install fzf

# Password manager.
install_deb proton_pass.deb "https://proton.me/download/PassDesktop/linux/x64/ProtonPass.deb"

# Note taking.
install_deb obsidian_1.13.7_amd64.deb "https://github.com/obsidianmd/obsidian-releases/releases/download/v1.13.7/obsidian_1.13.7_amd64.deb"

# Brave browser.
apt_install curl
if pkg_installed brave-browser; then
	skip "Brave"
else
	curl -fsS https://dl.brave.com/install.sh | sh
fi

# VS Code editor.
apt_install shfmt
install_deb code.deb "https://code.visualstudio.com/sha/download?build=stable&os=linux-deb-x64"

# Superluminal profiler. The Linux build is a plain tarball with no desktop
# integration, so unpack it into ~/.local/superluminal (user-owned, so its auto
# updater can write there) and add a launcher and a .desktop file by hand. The
# download link carries the version, so read the current one off the download
# page. The binary finds its bundled libraries relative to its real path, so a
# plain symlink on PATH is enough.
superluminal_dir=~/.local/superluminal
if [ -x "$superluminal_dir"/Superluminal ]; then
	skip "Superluminal"
else
	read -p "Install Superluminal profiler? [yN] " -r user_answer
	if [[ "$user_answer" =~ ^[Yy]$ ]]; then
		superluminal_url=$(curl -fsSL https://superluminal.eu/download/ |
			grep -oE 'https://[^"]*/SuperluminalLinux-[^"]*\.tar\.gz' | head -n 1)
		superluminal_tar="$downloads"/superluminal.tar.gz
		if [ -z "$superluminal_url" ]; then
			echo "== Could not find the Superluminal Linux download link, skipping." >&2
		elif fetch "$superluminal_tar" "$superluminal_url"; then
			mkdir -p "$superluminal_dir" ~/.local/bin ~/.local/share/applications \
				~/.local/share/icons/hicolor/scalable/apps
			# Entries are ./Superluminal/..., so strip both "." and "Superluminal".
			tar xzf "$superluminal_tar" -C "$superluminal_dir" --strip-components=2
			cp "$superluminal_dir"/Documentation/Superluminal/assets/img/logo.svg \
				~/.local/share/icons/hicolor/scalable/apps/superluminal.svg
			ln -sf "$superluminal_dir"/Superluminal ~/.local/bin/superluminal
			cat >~/.local/share/applications/superluminal.desktop <<EOF
[Desktop Entry]
Type=Application
Name=Superluminal
Comment=CPU profiler
Exec=$superluminal_dir/Superluminal
Icon=$HOME/.local/share/icons/hicolor/scalable/apps/superluminal.svg
Terminal=false
Categories=Development;Profiling;
EOF
		fi
	fi
fi

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
		read -p "Press ENTER once it is added... " -r
	done
	git clone "$repo_ssh" "$dotfiles"
fi

# Dotfiles install. Every ln -sf and mkdir -p here is already idempotent.
# The files that belong directly in $HOME live in home/; everything else is
# kept under the path it lands on, such as config/kitty.
cd "$dotfiles" || exit
ln -sf "$dotfiles"/home/.bash_aliases ~
ln -sf "$dotfiles"/home/.local.bashrc ~
git submodule update --init --recursive
# bin/ is the one that lands under a different name than it has in the repo,
# so name the link explicitly; -n so that a re-run replaces the existing link
# rather than following it and making bin/bin underneath it.
ln -sfn "$dotfiles"/bin ~/.bin
mkdir -p ~/.config/kitty
ln -sf "$dotfiles"/home/.dircolors ~
ln -sf "$dotfiles"/config/kitty/kitty.conf ~/.config/kitty
ln -sf "$dotfiles"/config/kitty/default-kitty ~/.config/kitty
ln -sf "$dotfiles"/config/kitty/resize_split.py ~/.config/kitty
ln -sf "$dotfiles"/home/.wezterm.lua ~
ln -sf "$dotfiles"/home/.alacritty.toml ~
mkdir -p ~/.config/tmux-powerline/themes
ln -sf "$dotfiles"/home/.tmux.conf ~
ln -sf "$dotfiles"/config/tmux-powerline/config.sh \
	~/.config/tmux-powerline/config.sh
ln -sf "$dotfiles"/config/tmux-powerline/themes/danylo-theme.sh \
	~/.config/tmux-powerline/themes/danylo-theme.sh
ln -sf "$dotfiles"/config/.blue-owl-custom.omp.json ~/.blue-owl-custom.omp.json
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
		curl -fsSL https://apt.coolercontrol.org/setup.sh | sudo sh
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

# Download OpenRGB.
if [ -f ~/.local/share/applications/openrgb.desktop ]; then
	skip "OpenRGB"
else
	read -p "Install OpenRGB? [yN] " -r user_answer
	if [[ "$user_answer" =~ ^[Yy]$ ]]; then
		fetch openrgb.zip "https://gitlab.com/CalcProgrammer1/OpenRGB/-/jobs/artifacts/master/download?job=Linux%20amd64%20AppImage"
		unzip -o openrgb.zip -d openrgb
		# Copy the rules from the downloaded openrgb zip folder for the latest build
		sudo openrgb/OpenRGB-x86_64.AppImage --generate-udev-rules /etc/udev/rules.d/60-openrgb.rules
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
		ln -sf "$dotfiles"/config/OpenRGB/Configuration.json ~/.config/OpenRGB/
		ln -sf "$dotfiles"/config/OpenRGB/OpenRGB.json ~/.config/OpenRGB/
		ln -sf "$dotfiles"/config/OpenRGB/profiles/blue.json ~/.config/OpenRGB/profiles/
		ln -sf "$dotfiles"/config/OpenRGB/profiles/off.json ~/.config/OpenRGB/profiles/
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
# source ~/.bin/fzf-tab-completion/bash/fzf-bash-completion.sh
# bind -x '"\t": fzf_bash_completion'

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
		read -p "Press ENTER to try again... " -r
	done
	sudo apt install -y ./PureRef-2.1.3_x64.deb
fi

# Calculators.
snap_install qalculate
apt_install qalc
install_deb speedcrunch.deb "https://bitbucket.org/heldercorreia/speedcrunch/downloads/SpeedCrunch-0.12-linux64.deb"

# Image viewer.
install_deb xnview.deb https://www.xnview.com/download.php?file=XnViewMP-linux-x64.deb

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
	sh <(wget -qO - https://downloads.nordcdn.com/apps/linux/install.sh) -n -p nordvpn-gui
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

# Flameshot screenshot.
if pkg_installed flameshot; then
	skip "flameshot"
else
	fetch flameshot.zip "https://github.com/flameshot-org/flameshot/releases/download/v14.0.0/flameshot-v14.0+git0.da6121bd-artifact-ubuntu-24.04-amd64.zip"
	unzip -o flameshot.zip -d flameshot
	sudo apt install -y ./flameshot/flameshot-14.0.0-1.ubuntu-24.04.amd64.deb

	# On Wayland, Flameshot's clipboard copy silently fails: it logs "Capture saved
	# to clipboard" but nothing ever reaches the compositor, because the capture
	# window is torn down before the compositor asks for the data. Upstream works
	# around this on Gnome only, by keeping that window alive until the data is
	# fetched, so on KDE the copy is simply lost. Running the daemon under XWayland
	# sidesteps it. The capture still goes through the desktop portal, so the
	# screenshots are identical either way, and on an X11 session this is the
	# platform Qt picks anyway, so setting it there changes nothing.
	#
	# Only the daemon matters here. The `flameshot gui` client that the screenshot
	# hotkey runs just sends the daemon a D-Bus message, so it needs no override --
	# but the daemon gets started two different ways, and both do: the autostart
	# entry at login, and D-Bus activation when something asks for
	# org.flameshot.Flameshot while no daemon is running.
	flameshot_autostart=~/.config/autostart/Flameshot.desktop
	if [ ! -f "$flameshot_autostart" ]; then
		# Flameshot writes this file itself the first time it runs with "launch at
		# startup" enabled, which has not happened yet on a fresh machine.
		mkdir -p "$(dirname "$flameshot_autostart")"
		cat >"$flameshot_autostart" <<'EOF'
	[Desktop Entry]
	Name=flameshot
	Icon=flameshot
	Exec=flameshot
	Terminal=false
	Type=Application
	X-GNOME-Autostart-enabled=true
EOF
	fi
	set_desktop_key "$flameshot_autostart" Exec "env QT_QPA_PLATFORM=xcb flameshot"

	# Shadows /usr/share/dbus-1/services/org.flameshot.Flameshot.service, which
	# activates the daemon with a bare `Exec=/usr/bin/flameshot`. Files in the home
	# directory win over the ones in /usr/share.
	mkdir -p ~/.local/share/dbus-1/services
	cat >~/.local/share/dbus-1/services/org.flameshot.Flameshot.service <<'EOF'
	[D-BUS Service]
	Name=org.flameshot.Flameshot
	Exec=/usr/bin/env QT_QPA_PLATFORM=xcb /usr/bin/flameshot
EOF
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
		read -p "Press ENTER to try again... " -r
	done
	wine msiexec /i EditorV11.x64.msi
fi

# Okular PDF viewer.
apt_install okular

# Wake sources, and coming back from suspend with a working display. Split out
# because it is a self-contained piece of system configuration with a long
# explanation attached, and because it is worth being able to re-run on its own.
if install_device_file 90-usb-wakeup.rules /etc/udev/rules.d/90-usb-wakeup.rules; then
	sudo udevadm control --reload
	# --reload only reloads the rule files; it does not reapply them to
	# devices that are already present, so trigger those explicitly.
	sudo udevadm trigger --action=add --subsystem-match=usb
fi
# Belt and braces for the rule above: re-assert the wake sources immediately
# before sleep, which is immune to enumeration order, replugs, dock power
# cycling, OpenRazer daemon restarts and driver rebinds.
#
# /usr/lib/systemd/system-sleep, not /etc/systemd/system-sleep: systemd only
# scans the former (it is the single system-sleep path in the systemd-sleep
# binary, and the only one man:systemd-sleep documents). A hook dropped in /etc
# is silently never run.
install_device_file usb-wakeup /usr/lib/systemd/system-sleep/usb-wakeup 0755

# Remove apport "experience a crash" popups.
sudo sed -i 's/enabled=1/enabled=0/g' /etc/default/apport
if [[ ${XDG_CURRENT_DESKTOP,,} == *gnome* ]]; then
	sudo systemctl stop apport.service
fi

# Fix icons.
apps=~/.local/share/applications

set_desktop_key "$apps"/gnu_image_manipulation_program.desktop StartupWMClass "gimp"
set_desktop_key "$apps"/gnu_image_manipulation_program.desktop X-AppImage-Name "GNU Image Manipulation Program"

set_desktop_key "$apps"/nextcloud_desktop.desktop StartupWMClass "Nextcloud"
set_desktop_key "$apps"/nextcloud_desktop.desktop X-AppImage-Name "Nextcloud Desktop"

set_desktop_key "$apps"/openrgb.desktop StartupWMClass "AppRun.wrapped"
set_desktop_key "$apps"/openrgb.desktop X-AppImage-Name "OpenRGB"

set_desktop_key /usr/share/applications/matlab.desktop StartupWMClass "MATLAB R2026a Update 5"
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

# App launchers: slug, display name, command, the letter combined with
# Shift+Control+Alt below, and -- for KDE, which binds shortcuts to desktop
# entries rather than to a command line -- the desktop entry to launch and the
# action within it. "_launch" is the entry's own Exec; anything else names one
# of its [Desktop Action ...] groups, which is how Flameshot gets its capture
# mode instead of starting the daemon a second time. Gnome uses the command and
# ignores the last two fields.
app_launchers=(
	"flameshot|Flameshot|flameshot gui|p|org.flameshot.Flameshot.desktop|Capture"
	"speedcrunch|SpeedCrunch|speedcrunch|n|speedcrunch.desktop|_launch"
	"brave|Brave|brave-browser|b|brave-browser.desktop|_launch"
	"pureref|PureRef|PureRef|r|pureref.desktop|_launch"
	"obsidian|Obsidian|obsidian|o|md.obsidian.Obsidian.desktop|_launch"
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
