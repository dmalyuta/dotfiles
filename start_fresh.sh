#!/bin/bash
#
# Set up a fresh Fedora KDE Plasma install. On a new machine, run
#
#   curl -fsSL https://raw.githubusercontent.com/dmalyuta/dotfiles/fedora/start_fresh.sh | bash
#
# Safe to re-run: every step skips what is already done. With -c, reuse the answers from the last
# run instead of asking again (pass it through curl with `| bash -s -- -c`).
#
# This script installs and configures the software, including the NVIDIA driver. At the end it runs
# scripts/configure_hardware.sh for low-level hardware setup (GPUs, kernel, sleep and hibernation,
# laptop quirks) and scripts/configure_kde.sh for the desktop.
#
# Author: Danylo Malyuta, 2026.

repo_ssh=git@github.com:dmalyuta/dotfiles.git
branch=fedora
raw_base=https://raw.githubusercontent.com/dmalyuta/dotfiles/$branch

# --------------------------------------------------------------------------------------------------
# Bootstrap.
# --------------------------------------------------------------------------------------------------

# Under `curl ... | bash` stdin is the script itself, so `read` would eat it. Re-exec a downloaded
# copy with the terminal on stdin instead.
if [ ! -t 0 ] && [ -z "${DOTFILES_BOOTSTRAP:-}" ]; then
	self=$(mktemp)
	curl -fsSL "$raw_base/start_fresh.sh" -o "$self" </dev/tty || exit
	DOTFILES_BOOTSTRAP=$self exec bash "$self" "$@" </dev/tty
fi
[ -n "${DOTFILES_BOOTSTRAP:-}" ] && trap 'rm -f "$DOTFILES_BOOTSTRAP"' EXIT

use_cached=
while getopts c opt; do
	case $opt in
	c) use_cached=1 ;;
	*) echo "Usage: $0 [-c]" >&2 && exit 1 ;;
	esac
done

# Use this checkout if running from one, else clone to ~/Projects/dotfiles below.
mkdir -p ~/Projects
dotfiles=~/Projects/dotfiles
this_script_dir=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
if [ -f "$this_script_dir/home/.bash_aliases" ] && [ -e "$this_script_dir/.git" ]; then
	dotfiles=$this_script_dir
fi

downloads=~/Downloads
mkdir -p "$downloads" ~/.local/bin ~/.local/share/applications
cd "$downloads" || exit
# Apps installed to ~/.local/bin below are run from here too.
export PATH=$HOME/.local/bin:$PATH

# --------------------------------------------------------------------------------------------------
# Helpers.
# --------------------------------------------------------------------------------------------------

# Shared helpers, see scripts/functions.sh. Before the repo is cloned, fetch them from GitHub.
if [ "$dotfiles" = "$this_script_dir" ]; then
	. "$dotfiles"/scripts/functions.sh
else
	functions=$(curl -fsSL "$raw_base/scripts/functions.sh") || exit
	eval "$functions"
fi

# Yes/no question whose default is the last answer, remembered under key $1. With -c, a remembered
# answer is used without asking.
ask() {
	local key=$1 hint="[yN]" a
	if [ -n "$use_cached" ] && [ -n "${answers[$key]:-}" ]; then
		[ "${answers[$key]}" = y ]
		return
	fi
	[ "${answers[$key]:-}" = y ] && hint="[Yn]"
	read -p "$2 $hint " -r a
	[[ ${a:-${answers[$key]:-n}} =~ ^[Yy]$ ]] && answers[$key]=y || answers[$key]=n
	[ "${answers[$key]}" = y ]
}

# Whether $1 lists VS Code workspaces as "<letter>:<absolute path>,..." (or is empty), with distinct
# letters.
valid_workspaces() {
	local entry="[A-Z]:/[^,;]*[^,;/]"
	[[ $1 =~ ^($entry(,$entry)*)?$ ]] &&
		[ -z "$(tr , '\n' <<<"$1" | cut -c1 | sort | uniq -d)" ]
}

# Download $1 to $2 with mode $3 (default 644), replacing $2 only once the download is complete.
fetch() {
	local tmp rc
	tmp=$(mktemp)
	curl -fsSL "$1" -o "$tmp" && install -Dm "${3:-644}" "$tmp" "$2"
	rc=$?
	rm -f "$tmp"
	return $rc
}

# Enable COPR repo $1 (owner/project) if it isn't yet.
copr() {
	compgen -G "/etc/yum.repos.d/_copr*:${1/\//:}.repo" >/dev/null || sudo dnf copr enable -y "$1"
}

# The key that signs the kernel modules built here, see "Kernel module signing" below.
akmods_cert=/etc/pki/akmods/certs/public_key.der

# Whether that key is enrolled in the firmware, or queued for enrollment on the next boot. Its
# directory is only readable by root.
mok_key_ok() {
	sudo test -f "$akmods_cert" &&
		! sudo mokutil --test-key "$akmods_cert" 2>/dev/null | grep -q 'not enrolled'
}

# --------------------------------------------------------------------------------------------------
# Questions, all up front so the rest runs unattended.
# --------------------------------------------------------------------------------------------------

# Last answers, kept in the repo (git-ignored). On a fresh install the repo is cloned later, so they
# are saved after that.
answers_file=$dotfiles/.start_fresh_answers
declare -A answers=()
[ -f "$answers_file" ] && . "$answers_file"

[ -n "$(git config --global user.name)" ] ||
	{ read -p "Git name: " -r a && git config --global user.name "$a"; }
[ -n "$(git config --global user.email)" ] ||
	{ read -p "Git email: " -r a && git config --global user.email "$a"; }

superluminal_dir=~/.local/superluminal
[ -x "$superluminal_dir"/Superluminal ] ||
	{ ask superluminal "Install Superluminal profiler?" && want_superluminal=1; }

if installed coolercontrol; then
	want_coolercontrol=1
elif ask coolercontrol "Install CoolerControl?"; then
	want_coolercontrol=1
	ask nct6775 "Try to find fans on desktop (nct6775)?" && want_nct6775=1
	if ask cc_backup "Restore CoolerControl settings from backup?"; then
		while true; do
			read -e -p "Path to the CoolerControl backup file: " -r cc_backup
			cc_backup="${cc_backup/#\~/$HOME}"
			[ -f "$cc_backup" ] && break
			echo "$cc_backup not found, try again."
		done
	fi
fi

installed openrgb || ask openrgb "Install OpenRGB?" && want_openrgb=1
installed asusctl || ask asusctl "Install asusctl and ROG Control Center?" && want_asusctl=1
installed cardwire || ask cardwire "Install cardwire hybrid GPU manager?" && want_cardwire=1
installed mfcj805dwpdrv || ask brother "Install Brother printer driver?" && want_brother=1
ask obs "Install the custom OBS configuration?" && want_obs=1

pdfx_dir=~/.wine/drive_c/"Program Files/PDF-XChange"
[ -d "$pdfx_dir" ] || { ask pdfx "Install PDF-XChange Editor?" && want_pdfx=1; }
[ -x ~/.local/bin/PureRef ] || { ask pureref "Install PureRef?" && want_pureref=1; }

# Kept out of the repo, since the projects may be private. The last answer, or the rejected one on a
# retry, is pre-filled for editing.
if [ -z "$use_cached" ] || ! valid_workspaces "${answers[workspaces]-x}"; then
	echo "VS Code workspaces for Meta+C, Meta+<letter>, as <letter>:<absolute path>,... (empty for"
	echo "none). Meta+<letter> is taken from whatever else uses it."
	workspaces=${answers[workspaces]:-}
	while true; do
		read -e -i "$workspaces" -p "Workspaces: " -r workspaces || exit
		valid_workspaces "$workspaces" && break
		echo "Invalid: expected e.g. M:/home/me/project,X:/home/me/other, with distinct letters and"
		echo "no trailing slashes."
	done
	answers[workspaces]=$workspaces
fi

# A one-time password for enrolling the module signing key, see "Kernel module signing" below.
# Asked every run until the key is enrolled, and only its hash is kept, for this run only.
if secure_boot && ! mok_key_ok; then
	echo "Secure Boot is on, so the NVIDIA driver needs a signing key that the firmware trusts. Pick"
	echo "a one-time password for adding it: the next boot asks for it once."
	while true; do
		read -s -p "Key password: " -r mok_password && echo
		read -s -p "Again: " -r a && echo
		[ -n "$mok_password" ] && [ "$mok_password" = "$a" ] && break
		echo "Empty or different, try again."
	done
	mok_hash=$(mokutil --generate-hash="$mok_password")
	unset mok_password a
fi

# --------------------------------------------------------------------------------------------------
# Repositories.
# --------------------------------------------------------------------------------------------------

# RPM Fusion, for the NVIDIA driver and the codecs that Fedora leaves out.
fedora=$(rpm -E %fedora)
if ! installed rpmfusion-free-release || ! installed rpmfusion-nonfree-release; then
	sudo dnf install -y \
		"https://mirrors.rpmfusion.org/free/fedora/rpmfusion-free-release-$fedora.noarch.rpm" \
		"https://mirrors.rpmfusion.org/nonfree/fedora/rpmfusion-nonfree-release-$fedora.noarch.rpm"
fi

# Terra, for most of what Fedora doesn't have: cardwire, asusctl, CoolerControl, keyd, OpenRazer,
# Polychromatic, gping and the Nerd Fonts. Its terra-release package adds the repo and its key.
# shellcheck disable=SC2016 # $releasever is for dnf.
installed terra-release ||
	sudo dnf install -y --nogpgcheck --repofrompath 'terra,https://repos.fyralabs.com/terra$releasever' \
		terra-release

# COPRs: FSearch from its author, and acpi_call for scripts/asus-power-limits.
copr cboxdoerfer/fsearch
[ -n "${want_asusctl:-}" ] && is_laptop && copr rhea/acpi_call

# The vendors' own repos. Fedora has Grafana too, but years behind.
write_root_file /etc/yum.repos.d/brave-browser.repo <<'EOF'
[brave-browser]
name=Brave Browser
baseurl=https://brave-browser-rpm-release.s3.brave.com/$basearch
enabled=1
gpgcheck=1
gpgkey=https://brave-browser-rpm-release.s3.brave.com/brave-core.asc
EOF
write_root_file /etc/yum.repos.d/vscode.repo <<'EOF'
[code]
name=Visual Studio Code
baseurl=https://packages.microsoft.com/yumrepos/vscode
enabled=1
autorefresh=1
type=rpm-md
gpgcheck=1
gpgkey=https://packages.microsoft.com/keys/microsoft.asc
EOF
write_root_file /etc/yum.repos.d/nordvpn.repo <<'EOF'
[nordvpn]
name=NordVPN
baseurl=https://repo.nordvpn.com/yum/nordvpn/centos/$basearch
enabled=1
gpgcheck=1
gpgkey=https://repo.nordvpn.com/gpg/nordvpn_public.asc
EOF
write_root_file /etc/yum.repos.d/grafana.repo <<'EOF'
[grafana]
name=Grafana
baseurl=https://rpm.grafana.com
enabled=1
gpgcheck=1
repo_gpgcheck=1
gpgkey=https://rpm.grafana.com/gpg.key
EOF

# --------------------------------------------------------------------------------------------------
# Kernel module signing.
# --------------------------------------------------------------------------------------------------

# With Secure Boot on, the kernel only loads modules signed by a key the firmware trusts. akmods
# builds the NVIDIA and OpenRazer modules and signs them with its own key, and DKMS (acpi_call) is
# set to use the same one, so that one key is all the firmware needs. mokutil queues it, and on the
# next boot MOK Manager asks for the password to enroll it. Until then those modules don't load.
if secure_boot; then
	sudo dnf install -y akmods kmodtool mokutil openssl
	sudo test -f "$akmods_cert" || sudo kmodgenca -a
	write_root_file /etc/dkms/framework.conf.d/akmods-key.conf <<EOF
# Sign DKMS modules with the akmods key, see start_fresh.sh.
mok_signing_key="/etc/pki/akmods/private/private_key.priv"
mok_certificate="$akmods_cert"
EOF
	if [ -n "${mok_hash:-}" ] && ! mok_key_ok; then
		hash_file=$(mktemp)
		echo "$mok_hash" >"$hash_file"
		sudo mokutil --import "$akmods_cert" --hash-file "$hash_file" && need_mok_enroll=1
		rm -f "$hash_file"
	fi
fi

# --------------------------------------------------------------------------------------------------
# Packages.
# --------------------------------------------------------------------------------------------------

dnf_apps=(
	@c-development @development-tools git curl wget unzip openssh-clients
	bat btop htop tree fzf fd-find ripgrep shfmt gping sqlite
	kitty tmux cascadiacode-nerd-fonts
	brave-browser code okular inkscape obs-studio gimp
	vlc vlc-plugins-all vlc-plugins-freeworld flameshot nextcloud-client
	qalculate-gtk speedcrunch fsearch
	texlive-scheme-full texlive-latexindent perl-YAML-Tiny perl-File-HomeDir
	openrazer-meta akmod-openrazer polychromatic keyd
	flatpak rustup nodejs npm grafana wine winetricks nordvpn nordvpn-gui
	# To build RAD Debugger, below.
	clang lld llvm libX11-devel libXext-devel libXfixes-devel mesa-libGL-devel mesa-libEGL-devel
	freetype-devel
)
flatpak_apps=(
	com.github.tchx84.Flatseal
	io.github.tanaybhomia.Whisp
	io.github.amit9838.mousam
	md.obsidian.Obsidian
	me.proton.Pass
	com.xnview.XnViewMP
)

groups=(plugdev nordvpn)
services=(grafana-server nordvpnd keyd)

# The NVIDIA driver, as an akmod: akmods builds it for each new kernel. The CUDA package also has
# nvidia-smi.
has_nvidia && dnf_apps+=(akmod-nvidia xorg-x11-drv-nvidia-cuda)
[ -n "${want_coolercontrol:-}" ] && dnf_apps+=(coolercontrol) && services+=(coolercontrold)
# The PX13 folds into a tablet, which needs an on-screen keyboard.
is_laptop && dnf_apps+=(plasma-keyboard)
[ -n "${want_openrgb:-}" ] && dnf_apps+=(openrgb i2c-tools) && groups+=(i2c)
[ -n "${want_asusctl:-}" ] && dnf_apps+=(asusctl asusctl-rog-gui) && services+=(asusd)
# For scripts/asus-power-limits.
[ -n "${want_asusctl:-}" ] && is_laptop && dnf_apps+=(acpi_call-dkms)
[ -n "${want_cardwire:-}" ] && dnf_apps+=(cardwire cardwire-gui) && services+=(cardwired)
if [ -n "${want_brother:-}" ]; then
	dnf_apps+=(cups) && services+=(cups)
	installed mfcj805dwpdrv ||
		dnf_apps+=(https://download.brother.com/welcome/dlf104570/mfcj805dwpdrv-1.0.5-0.i386.rpm)
fi

sudo dnf upgrade -y --refresh
# The full FFmpeg and AMD's hardware video decoding, in place of Fedora's builds without the
# patented codecs. --allowerasing swaps them for Fedora's. Before the rest, which may need them.
installed ffmpeg || sudo dnf install -y --allowerasing ffmpeg
installed mesa-va-drivers-freeworld || sudo dnf install -y --allowerasing mesa-va-drivers-freeworld
# cardwire replaces switcheroo-control, which Fedora KDE ships, and the two conflict.
[ -n "${want_cardwire:-}" ] && installed switcheroo-control &&
	sudo dnf swap -y switcheroo-control cardwire
# One package gone from a repo shouldn't hold up the rest. Any other problem fails the whole
# install, and everything after needs it, so stop there.
if ! sudo dnf install -y --skip-unavailable "${dnf_apps[@]}"; then
	say "Installing the packages failed, see above. Fix it and run this again." >&2
	exit 1
fi

# Build the kernel modules (NVIDIA and OpenRazer with akmods, acpi_call with DKMS) for the newest
# kernel now, rather than on its first boot, which would hold up the desktop for minutes. The
# upgrade may have just installed that kernel, before the modules' own hooks were there.
newest_kernel=$(rpm -q kernel --qf '%{VERSION}-%{RELEASE}.%{ARCH}\n' | sort -V | tail -n 1)
if installed akmods || installed dkms; then
	installed "kernel-devel-$newest_kernel" || sudo dnf install -y "kernel-devel-$newest_kernel"
	installed dkms && sudo dkms autoinstall -k "$newest_kernel"
fi
if installed akmods; then
	sudo akmods --kernels "$newest_kernel"
	if has_nvidia && ! modinfo -k "$newest_kernel" nvidia >/dev/null 2>&1; then
		say "The NVIDIA driver failed to build, see /var/cache/akmods/nvidia." >&2
	fi
fi

for g in "${groups[@]}"; do
	getent group "$g" >/dev/null || sudo groupadd --system "$g"
	sudo usermod -aG "$g" "$USER"
done

# Rust toolchains, in ~/.cargo and ~/.rustup. rustup-init also adds ~/.cargo/bin to PATH in the
# shell profiles.
[ -x ~/.cargo/bin/rustup ] || rustup-init -y

# Flatpak apps, plus the GL extension matching the Nvidia driver so they get hardware rendering
# under Wayland.
# System-wide, which needs root.
sudo flatpak remote-add --if-not-exists flathub https://dl.flathub.org/repo/flathub.flatpakrepo
nvidia_version=$(nvidia_version)
[ -n "$nvidia_version" ] &&
	flatpak_apps+=("org.freedesktop.Platform.GL.nvidia-${nvidia_version//./-}")
sudo flatpak install -y --noninteractive flathub "${flatpak_apps[@]}"

# --------------------------------------------------------------------------------------------------
# Apps from upstream, installed to ~/.local.
# --------------------------------------------------------------------------------------------------

# Superluminal: a plain tarball, unpacked user-owned so its updater can write.
if [ -n "${want_superluminal:-}" ]; then
	superluminal_url=$(curl -fsSL https://superluminal.eu/download/ |
		grep -oE 'https://[^"]*/SuperluminalLinux-[^"]*\.tar\.gz' | head -n 1)
	if [ -z "$superluminal_url" ]; then
		say "Could not find the Superluminal download link, skipping." >&2
	else
		mkdir -p "$superluminal_dir" ~/.local/share/icons/hicolor/scalable/apps
		# Entries are ./Superluminal/..., so strip both components.
		curl -fsSL "$superluminal_url" | tar xz -C "$superluminal_dir" --strip-components=2
		cp "$superluminal_dir"/Documentation/Superluminal/assets/img/logo.svg \
			~/.local/share/icons/hicolor/scalable/apps/superluminal.svg
		ln -sf "$superluminal_dir"/Superluminal ~/.local/bin/superluminal
		cat >~/.local/share/applications/superluminal.desktop <<EOF
[Desktop Entry]
Type=Application
Name=Superluminal
Comment=CPU profiler
Exec=$superluminal_dir/Superluminal
Icon=superluminal
Terminal=false
Categories=Development;Profiling;
EOF
	fi
fi

# Oh My Posh, which updates itself with `oh-my-posh upgrade`.
[ -x ~/.local/bin/oh-my-posh ] ||
	fetch https://github.com/JanDeDobbeleer/oh-my-posh/releases/latest/download/posh-linux-amd64 \
		~/.local/bin/oh-my-posh 755

# Bazelisk, which runs the Bazel version that a project asks for. Also as `bazel`.
if [ ! -x ~/.local/bin/bazelisk ]; then
	fetch https://github.com/bazelbuild/bazelisk/releases/latest/download/bazelisk-linux-amd64 \
		~/.local/bin/bazelisk 755 && ln -sf bazelisk ~/.local/bin/bazel
fi

# SN Pro font.
sn_pro_dir=~/.local/share/fonts/SN-Pro
if ! compgen -G "$sn_pro_dir/*.otf" >/dev/null; then
	mkdir -p "$sn_pro_dir"
	curl -fsSL -o sn-pro.zip https://github.com/supernotes/sn-pro/releases/latest/download/SN-Pro.zip &&
		unzip -q -j -o sn-pro.zip '*.otf' -d "$sn_pro_dir" && fc-cache -f "$sn_pro_dir"
	rm -f sn-pro.zip
fi

# PureRef, from the .deb on its pay-what-you-want download page, which has no direct link. It
# holds a static binary, its desktop entry and icons.
if [ -n "${want_pureref:-}" ]; then
	until pureref_deb=$(ls -t ./*[Pp]ure[Rr]ef*.deb 2>/dev/null | head -n 1) && [ -n "$pureref_deb" ]; do
		echo "Download PureRef's Linux .deb from https://www.pureref.com/download.php into $downloads"
		read -p "Press ENTER to try again... " -r
	done
	pureref_tmp=$(mktemp -d)
	if ar x --output "$pureref_tmp" "$pureref_deb" &&
		tar xf "$pureref_tmp"/data.tar.* -C ~/.local --strip-components=2 ./usr; then
		set_desktop_key ~/.local/share/applications/pureref.desktop Exec "$HOME/.local/bin/PureRef %U"
	else
		say "Could not unpack $pureref_deb." >&2
	fi
	rm -rf "$pureref_tmp"
fi

# RAD Debugger, built from its latest release, like the AUR package does.
if [ ! -x ~/.local/bin/raddbg ]; then
	rad_tag=$(curl -fsSLI -o /dev/null -w '%{url_effective}' \
		https://github.com/EpicGames/raddebugger/releases/latest)
	rad_src=$(mktemp -d)
	if git -c advice.detachedHead=false clone -q --depth 1 -b "${rad_tag##*/}" https://github.com/EpicGames/raddebugger.git "$rad_src" &&
		(cd "$rad_src" && ./build.sh clang release meta raddbg radbin radlink); then
		install -Dm755 "$rad_src"/build/{raddbg,radbin,radlink} -t ~/.local/bin
		install -Dm644 "$rad_src"/data/logo.png ~/.local/share/raddbg/logo.png
		cat >~/.local/share/applications/raddbg.desktop <<EOF
[Desktop Entry]
Type=Application
Name=RAD Debugger
Comment=Native graphical debugger
Exec=$HOME/.local/bin/raddbg
Icon=$HOME/.local/share/raddbg/logo.png
Terminal=false
Categories=Development;Debugger;
EOF
	else
		say "Could not build RAD Debugger." >&2
	fi
	rm -rf "$rad_src"
fi

# --------------------------------------------------------------------------------------------------
# SSH and dotfiles.
# --------------------------------------------------------------------------------------------------

mkdir -p -m 700 ~/.ssh
[ -f ~/.ssh/id_ed25519 ] || ssh-keygen -q -t ed25519 -C "$(git config --global user.email)" -f ~/.ssh/id_ed25519 -N ""
eval "$(ssh-agent -s)"
ssh-add ~/.ssh/id_ed25519
ssh-keygen -F github.com >/dev/null 2>&1 || ssh-keyscan github.com >>~/.ssh/known_hosts 2>/dev/null

if [ -d "$dotfiles" ]; then
	skip "dotfiles repo"
else
	until ssh -T git@github.com </dev/null 2>&1 | grep -q 'successfully authenticated'; do
		echo -e "\nAdd this public key to https://github.com/settings/keys:\n"
		cat ~/.ssh/id_ed25519.pub
		read -p $'\nPress ENTER once it is added... ' -r
	done
	git clone -b "$branch" "$repo_ssh" "$dotfiles"
fi

git -C "$dotfiles" submodule update --init --recursive
declare -p answers >"$answers_file"
mkdir -p ~/.config/kitty ~/.config/tmux-powerline/themes
ln -sf "$dotfiles"/home/{.bash_aliases,.local.bashrc,.dircolors,.wezterm.lua,.alacritty.toml} ~
ln -sf "$dotfiles"/home/.tmux.conf ~
ln -sfn "$dotfiles"/bin ~/.bin
ln -sf "$dotfiles"/config/kitty/{kitty.conf,default-kitty,resize_split.py} ~/.config/kitty
ln -sf "$dotfiles"/config/tmux-powerline/config.sh ~/.config/tmux-powerline/
ln -sf "$dotfiles"/config/tmux-powerline/themes/danylo-theme.sh ~/.config/tmux-powerline/themes/
ln -sf "$dotfiles"/config/.blue-owl-custom.omp.json ~
echo 'kitty.desktop' >~/.config/xdg-terminals.list

[ -d ~/.tmux/plugins/tpm ] || git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm

if grep -qF 'oh-my-posh init bash' ~/.bashrc 2>/dev/null; then
	skip "bashrc block"
else
	cat >>~/.bashrc <<'EOF'

# Binaries.
export PATH=$PATH:~/.local/bin
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

# --------------------------------------------------------------------------------------------------
# Per-app configuration.
# --------------------------------------------------------------------------------------------------

if [ -n "${want_nct6775:-}" ]; then
	sudo modprobe nct6775
	echo nct6775 | write_root_file /etc/modules-load.d/nct6775.conf
fi
[ -n "${cc_backup:-}" ] && sudo tar -xvf "$cc_backup" -C /

if [ -n "${want_openrgb:-}" ]; then
	sudo modprobe -a i2c-dev i2c-piix4
	printf 'i2c-dev\ni2c-piix4\n' | write_root_file /etc/modules-load.d/i2c.conf
	mkdir -p ~/.config/OpenRGB/profiles ~/.config/OpenRGB/plugins
	ln -sf "$dotfiles"/config/OpenRGB/{Configuration.json,OpenRGB.json} ~/.config/OpenRGB/
	ln -sf "$dotfiles"/config/OpenRGB/profiles/{blue.json,off.json} ~/.config/OpenRGB/profiles/
fi

if [ -n "${want_obs:-}" ]; then
	# OBS profile and scene collection, see config/obs/. Linked by directory, since OBS saves a file
	# by renaming a new one over it, which would replace a linked file. Existing ones are kept as
	# *.orig.
	obs_basic=~/.config/obs-studio/basic
	mkdir -p "$obs_basic"
	for d in profiles scenes; do
		[ -d "$obs_basic/$d" ] && [ ! -L "$obs_basic/$d" ] && mv -T "$obs_basic/$d" "$obs_basic/$d.orig"
		ln -sfn "$dotfiles/config/obs/$d" "$obs_basic/$d"
	done
	# Use that profile and scene collection, else OBS makes empty "Untitled" ones in the repo.
	# FirstRun skips the first-run auto-configuration wizard, which would overwrite the profile.
	obs_user=~/.config/obs-studio/user.ini
	kwriteconfig6 --file "$obs_user" --group General --key FirstRun true
	kwriteconfig6 --file "$obs_user" --group Basic --key Profile desktop
	kwriteconfig6 --file "$obs_user" --group Basic --key ProfileDir desktop
	kwriteconfig6 --file "$obs_user" --group Basic --key SceneCollection desktop_monitor
	kwriteconfig6 --file "$obs_user" --group Basic --key SceneCollectionFile desktop_monitor.json
	# Echo-cancelled webcam mic, so calls played on the speakers don't leak into OBS's mic.
	echo_cancel=~/.config/pipewire/pipewire.conf.d/60-echo-cancel.conf
	if [ "$(readlink "$echo_cancel")" != "$dotfiles/config/obs/pipewire/60-echo-cancel.conf" ]; then
		mkdir -p "${echo_cancel%/*}"
		ln -sf "$dotfiles"/config/obs/pipewire/60-echo-cancel.conf "$echo_cancel"
		systemctl --user restart pipewire pipewire-pulse wireplumber
	fi
	# The webcam mic level that the scene's filters are tuned for. PipeWire takes a moment to bring
	# the device back after a restart.
	if [ -e /proc/asound/C920 ]; then
		for try in {1..20}; do
			pactl set-source-volume alsa_input.usb-046d_HD_Pro_Webcam_C920_035EF47F-02.analog-stereo 80% \
				2>/dev/null && break
			sleep 0.5
		done
	fi
fi

# Grafana needs to read /home. Do NOT chmod -R here.
chmod o+rx "$HOME"
if write_root_file /etc/systemd/system/grafana-server.service.d/override.conf <<'EOF'
[Service]
ProtectHome=false
EOF
then
	sudo systemctl daemon-reload
	sudo systemctl try-restart grafana-server
fi

# keyd remaps keys below the display server, see scripts/keyd.conf. A restart applies a new config.
install_script_file keyd.conf /etc/keyd/default.conf && sudo systemctl try-restart keyd

sudo systemctl enable --now "${services[@]}"

# Batch mode accepts the license and skips the prompts, including the final one that sets up
# ~/.bashrc. Good: .local.bashrc sets conda up instead, because the block `conda init` writes delays
# every prompt by ~1 s. Remove that block if it was added before.
if [ -d ~/anaconda3 ]; then
	skip "Anaconda"
else
	curl -fL -o anaconda.sh \
		"https://repo.anaconda.com/archive/Anaconda3-2026.07-1-Linux-x86_64.sh" &&
		bash anaconda.sh -b -p ~/anaconda3
fi
sed -i '/^# >>> conda initialize >>>$/,/^# <<< conda initialize <<<$/d' ~/.bashrc

[ -f ~/.wine/drive_c/windows/Fonts/arial.ttf ] || winetricks -q corefonts
if [ -n "${want_pdfx:-}" ]; then
	while [ ! -f EditorV11.x64.msi ]; do
		echo "Download PDF-XChange Editor Plus 64-bit MSI installer from"
		echo "https://www.pdf-xchange.com/product/downloads into $downloads"
		read -p "Press ENTER to try again... " -r
	done
	wine msiexec /i EditorV11.x64.msi
fi

# Ship our own top-level launcher that runs the exe directly, and hide Wine's. This makes sure
# that the icon can be pinned to the task manager etc.
if [ -x "$pdfx_dir/PDF Editor/PXCEditor.exe" ]; then
	pdfx_icon=application-pdf
	for f in ~/.local/share/icons/hicolor/48x48/apps/*_PXCEditor.0.png; do
		[ -e "$f" ] && pdfx_icon=$(basename "$f" .png) && break
	done
	cat >~/.local/share/applications/pdf-xchange-editor.desktop <<EOF
[Desktop Entry]
Type=Application
Name=PDF-XChange Editor
GenericName=PDF Editor
Comment=View and edit PDF documents
Exec=env WINEPREFIX=$HOME/.wine WINEDEBUG=fixme-all wine "$pdfx_dir/PDF Editor/PXCEditor.exe" %f
Icon=$pdfx_icon
Terminal=false
StartupNotify=true
StartupWMClass=pxceditor.exe
Categories=Office;Viewer;
MimeType=application/pdf;
EOF
	set_desktop_key ~/.local/share/applications/wine/Programs/PDF-XChange/"PDF-XChange Editor.desktop" \
		NoDisplay true
	update-desktop-database ~/.local/share/applications
	kbuildsycoca6 >/dev/null 2>&1
fi

# Silence Wine from logging the spammy "fixme:" channel to stderr. Keep "err:" and "warn:", so real
# faults get reported.
mkdir -p ~/.config/environment.d
cat >~/.config/environment.d/50-winedebug.conf <<'EOF'
# Silence Wine's "fixme:" chatter, which otherwise floods the systemd journal.
# Real problems still log: the err: and warn: channels are left enabled.
WINEDEBUG=fixme-all
EOF
# environment.d only applies to sessions started after it, so cover the launchers directly too.
while IFS= read -r -d '' f; do
	grep -q '^Exec=env .*wine' "$f" && ! grep -q WINEDEBUG "$f" &&
		sed -i 's|^Exec=env |Exec=env "WINEDEBUG=fixme-all" |' "$f"
done < <(find ~/.local/share/applications -name '*.desktop' -print0 2>/dev/null)

# Flameshot's Wayland clipboard copy is lost on non-Gnome desktops because the capture window closes
# too early. Run the daemon under XWayland instead, for both ways it gets started: autostart and
# D-Bus activation.
flameshot_autostart=~/.config/autostart/Flameshot.desktop
if [ ! -f "$flameshot_autostart" ]; then
	mkdir -p ~/.config/autostart
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
mkdir -p ~/.local/share/dbus-1/services
cat >~/.local/share/dbus-1/services/org.flameshot.Flameshot.service <<'EOF'
[D-BUS Service]
Name=org.flameshot.Flameshot
Exec=/usr/bin/env QT_QPA_PLATFORM=xcb /usr/bin/flameshot
EOF

# Explicitly set the journald size (default 50 MB).
if write_root_file /etc/systemd/journald.conf.d/99-size.conf <<'EOF'
[Journal]
SystemMaxUse=256M
EOF
then
	sudo systemctl restart systemd-journald
fi

# Low-level hardware setup (GPUs, kernel, sleep and hibernation, laptop quirks), see
# scripts/configure_hardware.sh.
bash "$dotfiles"/scripts/configure_hardware.sh
[ $? -eq 2 ] && need_reboot=1

# KDE desktop configuration, see scripts/configure_kde.sh.
bash "$dotfiles"/scripts/configure_kde.sh "${answers[workspaces]:-}"
[ $? -eq 2 ] && need_reboot=1

if [ -n "${need_mok_enroll:-}" ]; then
	need_reboot=1
	say "Next boot: enroll the module signing key, or the NVIDIA driver won't load."
	echo "A blue MOK Manager screen waits 10 s for a key press. Press any key, then pick"
	echo "Enroll MOK > Continue > Yes, type the password you picked, and pick Reboot. If you miss"
	echo "it, run this script again and reboot."
fi

if [ -z "${need_reboot:-}" ]; then
	say "Done."
else
	read -r -s -p "$(say "Done. Press ENTER to reboot so the changes take effect...")"
	echo
	sudo reboot
fi
