# shellcheck shell=bash
# shellcheck disable=SC2034,SC2154 # shares variables with provision and its other libraries
#
# provision's package steps: software repositories, kernel module signing, the dnf and Flatpak
# packages for this machine (listed in packages.toml), and the kernel modules built for them.

# Enable COPR repo $1 (owner/project) if it isn't yet.
copr() {
	compgen -G "/etc/yum.repos.d/_copr*:${1/\//:}.repo" >/dev/null || sudo dnf copr enable -y "$1"
}

add_repositories() {
	local repo
	# RPM Fusion, for the NVIDIA driver and the codecs that Fedora leaves out.
	if ! installed rpmfusion-free-release || ! installed rpmfusion-nonfree-release; then
		sudo dnf install -y \
			"https://mirrors.rpmfusion.org/free/fedora/rpmfusion-free-release-$fedora.noarch.rpm" \
			"https://mirrors.rpmfusion.org/nonfree/fedora/rpmfusion-nonfree-release-$fedora.noarch.rpm"
	fi

	# Terra, for most of what Fedora doesn't have: cardwire, asusctl, CoolerControl, keyd, OpenRazer,
	# Polychromatic, gping and the Nerd Fonts. Its terra-release package adds the repo and its key.
	# shellcheck disable=SC2016 # $releasever is for dnf.
	installed terra-release ||
		sudo dnf install -y --nogpgcheck \
			--repofrompath 'terra,https://repos.fyralabs.com/terra$releasever' terra-release

	# COPRs: FSearch from its author, and acpi_call for scripts/bin/asus-power-limits.
	copr cboxdoerfer/fsearch
	[ -n "${want_asusctl:-}" ] && copr rhea/acpi_call

	# The vendors' own repos, see scripts/yum. Fedora has Grafana too, but years behind.
	for repo in "$dotfiles"/scripts/yum/*.repo; do
		install_script_file "yum/${repo##*/}" "/etc/yum.repos.d/${repo##*/}"
	done
}

# The key that signs the kernel modules built here, see set_up_module_signing.
akmods_cert=/etc/pki/akmods/certs/public_key.der

# Whether that key is enrolled in the firmware, or queued for enrollment on the next boot. Its
# directory is only readable by root.
mok_key_ok() {
	sudo test -f "$akmods_cert" &&
		! sudo mokutil --test-key "$akmods_cert" 2>/dev/null | grep -q 'not enrolled'
}

# With Secure Boot on, the kernel only loads modules signed by a key the firmware trusts. akmods
# (NVIDIA, OpenRazer) and DKMS (acpi_call) both sign with akmods' key, which mokutil queues: on the
# next boot, MOK Manager asks for the password to enroll it. Until then those modules don't load.
set_up_module_signing() {
	local hash_file
	sudo dnf install -y akmods kmodtool mokutil openssl
	sudo test -f "$akmods_cert" || sudo kmodgenca -a
	write_root_file /etc/dkms/framework.conf.d/akmods-key.conf <<EOF
# Sign DKMS modules with the akmods key, see provision.
mok_signing_key="/etc/pki/akmods/private/private_key.priv"
mok_certificate="$akmods_cert"
EOF
	if [ -n "${mok_hash:-}" ] && ! mok_key_ok; then
		hash_file=$(mktemp)
		echo "$mok_hash" >"$hash_file"
		sudo mokutil --import "$akmods_cert" --hash-file "$hash_file" && need_mok_enroll=1
		rm -f "$hash_file"
	fi
}

# Fill dnf_apps, flatpak_apps, services (enabled and started) and groups (the user joins them)
# from packages.toml, for this machine's hardware and the answers.
choose_packages() {
	local table key list flags=()
	# The file's tables, and whether each applies here.
	local -A applies=(
		["all"]=1
		["nvidia"]=$(has_nvidia && echo 1)
		["laptop"]=$(is_laptop && echo 1)
		["convertible"]=$(is_convertible && echo 1)
		["amd-sfh"]=$(has_amd_sfh && echo 1)
		["hybrid-gpu"]=${want_cardwire:-}
		["asus-laptop"]=${want_asusctl:-}
		["openrgb"]=${want_openrgb:-}
		["brother"]=${want_brother:-}
		["brother-install"]=$([ -n "${want_brother:-}" ] && ! installed brscan-skey && echo 1)
	)
	# Each of the file's lists, and the array it fills.
	local -A arrays=([dnf]=dnf_apps [flatpak]=flatpak_apps [services]=services [groups]=groups)
	for table in "${!applies[@]}"; do
		flags+=("$table=${applies[$table]}")
	done
	# Read with Fedora's own Python: its TOML parser needs 3.11, and nothing else here can read TOML.
	[ -x /usr/bin/python3 ] || sudo dnf install -y -q python3 </dev/null
	for key in "${!arrays[@]}"; do
		list=$(/usr/bin/python3 -I "$dotfiles"/scripts/lib/provision/packages.py \
			"$dotfiles"/packages.toml "$key" "${flags[@]}") || exit
		read -r -a "${arrays[$key]}" <<<"$list"
	done
}

install_packages() {
	local installed_groups p missing=()
	# The full FFmpeg and AMD's hardware video decoding, in place of Fedora's builds without the
	# patented codecs. --allowerasing swaps them for Fedora's. Before the rest, which may need them.
	installed ffmpeg || sudo dnf install -y --allowerasing ffmpeg
	installed mesa-va-drivers-freeworld || sudo dnf install -y --allowerasing mesa-va-drivers-freeworld
	# cardwire replaces switcheroo-control, which Fedora KDE ships, and the two conflict.
	[ -n "${want_cardwire:-}" ] && installed switcheroo-control &&
		sudo dnf swap -y switcheroo-control cardwire

	# Only those not installed yet, else dnf lists each one that is. A name may be a package, a
	# group (@...), a capability (nodejs) or another arch's package (glibc.i686). The group list
	# comes from root's dnf, which already trusts the repos' keys, and without stdin, so it can't
	# wait on a question.
	installed_groups=" $(sudo dnf -q group list --installed --hidden </dev/null 2>/dev/null |
		awk 'NR > 1 { printf "%s ", $1 }')"
	for p in "${dnf_apps[@]}"; do
		if [[ $p == @* ]]; then
			[[ $installed_groups == *" ${p#@} "* ]] || missing+=("$p")
		else
			installed "$p" || rpm -q --whatprovides "$p" >/dev/null 2>&1 || missing+=("$p")
		fi
	done
	# One package gone from a repo shouldn't hold up the rest. Any other problem fails the whole
	# install, and everything after needs it, so stop there.
	if [ ${#missing[@]} -gt 0 ] && ! sudo dnf install -y --skip-unavailable "${missing[@]}"; then
		say "Installing the packages failed, see above. Fix it and run this again." >&2
		exit 1
	fi
}

# Build the kernel modules for the newest kernel now, rather than on its first boot, which would
# hold up the desktop for minutes.
build_kernel_modules() {
	local newest_kernel acpi_call_ver
	newest_kernel=$(rpm -q kernel --qf '%{VERSION}-%{RELEASE}.%{ARCH}\n' | sort -V | tail -n 1)
	# acpi_call-dkms's uninstall scripts also run on an upgrade to a new build of the same version
	# (a Fedora upgrade), deleting the new source and its modules-load.d file, so DKMS builds
	# nothing. Reinstall it without scripts then: the autoinstall below builds it.
	if installed acpi_call-dkms; then
		acpi_call_ver=$(rpm -q --qf '%{VERSION}' acpi_call-dkms)
		if [ ! -f "/usr/src/acpi_call-$acpi_call_ver/dkms.conf" ]; then
			sudo dnf reinstall -y --setopt=tsflags=noscripts acpi_call-dkms
			[ -e "/var/lib/dkms/acpi_call/$acpi_call_ver/source" ] ||
				sudo dkms add -m acpi_call -v "$acpi_call_ver"
		fi
		write_root_file /etc/modules-load.d/acpi_call.conf <<<acpi_call
	fi
	if installed akmods || installed dkms; then
		installed "kernel-devel-$newest_kernel" || sudo dnf install -y "kernel-devel-$newest_kernel"
		installed dkms && sudo dkms autoinstall -k "$newest_kernel"
	fi
	if installed akmods; then
		sudo akmods --kernels "$newest_kernel"
		if has_nvidia && ! modinfo -k "$newest_kernel" nvidia >/dev/null 2>&1; then
			failed "The NVIDIA driver failed to build, see /var/cache/akmods/nvidia."
		fi
	fi
}

add_user_to_groups() {
	local g
	for g in "${groups[@]}"; do
		getent group "$g" >/dev/null || sudo groupadd --system "$g"
		sudo usermod -aG "$g" "$USER"
	done
}

# Flatpak apps, system-wide, plus the GL extension matching the NVIDIA driver for hardware
# rendering.
install_flatpak_apps() {
	local nvidia a missing=()
	sudo flatpak remote-add --if-not-exists flathub https://dl.flathub.org/repo/flathub.flatpakrepo
	nvidia=$(nvidia_version)
	[ -n "$nvidia" ] && flatpak_apps+=("org.freedesktop.Platform.GL.nvidia-${nvidia//./-}")
	# Only those not installed yet, else flatpak lists each one that is.
	for a in "${flatpak_apps[@]}"; do
		flatpak info --system "$a" >/dev/null 2>&1 || missing+=("$a")
	done
	[ ${#missing[@]} -eq 0 ] || sudo flatpak install -y --noninteractive flathub "${missing[@]}"
	# Install leaves installed apps as they are, so update them and their runtimes too.
	[ -z "$(flatpak remote-ls --system --updates 2>/dev/null)" ] ||
		sudo flatpak update -y --noninteractive
}
