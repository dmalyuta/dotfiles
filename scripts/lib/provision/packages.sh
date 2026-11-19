# shellcheck shell=bash
# shellcheck disable=SC2034,SC2154 # shares variables with provision and its other libraries
#
# provision's package steps: software repositories, kernel module signing, the dnf and Flatpak
# packages for this machine (listed in packages.toml), and the kernel modules built for them.

# Enable COPR repo $1 (owner/project) if it isn't yet.
copr() {
	compgen -G "/etc/yum.repos.d/_copr*:${1/\//:}.repo" >/dev/null || sudo dnf copr enable -y "$1"
}

# packages.toml's [held] table: the packages that dnf only updates when asked, by name, each with
# the version it is held at. See read_held_packages, ask_held_upgrades and hold_packages.
declare -A held_packages=()
packages_toml=$dotfiles/packages.toml
versionlock=/etc/dnf/versionlock.toml

# Fill held_packages from packages.toml, once. Before the questions, which come before
# choose_packages reads the rest of the file.
read_held_packages() {
	local pair list
	[ -z "${held_packages_read:-}" ] || return 0
	[ -x /usr/bin/python3 ] || sudo dnf install -y -q python3 </dev/null
	list=$(/usr/bin/python3 -I "$dotfiles"/scripts/lib/provision/packages.py \
		"$packages_toml" held) || exit
	for pair in $list; do
		held_packages[${pair%%=*}]=${pair#*=}
	done
	held_packages_read=1
}

# Offer each held package's newest version in its repo, if newer than the held one. Asked every run
# while there is one, even with the last answers reused. Before saying yes, review the release. A
# yes writes the new version into packages.toml, as a change to commit. Packages not installed yet
# are left out: they install at the version in packages.toml.
ask_held_upgrades() {
	local name ver newest a
	read_held_packages
	for name in "${!held_packages[@]}"; do
		installed "$name" || continue
		ver=${held_packages[$name]}
		# repoquery ignores versionlock, so it sees the versions that are held back.
		newest=$(dnf -q repoquery --arch="$(uname -m)" --latest-limit=1 --qf '%{version}\n' \
			"$name" </dev/null 2>/dev/null | tail -n 1)
		[ -n "$newest" ] && [ "$newest" != "$ver" ] &&
			[ "$(printf '%s\n' "$ver" "$newest" | sort -V | tail -n 1)" = "$newest" ] || continue
		read -p "$name $newest is out, held at $ver. Upgrade to it? Review it first. [yN] " -r a
		[[ $a =~ ^[Yy]$ ]] || continue
		# Only the line in [held], up to the next table.
		sed -i -E "/^\[held\]/,/^\[/ s/^($name = \")[^\"]*\"/\1$newest\"/" "$packages_toml"
		held_packages[$name]=$newest
		say "Holding $name at $newest now. Commit the change to packages.toml."
	done
}

# Lock each of held_packages to its version, any build of that (as the release, like 1.fc45,
# changes with Fedora), and move it there if it is installed at another. Before the upgrade, which
# would otherwise take the newest. Other entries in dnf's versionlock file stay as they are.
hold_packages() {
	local name ver next comment
	read_held_packages
	for name in "${!held_packages[@]}"; do
		ver=${held_packages[$name]}
		next=${ver%.*}.$((${ver##*.} + 1))
		comment="held by provision at $ver, see packages.toml"
		if ! grep -qF "comment = \"$name $comment\"" "$versionlock" 2>/dev/null; then
			grep -qF "name = \"$name\"" "$versionlock" 2>/dev/null &&
				sudo dnf versionlock delete "$name" >/dev/null
			[ -s "$versionlock" ] || echo 'version = "1.0"' | sudo tee "$versionlock" >/dev/null
			sudo tee -a "$versionlock" >/dev/null <<EOF

[[packages]]
name = "$name"
comment = "$name $comment"
[[packages.conditions]]
key = "evr"
comparator = ">="
value = "$ver"
[[packages.conditions]]
key = "evr"
comparator = "<"
value = "$next"
EOF
		fi
		if installed "$name" && [ "$(rpm -q --qf '%{VERSION}' "$name")" != "$ver" ]; then
			sudo dnf distro-sync -y "$name" ||
				failed "Could not move $name to $ver, where it is held."
		fi
	done
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

	# COPRs: FSearch and Proton Drive for Linux from their authors, and acpi_call for
	# scripts/bin/asus-power-limits.
	copr cboxdoerfer/fsearch
	copr narl-rs/proton-drive-linux
	[ -n "${want_asusctl:-}" ] && copr rhea/acpi_call
	hold_packages

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
	# The held packages, on every machine, at their version as hold_packages locked it.
	read_held_packages
	dnf_apps+=("${!held_packages[@]}")
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
	# A held version gone from its repo is skipped above. COPR deletes old builds some days after a
	# newer one.
	for p in "${!held_packages[@]}"; do
		installed "$p" || failed "Could not install $p ${held_packages[$p]}, where it is held:" \
			"its repo may have dropped that version. Review a newer one to hold instead."
	done
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
		# Without the line that says the modules are already there, when nothing is built.
		sudo akmods --kernels "$newest_kernel" 2>&1 |
			grep --line-buffered -v '^Checking kmods exist for .*OK'
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
	local nvidia a remote out plan rc missing=() refs=()
	sudo flatpak remote-add --if-not-exists flathub https://dl.flathub.org/repo/flathub.flatpakrepo
	# Fedora's own remote, which Fedora adds, while nothing is installed from it: everything here
	# is from Flathub. The update check below and flatpak update fetch every enabled remote, and
	# when registry.fedoraproject.org is slow they wait minutes on it, silently.
	if flatpak remotes --system --columns=name,options | grep -qE '^fedora\s+oci$' &&
		! flatpak list --system --columns=origin | grep -qx fedora; then
		sudo flatpak remote-modify --system --disable fedora
	fi
	nvidia=$(nvidia_version)
	[ -n "$nvidia" ] && flatpak_apps+=("org.freedesktop.Platform.GL.nvidia-${nvidia//./-}")
	# Only those not installed yet, else flatpak lists each one that is.
	for a in "${flatpak_apps[@]}"; do
		flatpak info --system "$a" >/dev/null 2>&1 || missing+=("$a")
	done
	[ ${#missing[@]} -eq 0 ] || sudo flatpak install -y --noninteractive flathub "${missing[@]}"
	# Install leaves installed apps as they are, so update them and their runtimes too. Each remote
	# is asked in turn, given 15 seconds, as flatpak has no timeout of its own and waits minutes on
	# one that doesn't answer. Then only that remote's refs are updated: flatpak update without refs
	# would also fetch every remote's app catalog, the one that didn't answer too. The question is
	# flatpak update itself, unprivileged, which with no input shows its plan and declines it. It
	# isn't remote-ls --updates, which only lists newer commits: a runtime whose extension is missing,
	# such as GNOME 50's codecs-extra for 25.08, has none, while Discover offers it as an update.
	while read -r remote; do
		mapfile -t out < <(flatpak list --system --columns=ref,origin | awk -v r="$remote" '$2 == r { print $1 }')
		[ ${#out[@]} -gt 0 ] || continue
		# Declining exits 1, so the plan is looked for first.
		plan=$(LC_ALL=C timeout 15 flatpak update --system "${out[@]}" </dev/null 2>/dev/null)
		rc=$?
		if [[ $plan == *'Proceed with these changes'* ]]; then
			refs+=("${out[@]}")
		elif [ $rc -eq 124 ]; then
			failed "Flatpak remote $remote didn't answer in 15 seconds, so its apps weren't updated."
		elif [ $rc -ne 0 ]; then
			failed "Could not check Flatpak remote $remote for updates, so its apps weren't updated."
		fi
	done < <(flatpak remotes --system --columns=name,options | awk '$2 !~ /disabled/ { print $1 }')
	[ ${#refs[@]} -eq 0 ] || sudo flatpak update -y --noninteractive "${refs[@]}"
	# What flatpak update without refs also does: remove the runtimes that nothing uses any more.
	sudo flatpak uninstall -y --noninteractive --unused >/dev/null
}
