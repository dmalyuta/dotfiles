# shellcheck shell=bash
# shellcheck disable=SC2154 # shares variables with start_fresh and its other libraries
#
# start_fresh's apps from upstream rather than from a package repository, most installed to
# ~/.local.

# Download $1 to $2 with mode $3 (default 644), replacing $2 only once the download is complete.
fetch() {
	local tmp rc
	tmp=$(mktemp)
	curl -fsSL "$1" -o "$tmp" && install -Dm "${3:-644}" "$tmp" "$2"
	rc=$?
	rm -f "$tmp"
	return $rc
}

# The tag of the latest release of GitHub project $1 (owner/name), from where its "latest" page
# redirects to.
latest_release_tag() {
	local url
	url=$(curl -fsSLI -o /dev/null -w '%{url_effective}' "https://github.com/$1/releases/latest")
	echo "${url##*/}"
}

# Download URLs of the assets matching regex $2 in the latest release of OpenRGB's repo $1.
openrgb_release_assets() {
	curl -fsSL "https://codeberg.org/api/v1/repos/OpenRGB/$1/releases/latest" |
		grep -oE '"browser_download_url":"[^"]+"' | cut -d'"' -f4 | grep -E "$2"
}

# OpenRGB from its latest upstream release, with its own hidapi-hotplug library, in place of
# Fedora's older packages, which can't read the profiles in config/OpenRGB and are only the
# fallback. A Fedora upgrade puts Fedora's back, which the next run swaps out again.
install_openrgb() {
	local dir url hidapi_url ver have hidapi_rpm actions
	dir=$(mktemp -d)
	# An .rpm per Fedora version, like openrgb_1.1_x86_64_f44_8e16de2.rpm: take this version's, or
	# the latest one before it.
	url=$(openrgb_release_assets OpenRGB '_x86_64_f[0-9]+_[0-9a-f]+\.rpm$' |
		awk -v fedora="$fedora" 'match($0, /_f[0-9]+_[0-9a-f]+\.rpm$/) {
			f = substr($0, RSTART + 2) + 0
			if (f <= fedora && f > best) { best = f; url = $0 }
		} END { print url }')
	hidapi_url=$(openrgb_release_assets hidapi-hotplug '/hidapi-hotplug-[0-9][^/]*\.x86_64\.rpm$' |
		head -n 1)
	# The file is named for version 1.1, say, and the package inside is 1.1.0. hidapi-hotplug's
	# file is named for its exact package.
	ver=$(sed -nE 's|.*/openrgb_([0-9.]+)_.*|\1|p' <<<"$url")
	have=$(rpm -q --qf '%{VERSION}' openrgb 2>/dev/null)
	hidapi_rpm=${hidapi_url##*/}
	actions=(--action=install "$dir"/{openrgb,hidapi-hotplug}.rpm)
	installed openrgb-udev-rules && actions+=(--action=remove openrgb-udev-rules)
	if [ -n "$ver" ] && { [ "$have" = "$ver" ] || [ "$have" = "$ver.0" ]; } &&
		installed "${hidapi_rpm%.x86_64.rpm}" && ! installed openrgb-udev-rules; then
		: # Up to date.
	elif [ -n "$url" ] && [ -n "$hidapi_url" ] &&
		curl -fsSL -o "$dir"/openrgb.rpm "$url" &&
		curl -fsSL -o "$dir"/hidapi-hotplug.rpm "$hidapi_url" &&
		sudo dnf "do" -y --skip-unavailable "${actions[@]}"; then
		say "Installed OpenRGB $(rpm -q --qf '%{VERSION}' openrgb) from upstream."
	else
		failed "Could not install upstream OpenRGB, falling back to Fedora's older one."
		installed openrgb || sudo dnf install -y openrgb
	fi
	rm -rf "$dir"
}

# Superluminal: a plain tarball, unpacked user-owned so its updater can write.
superluminal_dir=~/.local/superluminal
install_superluminal() {
	local url
	url=$(curl -fsSL https://superluminal.eu/download/ |
		grep -oE 'https://[^"]*/SuperluminalLinux-[^"]*\.tar\.gz' | head -n 1)
	if [ -z "$url" ]; then
		failed "Could not find the Superluminal download link, skipping it."
		return
	fi
	mkdir -p "$superluminal_dir" ~/.local/share/icons/hicolor/scalable/apps
	# Entries are ./Superluminal/..., so strip both components.
	curl -fsSL "$url" | tar xz -C "$superluminal_dir" --strip-components=2
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
}

# Oh My Posh, which updates itself with `oh-my-posh upgrade`.
install_oh_my_posh() {
	[ -x ~/.local/bin/oh-my-posh ] ||
		fetch https://github.com/JanDeDobbeleer/oh-my-posh/releases/latest/download/posh-linux-amd64 \
			~/.local/bin/oh-my-posh 755
}

# Bazelisk, which runs the Bazel version that a project asks for. Also as `bazel`.
install_bazelisk() {
	[ -x ~/.local/bin/bazelisk ] && return
	fetch https://github.com/bazelbuild/bazelisk/releases/latest/download/bazelisk-linux-amd64 \
		~/.local/bin/bazelisk 755 && ln -sf bazelisk ~/.local/bin/bazel
}

install_sn_pro_font() {
	local dir=~/.local/share/fonts/SN-Pro
	compgen -G "$dir/*.otf" >/dev/null && return
	mkdir -p "$dir"
	curl -fsSL -o sn-pro.zip https://github.com/supernotes/sn-pro/releases/latest/download/SN-Pro.zip &&
		unzip -q -j -o sn-pro.zip '*.otf' -d "$dir" && fc-cache -f "$dir"
	rm -f sn-pro.zip
}

# PureRef, from the .rpm on its pay-what-you-want download page, which has no direct link.
install_pureref() {
	local rpm
	# shellcheck disable=SC2012 # ls -t picks the newest download
	until rpm=$(ls -t ./*[Pp]ure[Rr]ef*.rpm 2>/dev/null | head -n 1) && [ -n "$rpm" ]; do
		echo "Download PureRef's Linux .rpm from https://www.pureref.com/download.php into $downloads"
		read -p "Press ENTER to try again... " -r
	done
	sudo dnf install -y "$rpm" || failed "Could not install $rpm."
}

# PdfCraft, from the .rpm in its latest GitHub release. Re-running this upgrades it.
install_pdfcraft() {
	local tag ver
	tag=$(latest_release_tag storytold/pdfcraft)
	ver=${tag#v}
	if [[ ! $ver =~ ^[0-9][0-9.]*$ ]]; then
		failed "Could not find the latest PdfCraft release, skipping it."
	elif [ "$(rpm -q --qf '%{VERSION}' pdfcraft 2>/dev/null)" != "$ver" ]; then
		sudo dnf install -y "https://github.com/storytold/pdfcraft/releases/download/$tag/pdfcraft-$ver-linux-x86_64.rpm" ||
			failed "Could not install PdfCraft."
	fi
}

# RAD Debugger, built from its latest release, like the AUR package does.
build_rad_debugger() {
	local src
	src=$(mktemp -d)
	if git -c advice.detachedHead=false clone -q --depth 1 \
		-b "$(latest_release_tag EpicGames/raddebugger)" \
		https://github.com/EpicGames/raddebugger.git "$src" &&
		(cd "$src" && ./build.sh clang release meta raddbg radbin radlink); then
		install -Dm755 "$src"/build/{raddbg,radbin,radlink} -t ~/.local/bin
		install -Dm644 "$src"/data/logo.png ~/.local/share/raddbg/logo.png
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
		failed "Could not build RAD Debugger, see the full output."
	fi
	rm -rf "$src"
}

# Anaconda, in batch mode, which accepts the license and doesn't touch ~/.bashrc: .local.bashrc
# sets conda up instead, since `conda init`'s block delays every prompt by ~1 s.
install_anaconda() {
	if [ ! -d ~/anaconda3 ]; then
		curl -fL -o anaconda.sh \
			"https://repo.anaconda.com/archive/Anaconda3-2026.07-1-Linux-x86_64.sh" &&
			bash anaconda.sh -b -p ~/anaconda3
		rm -f anaconda.sh
	fi
	# Remove that block if an earlier install added it.
	sed -i '/^# >>> conda initialize >>>$/,/^# <<< conda initialize <<<$/d' ~/.bashrc
}
