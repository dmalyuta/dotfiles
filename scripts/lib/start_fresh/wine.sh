# shellcheck shell=bash
# shellcheck disable=SC2034,SC2154 # shares variables with start_fresh and its other libraries
#
# start_fresh's Wine setup, and PDF-XChange Editor in it.

# Fedora's wine-mono is broken (its libmono needs MinGW DLLs from a folder Wine never searches), so
# .NET programs fail with "Could not load Mono". Install WineHQ's build into the prefix instead: the
# version this Wine asks for, checked against the hashes Wine carries.
install_wine_mono() {
	local appwiz msi ver
	appwiz=$(rpm -ql wine-core | grep -m1 'x86_64-windows/appwiz\.cpl$')
	msi=$(strings -el "$appwiz" | grep -m1 -xE 'wine-mono-[0-9.]+-x86\.msi')
	ver=${msi#wine-mono-} && ver=${ver%-x86.msi}
	[ -n "$ver" ] || return
	grep -A3 '"DisplayName"="Wine Mono Runtime"' ~/.wine/system.reg |
		grep -q "\"DisplayVersion\"=\"$ver\"" && return
	if curl -fsSL -o "$msi" "https://dl.winehq.org/wine/wine-mono/$ver/$msi" &&
		strings -n 64 "$appwiz" | grep -qx "$(sha256sum "$msi" | cut -d' ' -f1)"; then
		wine msiexec /i "$msi" /qn
	else
		failed "Couldn't download or verify $msi: .NET programs in Wine won't run."
	fi
	rm -f "$msi"
}

# Set a value in Wine's registry, under HKEY_CURRENT_USER, unless it is already set: each run of
# wine is slow. Args: key (like 'Control Panel\Desktop'), name, type (REG_SZ or REG_DWORD), value.
wine_reg() {
	local key=$1 name=$2 type=$3 value=$4 line
	case $type in
	REG_SZ) line="\"$name\"=\"$value\"" ;;
	REG_DWORD) line=$(printf '"%s"=dword:%08x' "$name" "$value") ;;
	esac
	# user.reg has the key as "[Control Panel\\Desktop] <time>", followed by its values.
	header="[${key//\\/\\\\}] " awk '/^\[/ { in_key = index($0, ENVIRON["header"]) == 1 } in_key' \
		~/.wine/user.reg 2>/dev/null | grep -qxF "$line" ||
		wine reg add "HKCU\\$key" /v "$name" /t "$type" /d "$value" /f >/dev/null
}

configure_wine() {
	# Font smoothing: ClearType on a desktop's RGB LCD, grayscale on the laptop's OLED, whose
	# subpixels would give ClearType color fringes.
	local smoothing_type=2
	is_laptop && smoothing_type=1
	wine_reg 'Control Panel\Desktop' FontSmoothing REG_SZ 2
	wine_reg 'Control Panel\Desktop' FontSmoothingType REG_DWORD "$smoothing_type"
	wine_reg 'Control Panel\Desktop' FontSmoothingOrientation REG_DWORD 1
	wine_reg 'Control Panel\Desktop' FontSmoothingGamma REG_DWORD 1400

	# Turn off Wine's USB and HID passthrough, which PDF-XChange doesn't need: after a sleep or
	# undocking, they spin a core at 100% (wineusb's hotplug thread, winebus re-reading gone hidraw
	# nodes). Their Start value is ignored, so disable them as DLLs.
	wine_reg 'Software\Wine\DllOverrides' wineusb.sys REG_SZ ''
	wine_reg 'Software\Wine\DllOverrides' winebus.sys REG_SZ ''
}

# Keep Wine's "fixme:" chatter out of the journal. err: and warn: still log.
quiet_wine_logs() {
	local f
	mkdir -p ~/.config/environment.d
	cat >~/.config/environment.d/50-winedebug.conf <<'EOF'
# Wine's "fixme:" chatter floods the journal. err: and warn: still log.
WINEDEBUG=fixme-all
EOF
	# environment.d only applies to sessions started after it, so cover Wine's launchers too.
	while IFS= read -r -d '' f; do
		grep -q '^Exec=env .*wine' "$f" && ! grep -q WINEDEBUG "$f" &&
			sed -i 's|^Exec=env |Exec=env "WINEDEBUG=fixme-all" |' "$f"
	done < <(find ~/.local/share/applications -name '*.desktop' -print0 2>/dev/null)
}

# The scanners that Wine programs see, through bin/pdf-xchange-editor's SANE_CONFIG_DIR: only the
# network backend, see config/sane-wine/dll.conf. The Brother is listed by name, since the
# system's blacklist (name_brother_devices) hides it from that backend's discovery. While the
# printer is off, its entry stays as it is.
set_up_wine_scanners() {
	mkdir -p ~/.config/sane-wine
	ln -sf "$dotfiles"/config/sane-wine/dll.conf ~/.config/sane-wine/
	if [ -z "${want_brother:-}" ]; then
		rm -f ~/.config/sane-wine/airscan.conf
	elif [ -n "${brother_model:-}" ]; then
		cat >~/.config/sane-wine/airscan.conf <<EOF
[devices]
"Brother $brother_model" = http://$brother_host:80/eSCL/, eSCL
EOF
	fi
}

# PDF-XChange Editor, from its installer, which has no direct download link.
pdfx_dir=~/.wine/drive_c/"Program Files/PDF-XChange"
install_pdf_xchange() {
	while [ ! -f EditorV11.x64.msi ]; do
		echo "Download PDF-XChange Editor Plus 64-bit MSI installer from"
		echo "https://www.pdf-xchange.com/product/downloads into $downloads"
		read -p "Press ENTER to try again... " -r
	done
	wine msiexec /i EditorV11.x64.msi
}

# Our own launcher for PDF-XChange Editor, which runs the exe directly so its icon can be pinned,
# with Wine's hidden. It goes through bin/pdf-xchange-editor, which turns the files' paths into
# Windows ones.
add_pdf_xchange_launcher() {
	local f icon=application-pdf
	for f in ~/.local/share/icons/hicolor/48x48/apps/*_PXCEditor.0.png; do
		[ -e "$f" ] && icon=$(basename "$f" .png) && break
	done
	cat >~/.local/share/applications/pdf-xchange-editor.desktop <<EOF
[Desktop Entry]
Type=Application
Name=PDF-XChange Editor
GenericName=PDF Editor
Comment=View and edit PDF documents
Exec=$HOME/.bin/pdf-xchange-editor %F
Icon=$icon
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
}
