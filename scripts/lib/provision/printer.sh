# shellcheck shell=bash
# shellcheck disable=SC2154 # $brother_model and $brother_ip are set by ask_questions
#
# provision's printers: the "Print to PDF" one, and the Brother network printer and scanner
# (finding it, its drivers, and its name in the print and scan dialogs).

# A printer that saves what it is sent as a PDF, for programs without a print-to-file of their own:
# Wine's, like PDF-XChange Editor, list just the CUPS printers, and none at all without one.
#
# cups-pdf saves to a fixed folder without asking. So it saves to a hidden one, which a systemd
# user unit watches (config/systemd-user): for each PDF there, bin/print-to-pdf-save asks where to
# save it, and moves it.
add_pdf_printer() {
	local unit
	if ! lpstat -v Print_to_PDF >/dev/null 2>&1 || lpstat -v Cups-PDF >/dev/null 2>&1; then
		# The printer is added through CUPS, which otherwise only starts with the other services.
		sudo systemctl enable --now cups
		# The package adds its own, named Cups-PDF: remove that one, which a reinstall brings back.
		if sudo lpadmin -p Print_to_PDF -E -v cups-pdf:/ -m CUPS-PDF_opt.ppd -D "Print to PDF"; then
			sudo lpadmin -x Cups-PDF 2>/dev/null
		else
			failed "Could not add the Print to PDF printer."
			return
		fi
	fi
	# Not the default 300 dpi: a program that prints its text as shapes, as PDF-XChange Editor does
	# for fonts that aren't TrueType, gets them rounded to this grid, and they look rough.
	lpoptions -p Print_to_PDF -l | grep -q 'Resolution.*\*1200dpi' ||
		sudo lpadmin -p Print_to_PDF -o Resolution=1200dpi

	# In ~/.cache, where SELinux lets cups-pdf write, as in the desktop folder, its default.
	# shellcheck disable=SC2016 # cups-pdf fills in ${HOME}, for the user who prints
	if ! sudo grep -qxF 'Out ${HOME}/.cache/print-to-pdf' /etc/cups/cups-pdf.conf; then
		sudo sed -i '/^Out /d' /etc/cups/cups-pdf.conf
		echo 'Out ${HOME}/.cache/print-to-pdf' | sudo tee -a /etc/cups/cups-pdf.conf >/dev/null
	fi
	mkdir -p ~/.cache/print-to-pdf ~/.config/systemd/user
	for unit in print-to-pdf.path print-to-pdf.service; do
		ln -sf "$dotfiles/config/systemd-user/$unit" ~/.config/systemd/user/
	done
	systemctl --user daemon-reload
	systemctl --user enable -q --now print-to-pdf.path
}

# Find a Brother printer on the network by what it announces over mDNS, and print its model (like
# MFC-L2750DW), hostname and IPv4 address. Prints nothing while it is off.
find_brother() {
	avahi-browse --resolve --parsable --terminate _ipp._tcp 2>/dev/null |
		awk -F';' '$1 == "=" && $3 == "IPv4" && match($0, /"ty=Brother [A-Za-z0-9-]+/) {
			print substr($0, RSTART + 12, RLENGTH - 12), $7, $8
			exit
		}'
}

# The Brother's printer driver packages, named for the model without its dashes, like its print
# queue.
brother_driver_packages() {
	local queue=${brother_model//-/}
	rpm -qa --qf '%{NAME}\n' "${queue,,}*"
}

# The Brother's scanner driver package: brscan4 or brscan5, by model.
brother_scanner_package() { rpm -qa --qf '%{NAME}\n' 'brscan[0-9]' | head -n 1; }

# The Brother printer and scanner, by Brother's installer tool: the printer driver, the scanner
# driver and its scan-button tool (brscan-skey). If the printer's IP address changes, run
#   sudo brsaneconfig4 -r <model> && sudo brsaneconfig4 -a name=<model> model=<model> ip=<new>
install_brother_drivers() {
	local dir tool
	dir=$(mktemp -d)
	tool=$dir/linux-brprinter-installer
	if curl -fsSL -o "$tool.gz" \
		https://download.brother.com/welcome/dlf006893/linux-brprinter-installer-2.2.6-1.gz &&
		sha256sum -c --quiet <<<"ce0f6221f5979804c54994ad562fad87ef74e100f596115afd9218d35a568e73  $tool.gz" &&
		gunzip "$tool.gz"; then
		# Its RPMs have digests but no signatures, which RPM 6 refuses, so an rpm first on its
		# PATH checks the digests only.
		printf '#!/bin/sh\nexec /usr/bin/rpm --define "_pkgverify_level digest" "$@"\n' >"$dir/rpm"
		chmod +x "$dir/rpm"
		# The tool installs its own build of the printer driver: remove any other, else RPM keeps
		# both. That removes the printer too, which the tool adds back.
		brother_driver_packages | xargs -r sudo rpm -e --allmatches
		# It adds the printer through CUPS, which otherwise only starts with the other services.
		sudo systemctl enable --now cups
		# Its questions, answered in order: install, accept the license, specify the device URI,
		# take the one found (A), no test print, accept the two scanner licenses, the scanner's IP.
		(cd "$dir" && printf '%s\n' y y y A n y y "$brother_ip" |
			sudo env PATH="$dir:/usr/sbin:/usr/bin" bash "$tool" "$brother_model")
		{ [ -n "$(brother_driver_packages)" ] && [ -n "$(brother_scanner_package)" ] &&
			installed brscan-skey; } ||
			failed "Could not install the Brother drivers, see the full output."
	else
		failed "The Brother installer tool download failed or changed, skipping its drivers."
	fi
	sudo rm -rf "$dir"
}

# List the Brother once, as "Brother <model>", wherever a scanner or printer is picked, instead of
# under its drivers' names.
name_brother_devices() {
	local name="Brother $brother_model" queue=${brother_model//-/} scanner uri
	# Fedora's eSCL backend (sane-airscan) finds the same scanner, so hide it there.
	write_root_file "/etc/sane.d/airscan.d/brother-${brother_model,,}.conf" <<EOF
[blacklist]
model = "*$brother_model"
EOF
	# A SANE alias also hides the original name, like brother4:net1;dev0 for brscan4.
	scanner=$(brother_scanner_package)
	[ -z "$scanner" ] || write_root_file /etc/sane.d/dll.aliases <<EOF
alias "$name" brother${scanner#brscan}:net1;dev0
EOF
	# CUPS can't rename a print queue, and its name can't have spaces: add the printer again under
	# the new name, then remove the old one, which a driver reinstall brings back.
	uri=$(lpstat -v "$queue" 2>/dev/null | sed -n "s/^device for $queue: //p")
	if [ -n "$uri" ] &&
		sudo lpadmin -p "Brother_$brother_model" -E -v "$uri" -P "/etc/cups/ppd/$queue.ppd" \
			-D "$name"; then
		sudo lpadmin -x "$queue"
	fi
}
