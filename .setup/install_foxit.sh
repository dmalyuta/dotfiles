#!/bin/bash
# ----------------------------------------------------------------------
#
# Foxit PDF Reader installation.
#
#
# View, create, convert, annotate and print. Collaborate and
# share. Fill Forms. Sign. Probably the best standardized PDF
# capability for Linux!
#
# To learn more about Foxit PDF Reader, visit:
# https://www.foxitsoftware.com/products/pdf-reader/
#
# ----------------------------------------------------------------------

echo_prefix_temp="$echo_prefix"
echo_prefix="[foxit setup] "

if ! type "FoxitReader" > /dev/null 2>&1; then
    # download Foxit Reader
    runcmd "wget http://cdn01.foxitsoftware.com/pub/foxit/reader/desktop/linux/2.x/2.2/en_us/FoxitReader2.2.1025_Server_x86_enu_Setup.run.tar.gz"

    # unpack the installer
    runcmd "tar -zxvf FoxitReader*.tar.gz"

    # install it
    echowarn "Please follow the GUI's instructions and install Foxit PDF Reader into /opt/foxitsoftware/foxitreader"
    foxit_installer_name=$(ls FoxitReader*.run)
    runcmd "sudo ./'$foxit_installer_name'"

    # delete the installer and compressed archive
    runcmd "rm -rf FoxitReader*"

    # disable the Foxit PDF Reader's cloud tools, which use lots of CPU, by moving to the fxpluging_old directory
    runcmd "mkdir -p /opt/foxitsoftware/foxitreader/fxplugins_old"
    runcmd "mv /opt/foxitsoftware/foxitreader/fxplugins/ /opt/foxitsoftware/foxitreader/fxplugins_old/"
fi

echo_prefix="$echo_prefix_temp"
