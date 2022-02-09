#!/bin/bash
#
# Syncing script for VS Code, to maintain consistency when pushing to remote
# repo.
#
# Author: Danylo Malyuta, 2022.

vscode_cleanup_settings() {
    SETTINGS_FILE_PATH=$DIR/.vscode/settings.json
    TAB=$(printf '\%.0s ' {1..4})

    # Save the current version
    cp $SETTINGS_FILE_PATH /tmp/settings.json.bk

    # Operate on settings.json
    # - Make sure that activity bar is hidden
    sed -i '/workbench\.activityBar\.visible/d' $SETTINGS_FILE_PATH
    sed -i '/\/\/\/ Appearance/a'"$TAB"'"workbench.activityBar.visible": false,' \
        $SETTINGS_FILE_PATH
}

vscode_restore_settings() {
    SETTINGS_FILE_PATH=$DIR/.vscode/settings.json
    mv /tmp/settings.json.bk $SETTINGS_FILE_PATH
}
