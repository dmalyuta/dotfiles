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
    # - Activity bar is hidden
    sed -i '/workbench\.activityBar\.visible/d' $SETTINGS_FILE_PATH
    sed -i '/\/\/\/ Appearance/a'"$TAB"'"workbench.activityBar.visible": false,' $SETTINGS_FILE_PATH
    # - Minimap is disabled
    sed -i '/editor\.minimap\.enabled/d' $SETTINGS_FILE_PATH
    sed -i '/workbench\.activityBar\.visible/a'"$TAB"'"editor.minimap.enabled": false,' $SETTINGS_FILE_PATH
    # - Window zoom level
    sed -i '/window\.zoomLevel/d' $SETTINGS_FILE_PATH
    sed -i '/editor\.minimap\.enabled/a'"$TAB"'"window.zoomLevel": -1,' $SETTINGS_FILE_PATH

    # Show the diff
    SETTINGS_DIFF=$(git diff $SETTINGS_FILE_PATH)
    if [[ $SETTINGS_DIFF = "" ]]; then
        echo "No VS Code settings.json changes"
    else
        echo $SETTINGS_DIFF
    fi
}

vscode_restore_settings() {
    SETTINGS_FILE_PATH=$DIR/.vscode/settings.json
    mv /tmp/settings.json.bk $SETTINGS_FILE_PATH
}
