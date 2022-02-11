#!/bin/bash
#
# Syncing script for VS Code, to maintain consistency when pushing to remote
# repo.
#
# Author: Danylo Malyuta, 2022.

VSCODE_TAB=$(printf '\%.0s ' {1..4})

escape_for_sed() {
    echo $(printf '%s\n' "$1" | sed -e 's/[]\/$*.^[]/\\&/g')
}

vscode_add_line() {
    FILE="$1"
    AFTER_LINE="$2"
    SETTING="$3"
    VALUE="$4"

    SETTING_ESCAPED=$(escape_for_sed "$SETTING")

    sed -i '/'"$SETTING_ESCAPED"'/d' $FILE
    sed -i '/'"$AFTER_LINE"'/a'"$VSCODE_TAB"'"'"$SETTING"'": '"$VALUE"',' $FILE
}

vscode_cleanup_settings() {
    SETTINGS_FILE_PATH=$DIR/.vscode/settings.json

    # Save the current version
    cp $SETTINGS_FILE_PATH /tmp/settings.json.bk

    # Operate on settings.json
    AFTER_LINE="^{$"
    vscode_add_line "$SETTINGS_FILE_PATH" "$AFTER_LINE" "window.zoomLevel" -1
    vscode_add_line "$SETTINGS_FILE_PATH" "$AFTER_LINE" "editor.minimap.enabled" false
    vscode_add_line "$SETTINGS_FILE_PATH" "$AFTER_LINE" "workbench.activityBar.visible" false

    CONTROLLED_SETTINGS_HEADER=$(escape_for_sed "/// Default controlled settings")
    sed -i '/'"$CONTROLLED_SETTINGS_HEADER"'/d' $SETTINGS_FILE_PATH
    sed -i '/'"$AFTER_LINE"'/a'"$VSCODE_TAB""$CONTROLLED_SETTINGS_HEADER"'' $SETTINGS_FILE_PATH

    # Show the diff
    if [[ $(git diff $SETTINGS_FILE_PATH) = "" ]]; then
        echo "No VS Code settings.json changes"
    else
        git --no-pager diff $SETTINGS_FILE_PATH
    fi
}

vscode_restore_settings() {
    SETTINGS_FILE_PATH=$DIR/.vscode/settings.json
    mv /tmp/settings.json.bk $SETTINGS_FILE_PATH
}
