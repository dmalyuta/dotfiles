// A system tray button that shows or hides the on-screen keyboard. It is only in the tray in tablet
// mode, or while the keyboard is up.
//
// KWin is set never to show the keyboard by itself (VirtualKeyboardMode=0, see
// scripts/configure_kde), since tapping a text field popped it up over the app. Plasma's own
// On-Screen Keyboard tray item can't show it then: a tap only opens its settings. A tap here asks
// KWin to show it anyway. It types into the focused text field, or as a hardware keyboard into an
// app that doesn't support input methods.

import QtQuick

import org.kde.plasma.plasmoid
import org.kde.plasma.core as PlasmaCore
import org.kde.plasma.workspace.keyboardlayout as Keyboards
import org.kde.kirigami as Kirigami

PlasmoidItem {
    id: root

    readonly property bool keyboardShown: Keyboards.KWinVirtualKeyboard.visible

    function toggle() {
        if (keyboardShown) {
            Keyboards.KWinVirtualKeyboard.active = false;
        } else {
            Keyboards.KWinVirtualKeyboard.forceActivate();
        }
    }

    Plasmoid.icon: keyboardShown ? "input-keyboard-virtual-hide-symbolic"
                                 : "input-keyboard-virtual-show-symbolic"
    Plasmoid.status: Keyboards.KWinVirtualKeyboard.available
        && (Kirigami.Settings.tabletMode || keyboardShown)
        ? PlasmaCore.Types.ActiveStatus : PlasmaCore.Types.HiddenStatus
    Plasmoid.onActivated: toggle()

    toolTipMainText: keyboardShown ? "Hide the on-screen keyboard" : "Show the on-screen keyboard"
    toolTipSubText: ""

    preferredRepresentation: fullRepresentation
    fullRepresentation: Kirigami.Icon {
        source: Plasmoid.icon
        active: mouse.containsMouse
        activeFocusOnTab: true
        Accessible.name: root.toolTipMainText
        Accessible.role: Accessible.Button

        Keys.onPressed: event => {
            switch (event.key) {
            case Qt.Key_Space:
            case Qt.Key_Enter:
            case Qt.Key_Return:
            case Qt.Key_Select:
                root.toggle();
                break;
            }
        }

        MouseArea {
            id: mouse
            anchors.fill: parent
            hoverEnabled: true
            onClicked: root.toggle()
        }
    }
}
