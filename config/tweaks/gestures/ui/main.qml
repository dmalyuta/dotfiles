// Settings of scripts/patches/kwin-touchpad-gestures.patch, which KWin reads when they're applied.

import QtQuick
import QtQuick.Controls as QQC2
import QtQuick.Layouts
import org.kde.kirigami as Kirigami
import org.kde.kcmutils as KCM

KCM.SimpleKCM {
    id: root

    readonly property bool switchesWindows: kcm.settings.threeFingerSwipe === 0

    // A spin box for a number setting, in steps of 0.05, or of 10 for a `whole` one.
    component Amount: QQC2.SpinBox {
        id: amount
        required property string setting
        property string unit: ""
        property bool whole: false
        readonly property real scale: whole ? 1 : 100
        from: 0
        to: 1000 * scale
        stepSize: whole ? 10 : 5
        editable: true
        value: Math.round(kcm.settings[setting] * scale)
        onValueModified: kcm.settings[setting] = value / scale
        textFromValue: (value, locale) => Number(value / amount.scale).toLocaleString(locale, "f", amount.whole ? 0 : 2) + amount.unit
        valueFromText: (text, locale) => Math.round(Number.fromLocaleString(locale, text.replace(amount.unit, "").trim()) * amount.scale)
        KCM.SettingStateBinding {
            configObject: kcm.settings
            settingName: amount.setting
        }
    }

    component Help: QQC2.Label {
        Layout.fillWidth: true
        Layout.maximumWidth: Kirigami.Units.gridUnit * 24
        wrapMode: Text.Wrap
        font: Kirigami.Theme.smallFont
        opacity: 0.7
    }

    Kirigami.FormLayout {
        QQC2.ComboBox {
            Kirigami.FormData.label: "Three-finger sideways swipes:"
            model: ["Switch windows", "Switch desktops", "Do nothing"]
            currentIndex: kcm.settings.threeFingerSwipe
            onActivated: index => kcm.settings.threeFingerSwipe = index
            KCM.SettingStateBinding {
                configObject: kcm.settings
                settingName: "threeFingerSwipe"
            }
        }
        Help {
            text: "Four fingers always switch desktops."
        }
        QQC2.CheckBox {
            Kirigami.FormData.label: "Desktops:"
            text: "Move the way the fingers go"
            checked: kcm.settings.desktopsFollowFingers
            onToggled: kcm.settings.desktopsFollowFingers = checked
            KCM.SettingStateBinding {
                configObject: kcm.settings
                settingName: "desktopsFollowFingers"
            }
        }
        Help {
            text: "Like scrolling: swiping left goes to the desktop on the left. Off: KDE's way, where it goes to the one on the right."
        }

        Kirigami.Separator {
            Kirigami.FormData.isSection: true
            Kirigami.FormData.label: "Switching windows"
        }
        Amount {
            Kirigami.FormData.label: "Open the switcher after:"
            enabled: root.switchesWindows
            setting: "openDistance"
            unit: " mm"
        }
        Help {
            text: "How far the fingers move sideways before the switcher opens, on the previous window."
        }
        Amount {
            Kirigami.FormData.label: "Next window every:"
            enabled: root.switchesWindows
            setting: "stepDistance"
            unit: " mm"
        }
        QQC2.CheckBox {
            Kirigami.FormData.label: "Rows:"
            enabled: root.switchesWindows
            text: "Switch rows with up and down movement"
            checked: kcm.settings.switchRows
            onToggled: kcm.settings.switchRows = checked
            KCM.SettingStateBinding {
                configObject: kcm.settings
                settingName: "switchRows"
            }
        }
        Amount {
            Kirigami.FormData.label: "Next row every:"
            enabled: root.switchesWindows && kcm.settings.switchRows
            setting: "rowDistance"
            unit: " mm"
        }
        Amount {
            Kirigami.FormData.label: "Direction counted every:"
            enabled: root.switchesWindows
            setting: "axisDistance"
            unit: " mm"
        }
        Help {
            text: "The movement counts in pieces this long, each along the direction it mostly went, and must go this far past a window or row to change it, so that drift and jitter don't. 0 counts every bit."
        }
        Amount {
            Kirigami.FormData.label: "Flick time:"
            enabled: root.switchesWindows
            setting: "flickTime"
            unit: " ms"
            whole: true
        }
        Help {
            text: "A swipe over sooner switches to the previous window without showing the switcher."
        }
    }
}
