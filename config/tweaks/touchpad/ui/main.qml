// Settings of scripts/patches/libinput-touchpad-lag.patch, which libinput reads within a second.

import QtQuick
import QtQuick.Controls as QQC2
import QtQuick.Layouts
import org.kde.kirigami as Kirigami
import org.kde.kcmutils as KCM

KCM.SimpleKCM {
    id: root

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
        Kirigami.Separator {
            Kirigami.FormData.isSection: true
            Kirigami.FormData.label: "With tap-to-click on"
        }
        QQC2.CheckBox {
            Kirigami.FormData.label: "One finger:"
            text: "Move the cursor at once"
            checked: kcm.settings.moveDuringTap
            onToggled: kcm.settings.moveDuringTap = checked
            KCM.SettingStateBinding {
                configObject: kcm.settings
                settingName: "moveDuringTap"
            }
        }
        Help {
            text: "Off: the cursor waits until the finger has moved 1.3 mm or 180 ms have passed, in case it's a tap (libinput's default)."
        }
        QQC2.CheckBox {
            Kirigami.FormData.label: "Two fingers:"
            text: "Start scrolling at once"
            checked: kcm.settings.scrollDuringTap
            onToggled: kcm.settings.scrollDuringTap = checked
            KCM.SettingStateBinding {
                configObject: kcm.settings
                settingName: "scrollDuringTap"
            }
        }
        Help {
            text: "Off: scrolling waits the same way, in case it's a two-finger tap."
        }

        Kirigami.Separator {
            Kirigami.FormData.isSection: true
            Kirigami.FormData.label: "Cursor"
        }
        Amount {
            Kirigami.FormData.label: "Speed when moving slowly:"
            setting: "slowSpeedGain"
        }
        Help {
            text: "The cursor's speed at the slowest finger speeds, compared with from 7 mm/s up. 1 keeps it even. libinput's 0.33 gives finer control, but slow movements speed up just after they start."
        }
        Amount {
            Kirigami.FormData.label: "Average the speed over:"
            setting: "speedSmoothing"
            unit: " ms"
            whole: true
        }
        Help {
            text: "With a slowdown above, makes it change gradually. 0 uses libinput's estimate, from the last movement only."
        }

        QQC2.CheckBox {
            Kirigami.FormData.label: "Jitter filter:"
            text: "Turn on when the finger wobbles"
            checked: kcm.settings.wobbleHysteresis
            onToggled: kcm.settings.wobbleHysteresis = checked
            KCM.SettingStateBinding {
                configObject: kcm.settings
                settingName: "wobbleHysteresis"
            }
        }
        Help {
            text: "On (libinput's default): once the finger's position flickers right-left-right, the cursor ignores the first 0.25 mm of every start and turn, until the next login or wake-up. A few jittery events in a quick stroke are enough."
        }
        Amount {
            Kirigami.FormData.label: "Next to a resting touch, move after:"
            setting: "restingTouchDistance"
            unit: " mm"
        }
        Help {
            text: "With a second touch resting lower on the touchpad (a thumb waiting to click), how far the finger moves before the cursor does. libinput's: 4 mm. A thumb at the very bottom never holds the cursor back."
        }

        Kirigami.Separator {
            Kirigami.FormData.isSection: true
            Kirigami.FormData.label: "Two-finger scrolling"
        }
        Amount {
            Kirigami.FormData.label: "Scroll after moving:"
            setting: "scrollStartDistance"
            unit: " mm"
        }
        Help {
            text: "How far both fingers move the same way before it's a scroll. libinput's: 1.5 mm."
        }
        Amount {
            Kirigami.FormData.label: "Decide after moving:"
            setting: "scrollDecideDistance"
            unit: " mm"
        }
        Help {
            text: "How far one finger moves before it's decided whether it's a scroll. libinput's: 1 mm."
        }
        Amount {
            Kirigami.FormData.label: "Once the fingers have settled:"
            setting: "scrollRestedDistance"
            unit: " mm"
        }
        Help {
            text: "The same, for fingers that have been down for 150 ms: a slow scroll starts sooner than one that starts as the fingers land. libinput's: 1 mm."
        }
        Amount {
            Kirigami.FormData.label: "Catch-up speed:"
            setting: "scrollCatchUp"
        }
        Help {
            text: "Makes up for the movement before it was a scroll: at first, the content scrolls 1 + this times as fast as the fingers, then less and less as it catches up. 0 drops that movement (libinput's default)."
        }
    }
}
