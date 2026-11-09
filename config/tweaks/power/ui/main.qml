// Settings of scripts/patches/plasma-battery-absent-devices.patch and
// kidletime-wayland-simulate-activity.patch.

import QtQuick
import QtQuick.Controls as QQC2
import QtQuick.Layouts
import org.kde.kirigami as Kirigami
import org.kde.kcmutils as KCM

KCM.SimpleKCM {
    component Help: QQC2.Label {
        Layout.fillWidth: true
        Layout.maximumWidth: Kirigami.Units.gridUnit * 24
        wrapMode: Text.Wrap
        font: Kirigami.Theme.smallFont
        opacity: 0.7
    }

    Kirigami.FormLayout {
        QQC2.CheckBox {
            Kirigami.FormData.label: "Battery menu:"
            text: "List a device's battery only once it's there"
            checked: kcm.settings.hideAbsentDeviceBatteries
            onToggled: kcm.settings.hideAbsentDeviceBatteries = checked
            KCM.SettingStateBinding {
                configObject: kcm.settings
                settingName: "hideAbsentDeviceBatteries"
            }
        }
        Help {
            text: "Like the touchscreen pen's, which shows at 0% until a pen is used. Applying a change restarts the panel."
        }
        QQC2.CheckBox {
            Kirigami.FormData.label: "Sleep:"
            text: "Sleep again after waking up by itself"
            checked: kcm.settings.restartIdleTimers
            onToggled: kcm.settings.restartIdleTimers = checked
            KCM.SettingStateBinding {
                configObject: kcm.settings
                settingName: "restartIdleTimers"
            }
        }
        Help {
            text: "After a wakeup nobody touches, sleep again once the idle time has passed. Off: it stays awake until used."
        }
    }
}
