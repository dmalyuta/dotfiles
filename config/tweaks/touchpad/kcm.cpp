// System Settings page for libinput-touchpad-lag.patch's settings, see ui/main.qml.

#include <KPluginFactory>

#include "../tweaksmodule.h"
#include "touchpadsettings.h"

class TouchpadModule : public TweaksModule
{
    Q_OBJECT

public:
    TouchpadModule(QObject *parent, const KPluginMetaData &data)
        : TweaksModule(new TouchpadSettings, parent, data)
    {
    }
};

K_PLUGIN_CLASS_WITH_JSON(TouchpadModule, "kcm_danylo_touchpad.json")

#include "kcm.moc"
