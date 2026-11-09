// System Settings page for libinput-touchpad-lag.patch's settings, see ui/main.qml.

#include <KPluginFactory>
#include <KQuickManagedConfigModule>
#include <QQmlEngine>

#include "touchpadsettings.h"

class TouchpadModule : public KQuickManagedConfigModule
{
    Q_OBJECT
    Q_PROPERTY(TouchpadSettings *settings MEMBER m_settings CONSTANT)

public:
    TouchpadModule(QObject *parent, const KPluginMetaData &data)
        : KQuickManagedConfigModule(parent, data)
        , m_settings(new TouchpadSettings(this))
    {
        qmlRegisterAnonymousType<TouchpadSettings>("danylotweaks", 1);
    }

private:
    TouchpadSettings *m_settings;
};

K_PLUGIN_CLASS_WITH_JSON(TouchpadModule, "kcm_danylo_touchpad.json")

#include "kcm.moc"
