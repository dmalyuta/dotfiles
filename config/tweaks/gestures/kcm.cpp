// System Settings page for kwin-touchpad-gestures.patch's settings, see ui/main.qml.

#include <KPluginFactory>
#include <QDBusConnection>
#include <QDBusMessage>

#include "../tweaksmodule.h"
#include "gesturessettings.h"

class GesturesModule : public TweaksModule
{
    Q_OBJECT

public:
    GesturesModule(QObject *parent, const KPluginMetaData &data)
        : TweaksModule(new GesturesSettings, parent, data)
    {
    }

    void save() override
    {
        TweaksModule::save();
        // KWin reads them when it reloads its settings.
        QDBusConnection::sessionBus().send(QDBusMessage::createMethodCall(QStringLiteral("org.kde.KWin"),
                                                                          QStringLiteral("/KWin"),
                                                                          QStringLiteral("org.kde.KWin"),
                                                                          QStringLiteral("reconfigure")));
    }
};

K_PLUGIN_CLASS_WITH_JSON(GesturesModule, "kcm_danylo_gestures.json")

#include "kcm.moc"
