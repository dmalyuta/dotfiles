// System Settings page for kwin-touchpad-gestures.patch's settings, see ui/main.qml.

#include <KPluginFactory>
#include <KQuickManagedConfigModule>
#include <QDBusConnection>
#include <QDBusMessage>
#include <QQmlEngine>

#include "gesturessettings.h"

class GesturesModule : public KQuickManagedConfigModule
{
    Q_OBJECT
    Q_PROPERTY(GesturesSettings *settings MEMBER m_settings CONSTANT)

public:
    GesturesModule(QObject *parent, const KPluginMetaData &data)
        : KQuickManagedConfigModule(parent, data)
        , m_settings(new GesturesSettings(this))
    {
        qmlRegisterAnonymousType<GesturesSettings>("danylotweaks", 1);
    }

    void save() override
    {
        KQuickManagedConfigModule::save();
        // KWin reads them when it reloads its settings.
        QDBusConnection::sessionBus().send(QDBusMessage::createMethodCall(QStringLiteral("org.kde.KWin"),
                                                                          QStringLiteral("/KWin"),
                                                                          QStringLiteral("org.kde.KWin"),
                                                                          QStringLiteral("reconfigure")));
    }

private:
    GesturesSettings *m_settings;
};

K_PLUGIN_CLASS_WITH_JSON(GesturesModule, "kcm_danylo_gestures.json")

#include "kcm.moc"
