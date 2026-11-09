// System Settings page for the settings of plasma-battery-absent-devices.patch and
// kidletime-wayland-simulate-activity.patch, see ui/main.qml.

#include <KPluginFactory>
#include <KQuickManagedConfigModule>
#include <QProcess>
#include <QQmlEngine>

#include "powersettings.h"

class PowerModule : public KQuickManagedConfigModule
{
    Q_OBJECT
    Q_PROPERTY(PowerSettings *settings MEMBER m_settings CONSTANT)

public:
    PowerModule(QObject *parent, const KPluginMetaData &data)
        : KQuickManagedConfigModule(parent, data)
        , m_settings(new PowerSettings(this))
    {
        qmlRegisterAnonymousType<PowerSettings>("danylotweaks", 1);
    }

    void load() override
    {
        KQuickManagedConfigModule::load();
        m_hideAbsentDeviceBatteries = m_settings->hideAbsentDeviceBatteries();
    }

    void save() override
    {
        KQuickManagedConfigModule::save();
        // The panel reads it when it starts.
        if (m_settings->hideAbsentDeviceBatteries() != m_hideAbsentDeviceBatteries) {
            m_hideAbsentDeviceBatteries = m_settings->hideAbsentDeviceBatteries();
            QProcess::startDetached(QStringLiteral("systemctl"),
                                    {QStringLiteral("--user"), QStringLiteral("restart"), QStringLiteral("plasma-plasmashell.service")});
        }
    }

private:
    PowerSettings *m_settings;
    bool m_hideAbsentDeviceBatteries = true;
};

K_PLUGIN_CLASS_WITH_JSON(PowerModule, "kcm_danylo_power.json")

#include "kcm.moc"
