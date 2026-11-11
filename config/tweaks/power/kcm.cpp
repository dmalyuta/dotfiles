// System Settings page for the settings of plasma-battery-absent-devices.patch and
// kidletime-wayland-simulate-activity.patch, see ui/main.qml.

#include <KPluginFactory>
#include <QProcess>

#include "../tweaksmodule.h"
#include "powersettings.h"

class PowerModule : public TweaksModule
{
    Q_OBJECT

public:
    PowerModule(QObject *parent, const KPluginMetaData &data)
        : TweaksModule(new PowerSettings, parent, data)
    {
    }

    void load() override
    {
        TweaksModule::load();
        m_hideAbsentDeviceBatteries = settings<PowerSettings>()->hideAbsentDeviceBatteries();
    }

    void save() override
    {
        TweaksModule::save();
        // The panel reads it when it starts.
        if (settings<PowerSettings>()->hideAbsentDeviceBatteries() != m_hideAbsentDeviceBatteries) {
            m_hideAbsentDeviceBatteries = settings<PowerSettings>()->hideAbsentDeviceBatteries();
            QProcess::startDetached(QStringLiteral("systemctl"),
                                    {QStringLiteral("--user"), QStringLiteral("restart"), QStringLiteral("plasma-plasmashell.service")});
        }
    }

private:
    bool m_hideAbsentDeviceBatteries = true;
};

K_PLUGIN_CLASS_WITH_JSON(PowerModule, "kcm_danylo_power.json")

#include "kcm.moc"
