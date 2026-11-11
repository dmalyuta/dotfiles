// What the pages of System Settings > Danylo's Linux Tweaks share: a page that edits one settings
// object (generated from the page's .kcfg), which its QML reads and writes as kcm.settings.

#pragma once

#include <KCoreConfigSkeleton>
#include <KQuickManagedConfigModule>

class TweaksModule : public KQuickManagedConfigModule
{
    Q_OBJECT
    Q_PROPERTY(KCoreConfigSkeleton *settings MEMBER m_settings CONSTANT)

public:
    // The page owns the settings, which is also how it finds them to load, save and reset.
    TweaksModule(KCoreConfigSkeleton *settings, QObject *parent, const KPluginMetaData &data)
        : KQuickManagedConfigModule(parent, data)
        , m_settings(settings)
    {
        settings->setParent(this);
    }

protected:
    // The settings as the page's own class, for its getters.
    template<class Settings>
    Settings *settings() const
    {
        return static_cast<Settings *>(m_settings);
    }

private:
    KCoreConfigSkeleton *m_settings;
};
