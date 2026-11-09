kcmutils_add_qml_kcm(kcm_danylo_power SOURCES kcm.cpp DISABLE_DESKTOP_FILE_GENERATION)
kconfig_add_kcfg_files(kcm_danylo_power powersettings.kcfgc GENERATE_MOC)
target_link_libraries(kcm_danylo_power PRIVATE Qt::DBus Qt::Qml KF6::ConfigGui KF6::CoreAddons KF6::KCMUtilsQuick)
