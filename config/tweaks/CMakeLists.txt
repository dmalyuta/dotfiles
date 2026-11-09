# System Settings > Danylo's Linux Tweaks: the settings of scripts/patches, one page each for the
# touchpad, its gestures, and power. Built and installed by ./install.
cmake_minimum_required(VERSION 3.20)
project(danylo-tweaks LANGUAGES CXX)

find_package(ECM REQUIRED NO_MODULE)
set(CMAKE_MODULE_PATH ${ECM_MODULE_PATH})
set(QT_MAJOR_VERSION 6)
include(KDEInstallDirs)
include(KDECMakeSettings)
include(KDECompilerSettings NO_POLICY_SCOPE)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)

find_package(Qt6 REQUIRED COMPONENTS Core DBus Qml)
find_package(KF6 REQUIRED COMPONENTS Config CoreAddons KCMUtils)

add_subdirectory(touchpad)
add_subdirectory(gestures)
add_subdirectory(power)

# The section, with the start menu's icon.
install(FILES settings-danylo-tweaks.desktop DESTINATION ${KDE_INSTALL_DATADIR}/systemsettings/categories)
install(FILES ../icons/start_menu.svg DESTINATION ${KDE_INSTALL_ICONDIR}/hicolor/scalable/apps
        RENAME danylo-tweaks.svg)
