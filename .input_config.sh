#!/bin/bash
# ----------------------------------------------------------------------
#
# Input device configuration (mouse, touchpad, keyboard, tablet, etc).
#
# ----------------------------------------------------------------------

ERRF=/tmp/input_config_errors

#####################################
########## KEYBOARD
#####################################

# Key stickiness and move speed
gsettings set org.gnome.desktop.peripherals.keyboard delay 212 &>>$ERRF
gsettings set org.gnome.desktop.peripherals.keyboard repeat-interval 19 &>>$ERRF

#####################################
########## TOUCHPAD
#####################################

# ..:: Logitech T650 external touchpad ::..

Speed=1.5

xinput set-prop "Logitech Rechargeable Touchpad T650" "Synaptics Edges" 113 2719 127 2237 &>>$ERRF
xinput set-prop "Logitech Rechargeable Touchpad T650" "Synaptics Finger" 1 1 0 &>>$ERRF
xinput set-prop "Logitech Rechargeable Touchpad T650" "Synaptics Tap Time" 100 &>>$ERRF
xinput set-prop "Logitech Rechargeable Touchpad T650" "Synaptics Tap Move" 200 &>>$ERRF
xinput set-prop "Logitech Rechargeable Touchpad T650" "Synaptics Tap Durations" 150 250 100 &>>$ERRF
xinput set-prop "Logitech Rechargeable Touchpad T650" "Synaptics Move Speed" $Speed $Speed 0 0 &>>$ERRF
xinput set-prop "Logitech Rechargeable Touchpad T650" "Synaptics Tap Action" 0 0 0 0 0 0 0 &>>$ERRF
xinput set-prop "Logitech Rechargeable Touchpad T650" "Synaptics Click Action" 1 3 0 &>>$ERRF
xinput set-prop "Logitech Rechargeable Touchpad T650" "Synaptics Palm Detection" 0 &>>$ERRF
# Noise cancellation = (horizontal,vertical) hysteresis
xinput set-prop "Logitech Rechargeable Touchpad T650" "Synaptics Noise Cancellation" 0 0 &>>$ERRF
xinput set-prop "Logitech Rechargeable Touchpad T650" "Synaptics Circular Scrolling" 0 &>>$ERRF
xinput set-prop "Logitech Rechargeable Touchpad T650" "Synaptics Coasting Speed" 0 0 &>>$ERRF
xinput set-prop "Logitech Rechargeable Touchpad T650" "Synaptics Circular Pad" 0 &>>$ERRF
# Quadratic acceleration profile -- feels better
xinput set-prop "Logitech Rechargeable Touchpad T650" "Device Accel Profile" 2 &>>$ERRF
xinput set-prop "Logitech Rechargeable Touchpad T650" "Device Accel Constant Deceleration" 3.5 &>>$ERRF
xinput set-prop "Logitech Rechargeable Touchpad T650" "Device Accel Adaptive Deceleration" 1.0 &>>$ERRF
xinput set-prop "Logitech Rechargeable Touchpad T650" "Device Accel Velocity Scaling" 9.0 &>>$ERRF

#####################################
########## STYLUS
#####################################

# ..:: Wacom Intuos S BT Pad/Stylus ::..
# Some command line commands you can run:
# - To view devices:
#  $ xsetwacom --list devices
# - To view all options:
#  $ xsetwacom -s get "Wacom Intuos BT S Pen stylus" all

xsetwacom --set "Wacom Intuos BT S Pen stylus" "Button" 2 "pan" &>>$ERRF
xsetwacom --set "Wacom Intuos BT S Pen stylus" "PanScrollThreshold" 300 &>>$ERRF

# ..:: Seenda external touchpad (DEPRECATED) ::..

# FingerLow=3 # Release when pressure below this value
# FingerHigh=5 # Touch when pressure above this value
# MaxTapTime=150
# SingleTapTimeout=150
# MaxDoubleTapTime=150 #$((MaxTapTime+SingleTapTimeout))
# MaxTapMove=180
# Speed=3.0

# xinput set-prop "HTX USB HID Device HTX HID Device Touchpad" "Synaptics Finger" $FingerLow $FingerHigh 0 &>>$ERRF
# xinput set-prop "HTX USB HID Device HTX HID Device Touchpad" "Synaptics Tap Time" $MaxTapTime &>>$ERRF
# xinput set-prop "HTX USB HID Device HTX HID Device Touchpad" "Synaptics Tap Move" $MaxTapMove &>>$ERRF
# # SingleTapTimeout, MaxDoubleTapTime, ClickTime
# xinput set-prop "HTX USB HID Device HTX HID Device Touchpad" "Synaptics Tap Durations" $SingleTapTimeout $MaxDoubleTapTime 60 &>>$ERRF
# xinput set-prop "HTX USB HID Device HTX HID Device Touchpad" "Synaptics Move Speed" $Speed $Speed 0 0 &>>$ERRF
# xinput set-prop "HTX USB HID Device HTX HID Device Touchpad" "Synaptics Tap Action" 0 0 0 0 1 3 2 &>>$ERRF
# xinput set-prop "HTX USB HID Device HTX HID Device Touchpad" "Synaptics Click Action" 0 0 0 &>>$ERRF
# xinput set-prop "HTX USB HID Device HTX HID Device Touchpad" "Synaptics Palm Detection" 0 &>>$ERRF
# # Noise cancellation = (horizontal,vertical) hysteresis
# xinput set-prop "HTX USB HID Device HTX HID Device Touchpad" "Synaptics Noise Cancellation" 0 0 &>>$ERRF
# xinput set-prop "HTX USB HID Device HTX HID Device Touchpad" "Synaptics Circular Scrolling" 0 &>>$ERRF
# xinput set-prop "HTX USB HID Device HTX HID Device Touchpad" "Synaptics Coasting Speed" 0 0 &>>$ERRF
# xinput set-prop "HTX USB HID Device HTX HID Device Touchpad" "Synaptics Circular Pad" 0 &>>$ERRF
# # Quadratic acceleration profile -- feels better
# xinput set-prop "HTX USB HID Device HTX HID Device Touchpad" "Device Accel Profile" 2 &>>$ERRF
# xinput set-prop "HTX USB HID Device HTX HID Device Touchpad" "Device Accel Constant Deceleration" 2.4 &>>$ERRF
# xinput set-prop "HTX USB HID Device HTX HID Device Touchpad" "Device Accel Adaptive Deceleration" 1.0 &>>$ERRF
# xinput set-prop "HTX USB HID Device HTX HID Device Touchpad" "Device Accel Velocity Scaling" 9.0 &>>$ERRF
