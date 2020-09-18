#!/bin/bash
# ----------------------------------------------------------------------
#
# Synaptics touchpad configuration.
#
# ----------------------------------------------------------------------

#####################################
# ..:: Synaptics touchpad (designed for Logitech T650 external touchpad) ::..

ERRF=/tmp/touchpad_errors

xinput set-prop "Logitech Rechargeable Touchpad T650" "Synaptics Edges" 113 2719 127 2237 &>>$ERRF
xinput set-prop "Logitech Rechargeable Touchpad T650" "Synaptics Finger" 1 1 0 &>>$ERRF
xinput set-prop "Logitech Rechargeable Touchpad T650" "Synaptics Tap Time" 100 &>>$ERRF
xinput set-prop "Logitech Rechargeable Touchpad T650" "Synaptics Tap Move" 200 &>>$ERRF
xinput set-prop "Logitech Rechargeable Touchpad T650" "Synaptics Tap Durations" 150 250 100 &>>$ERRF
xinput set-prop "Logitech Rechargeable Touchpad T650" "Synaptics Move Speed" 1.6 1.6 0 0 &>>$ERRF
xinput set-prop "Logitech Rechargeable Touchpad T650" "Synaptics Tap Action" 0 0 0 0 0 0 0 &>>$ERRF
xinput set-prop "Logitech Rechargeable Touchpad T650" "Synaptics Click Action" 1 3 0 &>>$ERRF
xinput set-prop "Logitech Rechargeable Touchpad T650" "Synaptics Palm Detection" 0 &>>$ERRF
# Noise cancellation = (horizontal,vertical) hysteresis
xinput set-prop "Logitech Rechargeable Touchpad T650" "Synaptics Noise Cancellation" 0 0 &>>$ERRF
xinput set-prop "Logitech Rechargeable Touchpad T650" "Synaptics Circular Scrolling" 0 &>>$ERRF
xinput set-prop "Logitech Rechargeable Touchpad T650" "Synaptics Coasting Speed" 0 0 &>>$ERRF
xinput set-prop "Logitech Rechargeable Touchpad T650" "Synaptics Circular Pad" 0 &>>$ERRF

xinput set-prop "HTX USB HID Device HTX HID Device Touchpad" "Synaptics Finger" 3 5 0 &>>$ERRF
xinput set-prop "HTX USB HID Device HTX HID Device Touchpad" "Synaptics Tap Time" 90 &>>$ERRF
xinput set-prop "HTX USB HID Device HTX HID Device Touchpad" "Synaptics Tap Move" 30 &>>$ERRF
# SingleTapTimeout, MaxDoubleTapTime, ClickTime
xinput set-prop "HTX USB HID Device HTX HID Device Touchpad" "Synaptics Tap Durations" 180 180 100 &>>$ERRF
xinput set-prop "HTX USB HID Device HTX HID Device Touchpad" "Synaptics Move Speed" 1.8 1.8 0 0 &>>$ERRF
xinput set-prop "HTX USB HID Device HTX HID Device Touchpad" "Synaptics Tap Action" 0 0 0 0 1 3 2 &>>$ERRF
xinput set-prop "HTX USB HID Device HTX HID Device Touchpad" "Synaptics Click Action" 0 0 0 &>>$ERRF
xinput set-prop "HTX USB HID Device HTX HID Device Touchpad" "Synaptics Palm Detection" 0 &>>$ERRF
# Noise cancellation = (horizontal,vertical) hysteresis
xinput set-prop "HTX USB HID Device HTX HID Device Touchpad" "Synaptics Noise Cancellation" 0 0 &>>$ERRF
xinput set-prop "HTX USB HID Device HTX HID Device Touchpad" "Synaptics Circular Scrolling" 0 &>>$ERRF
xinput set-prop "HTX USB HID Device HTX HID Device Touchpad" "Synaptics Coasting Speed" 0 0 &>>$ERRF
xinput set-prop "HTX USB HID Device HTX HID Device Touchpad" "Synaptics Circular Pad" 0 &>>$ERRF
