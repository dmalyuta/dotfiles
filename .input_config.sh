#!/bin/bash
# ----------------------------------------------------------------------
#
# Input device configuration (mouse, touchpad, keyboard, tablet, etc).
#
# To keep activated in the background, you can set up a cron job as follows:
#
#   $ crontab -e
#
# and then at the bottom of the file, add (3 minute interval repeats):
#
#   */3 *   * * *    source ~/.input_config.sh
#
# ----------------------------------------------------------------------

ERRF=/tmp/input_config_errors

#####################################
########## KEYBOARD
#####################################

# ..:: Key stickiness and move speed ::..

Delay=175 # [ms] Delay before repeating pressed key
RepeatRate=$(bc -l <<< 'scale=0; 1/(15*0.001)') # [Hz] Rate at which to repeat pressed key

# This works reliably (see https://raspberrypi.stackexchange.com/a/99940)
# Note: `$ xset q` shows the current keyboard delay and repeat rate settings
xset r rate $Delay $RepeatRate &>>$ERRF
# The lines below *should* do the same thing, but don't work reliably:
# gsettings set org.gnome.desktop.peripherals.keyboard delay 200 &>>$ERRF
# gsettings set org.gnome.desktop.peripherals.keyboard repeat-interval 15 &>>$ERRF

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
xinput set-prop "Logitech Rechargeable Touchpad T650" "Synaptics Two-Finger Scrolling" 1 1 &>>$ERRF
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
xsetwacom --set "Wacom Intuos BT S Pen stylus" "PanScrollThreshold" 100 &>>$ERRF
