#!/bin/bash
# ----------------------------------------------------------------------
#
# Synaptics touchpad configuration.
#
# ----------------------------------------------------------------------

#####################################
# ..:: Synaptics touchpad (designed for Logitech T650 external touchpad) ::..

ERRF=/tmp/touchpad_errors

(xinput set-prop "Logitech Rechargeable Touchpad T650" "Synaptics Edges" 113 2719 127 2237; \
    xinput set-prop "Logitech Rechargeable Touchpad T650" "Synaptics Finger" 1 1 0; \
    xinput set-prop "Logitech Rechargeable Touchpad T650" "Synaptics Tap Time" 120; \
    xinput set-prop "Logitech Rechargeable Touchpad T650" "Synaptics Tap Move" 200; \
    xinput set-prop "Logitech Rechargeable Touchpad T650" "Synaptics Tap Durations" 150 250 100; \
    xinput set-prop "Logitech Rechargeable Touchpad T650" "Synaptics Move Speed" 1.3 1.3 0.0542299 0; \
    xinput set-prop "Logitech Rechargeable Touchpad T650" "Synaptics Tap Action" 0 0 0 0 1 0 0; \
    xinput set-prop "Logitech Rechargeable Touchpad T650" "Synaptics Click Action" 1 3 2; \
    xinput set-prop "Logitech Rechargeable Touchpad T650" "Synaptics Palm Detection" 0; \
    xinput set-prop "Logitech Rechargeable Touchpad T650" "Synaptics Noise Cancellation" 1 1; \
    xinput set-prop "Logitech Rechargeable Touchpad T650" "Synaptics Circular Scrolling" 0; \
    xinput set-prop "Logitech Rechargeable Touchpad T650" "Synaptics Circular Pad" 0) &>>$ERRF

synclient HorizHysteresis=0
synclient VertHysteresis=0

# synclient EmulateMidButtonTime=0
# synclient EmulateTwoFingerMinZ=56
# synclient EmulateTwoFingerMinW=0
# synclient VertScrollDelta=73
# synclient HorizScrollDelta=73
# synclient VertEdgeScroll=0
# synclient HorizEdgeScroll=0
# synclient CornerCoasting=0
# synclient VertTwoFingerScroll=1
# synclient HorizTwoFingerScroll=1
# synclient TouchpadOff=0
# synclient LockedDrags=0
# synclient LockedDragTimeout=5000
# synclient CoastingSpeed=20
# synclient CoastingFriction=50
# synclient PressureMotionMinZ=5
# synclient PressureMotionMaxZ=31
# synclient PressureMotionMinFactor=1
# synclient PressureMotionMaxFactor=1
# synclient ResolutionDetect=1
# synclient GrabEventDevice=0
# synclient TapAndDragGesture=1
# synclient AreaLeftEdge=0
# synclient AreaRightEdge=0
# synclient AreaTopEdge=0
# synclient AreaBottomEdge=0
# synclient ClickPad=1
# synclient RightButtonAreaLeft=0
# synclient RightButtonAreaRight=0
# synclient RightButtonAreaTop=0
# synclient RightButtonAreaBottom=0
# synclient MiddleButtonAreaLeft=0
# synclient MiddleButtonAreaRight=0
# synclient MiddleButtonAreaTop=0
# synclient MiddleButtonAreaBottom=0
