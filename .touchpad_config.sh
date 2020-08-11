#!/bin/bash
# ----------------------------------------------------------------------
#
# Synaptics touchpad configuration.
#
# ----------------------------------------------------------------------

#####################################
# ..:: Synaptics touchpad (designed for Logitech T650 external touchpad) ::..
synclient LeftEdge=113
synclient RightEdge=2719
synclient TopEdge=127
synclient BottomEdge=2237
synclient FingerLow=1
synclient FingerHigh=1 #2
synclient MaxTapTime=70 #100
synclient MaxTapMove=200 #100
synclient MaxDoubleTapTime=250
synclient SingleTapTimeout=200 #180
synclient ClickTime=100
synclient EmulateMidButtonTime=0
synclient EmulateTwoFingerMinZ=56
synclient EmulateTwoFingerMinW=7
synclient VertScrollDelta=73
synclient HorizScrollDelta=73
synclient VertEdgeScroll=0
synclient HorizEdgeScroll=0
synclient CornerCoasting=0
synclient VertTwoFingerScroll=1
synclient HorizTwoFingerScroll=1
synclient MinSpeed=1.3 #1
synclient MaxSpeed=1.3 #1.75
synclient AccelFactor=0.0542299
synclient TouchpadOff=0
synclient LockedDrags=0
synclient LockedDragTimeout=5000
synclient RTCornerButton=0
synclient RBCornerButton=0
synclient LTCornerButton=0
synclient LBCornerButton=0
synclient TapButton1=1 # Left click for one-finger tap
synclient TapButton2=0 # No action to double-finger tap
synclient TapButton3=3 # Right click for triple-finger tap
synclient ClickFinger1=1
synclient ClickFinger2=3
synclient ClickFinger3=2
synclient CircularScrolling=0
synclient CircScrollDelta=0.1
synclient CircScrollTrigger=0
synclient CircularPad=0
synclient PalmDetect=0
synclient PalmMinWidth=10
synclient PalmMinZ=39
synclient CoastingSpeed=20
synclient CoastingFriction=50
synclient PressureMotionMinZ=5
synclient PressureMotionMaxZ=31
synclient PressureMotionMinFactor=1
synclient PressureMotionMaxFactor=1
synclient ResolutionDetect=1
synclient GrabEventDevice=0
synclient TapAndDragGesture=1
synclient AreaLeftEdge=0
synclient AreaRightEdge=0
synclient AreaTopEdge=0
synclient AreaBottomEdge=0
synclient HorizHysteresis=0 #18
synclient VertHysteresis=0 #18
synclient ClickPad=1
synclient RightButtonAreaLeft=0
synclient RightButtonAreaRight=0
synclient RightButtonAreaTop=0
synclient RightButtonAreaBottom=0
synclient MiddleButtonAreaLeft=0
synclient MiddleButtonAreaRight=0
synclient MiddleButtonAreaTop=0
synclient MiddleButtonAreaBottom=0
# xinput --set-prop "Logitech Rechargeable Touchpad T650" 320 2 10 15 # Finger
# xinput --set-prop "Logitech Rechargeable Touchpad T650" 321 80 # Tap Time
# xinput --set-prop "Logitech Rechargeable Touchpad T650" 322 100 # Tap Move
