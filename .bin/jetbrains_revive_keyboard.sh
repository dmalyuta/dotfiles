#!/bin/bash
# Fix unresponsive keyboard in Jetbrains' intelliJ based IDEs (e.g. CLion, Pycharm)

# Solution from http://stackoverflow.com/questions/23294884/pycharm-with-ideavim-occasionally-makes-the-keyboard-unresponsive
killall -9 ibus-x11

# Alternative solution from http://askubuntu.com/questions/501907/keyboard-locks-in-intellij-idea-on-ubuntu-14-04
#ibus-daemon -rd
