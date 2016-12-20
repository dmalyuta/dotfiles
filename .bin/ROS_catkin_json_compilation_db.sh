#!/bin/bash
# --------------------------------------------------------------------
#
# Create a compilation database for a ROS package using catkin. This
# will enable irony flycheck (linting) and company (autocompletion)
# for the entire ROS package given all of its dependencies!
#
# Source:
# http://emacs.stackexchange.com/questions/28026/add-include-paths-to-flycheck-and-to-company-irony
#
# Usage instructions:
#
# You only ever have to run this script once for each ROS installation
# (which usually means run once and that's it forever). Once you did
# run it once, running `catkin build` (using
# [catkin_tools](http://catkin-tools.readthedocs.io/en/latest/index.html)),
# will generate JSON compilation databases compile_commends.json in
# ~/catkin_ws/build/xxx for each ROS package xxx in
# ~/catkin_ws/src/xxx. To use these databases in Emacs such that
# flycheck (code linting) and company (autocompletion) work, run once
# from a source file buffer of project xxx:
#
#  M-x irony-cdb-json-add-compile-commands-path RET
#
# you will be prompted with the compile_commands.json to add, in this
# case, set the "Project root" to ~/catkin_ws/src/xxx/ and set then
# "Compile commands" to ~/catkin_ws/build/xxx/compile_commands.json
#
# --------------------------------------------------------------------

# create compilation databases
# ~/catkin_ws/build/xxx/compile_commands.json for all packages located
# in ~/catkin_ws/src/xxx when running `catkin build`
# NB: 1>/dev/null means run silent, show only errors (stderr) if any come up
echo "Adding -DCMAKE_EXPORT_COMPILE_COMMANDS=ON arg to CMake to generate JSON compilation databases for ROS packages"
( cd ~/catkin_ws/; catkin config --cmake-args -DCMAKE_EXPORT_COMPILE_COMMANDS=ON 1>/dev/null )
if [[  $? -eq 0 ]]; then
    echo "Successful"
    exit 0
else
    echo "Something went wrong!"
    exit 1
fi
