# ~/.bash_profile: used for login shells, run at login either of the
# local computer or when starting an ssh session. Put non-bash related
# stuff here like setting the PATH/one-time configuration of the
# working environment.

# source .bashrc
# see http://stackoverflow.com/questions/820517/bashrc-at-ssh-login
if [[ -f ~/.bashrc ]]; then
    . ~/.bashrc
fi

# add .bin directory to path, which includes custom
# user scripts
if [[ -d ~/.bin ]]; then
    PATH="${HOME}/.bin:$PATH"
fi

# Emacs ansi-term directory tracking
# if [ "$TERM" = eterm-color ]; then
#     function eterm-set-cwd {
#         $@
#         echo -e "\033AnSiTc" $(pwd)
#     }
    
#     # set hostname, user, and cwd
#     function eterm-reset {
#         echo -e "\033AnSiTu" $(whoami)
#         echo -e "\033AnSiTc" $(pwd)
#         echo -e "\033AnSiTh" $(hostname)
#     }
    
#     for temp in cd pushd popd; do
#         alias $temp="eterm-set-cwd $temp"
#     done
    
#     # set hostname, user, and cwd now
#     eterm-reset
# fi

# ROS setup
if [[ -f "/opt/ros/indigo/setup.bash" ]]; then
   . /opt/ros/indigo/setup.bash
fi
if [[ -f ~/catkin_ws/devel/setup.bash ]]; then
    . ~/catkin_ws/devel/setup.bash
fi
# in ~/.bashrc optionally add the following lines for ROS network setup:
# export ROS_HOSTNAME=<local_IP_address (AAA.BBB.C.DDD)>
# export ROS_IP=<local_IP_address (AAA.BBB.C.DDD)>
# export ROS_MASTER_URI=<master_URI (http://EEE.FFF.H.III:JKLMN)>
