
;;;;;;;;;;;;;;;;; ROS

(use-package exec-path-from-shell
  :ensure t
  :config
  (setq exec-path-from-shell-check-startup-files 'nil)
  (exec-path-from-shell-copy-env "ROS_ROOT")
  (exec-path-from-shell-copy-env "ROS_PACKAGE_PATH")
  (exec-path-from-shell-copy-env "ROS_MASTER_URI")
  (exec-path-from-shell-copy-env "ROS_HOSTNAME")
  (exec-path-from-shell-copy-env "ROSLISP_PACKAGE_DIRECTORIES")
  (exec-path-from-shell-copy-env "ROS_DISTRO")
  (exec-path-from-shell-copy-env "ROS_IP")
  (exec-path-from-shell-copy-env "ROS_ETC_DIR")
  (exec-path-from-shell-initialize))

(if (string= (getenv "ROS_DISTRO") "indigo")
    (;; rosemacs: a collection of packages to work on ROS-based software
     ;; from Emacs.
     (add-to-list 'load-path "/opt/ros/indigo/share/emacs/site-lisp")
     (require 'rosemacs-config)))
