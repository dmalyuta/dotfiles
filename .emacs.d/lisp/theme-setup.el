;;; theme-setup.el --- 

;;; Commentary:

;;; Theme switching GUI/terminal

;;; Code:

(defun use-zenburn-theme()
  (load-theme 'zenburn t t) ;; last t is for NO-ENABLE
  (enable-theme 'zenburn)
  (set-face-attribute 'mode-line nil :box nil)
  (set-face-attribute 'mode-line-inactive nil :box nil)
  )

(defun use-nw-theme()
  ;; use no theme...
  ;; (load-theme 'tsdh-dark t t) ;; last t is for NO-ENABLE
  ;; (enable-theme 'tsdh-dark)
  )

(defun my-gui-config ()
  (interactive)
  ;;(disable-theme 'tsdh-dark) ; in case it was active
  (use-zenburn-theme)
  ;; highlight current line (only in GUI mode, since it's
  ;; uncomfortable in the terminal)
  (add-hook 'c-mode-common-hook 'hl-line-mode)
  (add-hook 'python-mode-hook 'hl-line-mode)
  (add-hook 'sh-mode-hook 'hl-line-mode)
  (add-hook 'emacs-lisp-mode-hook 'hl-line-mode)
  ;;(global-hl-line-mode +1)
  )

(defun my-terminal-config ()
  (interactive)
  (disable-theme 'zenburn) ; in case it was active
  (remove-hook 'c-mode-common-hook 'hl-line-mode)
  (remove-hook 'python-mode-hook 'hl-line-mode)
  (remove-hook 'sh-mode-hook 'hl-line-mode)
  (remove-hook 'emacs-lisp-mode-hook 'hl-line-mode)
  (use-nw-theme)
  )

(defun pick-color-theme (frame)
  (interactive)
  (select-frame frame)
  (if (window-system)
      (my-gui-config)
    (my-terminal-config))
  )

(defun my-normal-startup-pick-color-theme ()
  (interactive)
  (if (window-system)
      (my-gui-config)
    (my-terminal-config))
  )

(provide 'theme-setup)
;;; theme-setup.el ends here
