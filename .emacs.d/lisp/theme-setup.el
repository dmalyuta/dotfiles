;;; theme-setup.el ---

;;; Commentary:

;;; Theme switching GUI/terminal

;;; Code:

(defun use-gui-theme()
  ;; (load-theme 'zenburn t t) ;; last t is for NO-ENABLE
  ;; (enable-theme 'zenburn)
  ;; (set-face-attribute 'mode-line nil :box nil)
  ;; (set-face-attribute 'mode-line-inactive nil :box nil)
  (require 'doom-themes)
  ;; Load the theme (doom-one, doom-dark, etc.)
  (load-theme 'doom-one t)
  ;; Enable custom neotree theme
  (doom-themes-neotree-config)
  )

(defun use-nw-theme()
  ;; Works well with TERM=xterm-256-color
  ;; ---
  ;; If you cannot do TERM=xterm-256-color, the just comment
  ;; out - no theme at all is next-best alternative I think
  (use-gui-theme)
  (set-face-attribute 'region nil :background "#666")
  )

(defun my-gui-config ()
  (interactive)
  ;;(disable-theme 'tsdh-dark) ; in case it was active
  (use-gui-theme)
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
