;;; theme-setup.el --- 

;;; Commentary:

;;; Theme switching GUI/terminal

;;; Code:

(defun use-zenburn-theme()
  ;; (load-theme 'zenburn t t) ;; last t is for NO-ENABLE
  ;; (enable-theme 'zenburn)
  ;; (set-face-attribute 'mode-line nil :box nil)
  ;; (set-face-attribute 'mode-line-inactive nil :box nil)
  (require 'doom-themes)
  ;; Load the theme (doom-one, doom-dark, etc.)
  (load-theme 'doom-one t)
  ;; brighter source buffers (that represent files)
  (add-hook 'find-file-hook #'doom-buffer-mode-maybe)
  ;; ...if you use auto-revert-mode
  (add-hook 'after-revert-hook #'doom-buffer-mode-maybe)
  ;; And you can brighten other buffers (unconditionally) with:
  (add-hook 'ediff-prepare-buffer-hook #'doom-buffer-mode)
  ;; Enable custom neotree theme
  (doom-themes-neotree-config)
  ;; Enable nlinum line highlighting
  (doom-themes-nlinum-config)
  )

(defun use-nw-theme()
  ;; Zenburn works well with TERM=xterm-256-color
  ;; otherwise just comment out - no theme at all is next-best
  ;; alternative I think
  (use-zenburn-theme)
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
