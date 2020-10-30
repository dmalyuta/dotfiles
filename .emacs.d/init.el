;;; init.el
;; To byte-compile: [M-x byte-compile-init-dir]

;; No garbage collection at startup
(setq gc-cons-threshold most-positive-fixnum)

;;; ..:: Package management ::..

(require 'package)
(setq package-enable-at-startup nil)
(setq package-check-signature nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

;; Install use-package
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; Install Quelpa
(unless (package-installed-p 'quelpa)
  (with-temp-buffer
    (url-insert-file-contents
     "https://raw.githubusercontent.com/quelpa/quelpa/master/quelpa.el")
    (eval-buffer)
    (quelpa-self-upgrade)))
(setq quelpa-update-melpa-p nil
      quelpa-upgrade-p nil
      quelpa-verbose nil
      quelpa-build-verbose nil)

(quelpa
 '(quelpa-use-package
   :fetcher git
   :url "https://github.com/quelpa/quelpa-use-package.git")
 :upgrade nil)
(require 'quelpa-use-package)

;;; ..:: Garbage collection ::..

;; 16MB of garbage collection space once running
(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold
				       (eval-when-compile
					 (* 1024 1024 100)))))

;; Garbage-collect on focus-out, Emacs should feel snappier.
;; Source: https://github.com/angrybacon/dotemacs/blob/master/
;;         dotemacs.org#load-customel
(add-hook 'focus-out-hook
	  (lambda ()
	    (unless (equal major-mode 'dashboard-mode)
	      (add-hook 'focus-out-hook #'garbage-collect))))

;; Garbage collection message

;;;###autoload
(defun danylo/gc-message ()
  "Garbage collection message."
  (when garbage-collection-messages
    (let ((message-log-max nil))
      (message "%s Collecting garbage"
	       (propertize (all-the-icons-faicon "trash-o")
			   'face `(:family
				   ,(all-the-icons-faicon-family)
				   :height 1.0)
			   'display '(raise -0.05))))))

(add-hook 'post-gc-hook (lambda () (danylo/gc-message) nil))

(setq garbage-collection-messages t)

(use-package gcmh
  ;; https://github.com/emacsmirror/gcmh"
  ;; The Garbage Collector Magic Hack
  :config
  (gcmh-mode 1))

;;; ..:: Keybinding management ::..

(use-package general
  ;; https://github.com/noctuid/general.el
  ;; More convenient key definitions in emacs
  )

;;; ..:: General usability ::..

;; Remove GUI elements
(when (window-system)
  (menu-bar-mode 0)
  (tool-bar-mode 0)
  (scroll-bar-mode 0)
  (tooltip-mode 0)
  (blink-cursor-mode 0))

;; Mouse behaviour

;; Frame size
(setq default-frame-alist
      (append default-frame-alist '((height . 50) (width . 100))))

;; Default directory
(setq default-directory "~/")

;; Remove warnings like: ad-handle-definition: ‘ansi-term’ got redefined
(setq ad-redefinition-action 'accept)

;; Comint unset C-up C-down
(global-unset-key (kbd "C-<up>"))
(global-unset-key (kbd "C-<down>"))

;; Lisp deprecation
;; https://github.com/kiwanami/emacs-epc/issues/35
(setq byte-compile-warnings '(cl-functions))

;; Better start screen
(use-package dashboard
  ;; https://github.com/emacs-dashboard/emacs-dashboard
  ;; An extensible emacs startup screen showing you what’s most important
  :diminish dashboard-mode
  :init
  (setq dashboard-banner-logo-title "Change the world, step by step"
	;; dashboard-startup-banner nil ; No image
	dashboard-items '((recents  . 5) (projects . 5) (bookmarks . 5))
	dashboard-center-content t
	dashboard-set-heading-icons t
	dashboard-set-file-icons t
	)
  :config
  (dashboard-setup-startup-hook))

;; A welcome message after startup
(defun display-startup-echo-area-message ())
(add-hook 'dashboard-mode-hook (lambda () (message "Welcome back")))

(setq inhibit-splash-screen t)
(setq initial-scratch-message ";; Change The World")

;; Keep init.el clean from custom-set-variable
(setq-default custom-file
	      (expand-file-name ".custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; Zoom in/out

(defvar danylo/default-font-size nil
  "The default font size when Emacs opens.")

;;;###autoload
(defun danylo/reset-font-size ()
  (interactive)
  (setq danylo/current-font-size (face-attribute 'default :height))
  (setq danylo/text-increment (- danylo/default-font-size danylo/current-font-size))
  (require 'default-text-scale)
  (default-text-scale-increment danylo/text-increment)
  )

(use-package default-text-scale
  ;; https://github.com/purcell/default-text-scale
  ;; Easily adjust the font size in all Emacs frames
  ;; Key bindings:
  ;;   C-M-= : zoom in
  ;;   C-M-- : zoom out
  :bind (("C-M--" . default-text-scale-decrease)
	 ("C-M-=" . default-text-scale-increase)
	 :map default-text-scale-mode-map
	 ("C-M-0" . danylo/reset-font-size))
  :init
  (add-hook 'after-init-hook
	    (lambda ()
	      (setq danylo/default-font-size
		    (face-attribute 'default :height))))
  :config
  (default-text-scale-mode))

(global-unset-key (kbd "C-x C--"))
(global-unset-key (kbd "C-x C-="))

;; Fix laggy point (cursor)
;; In particular: cursor freezing when moving down (next-line) for a while
;; https://emacs.stackexchange.com/questions/28736/emacs-pointcursor-movement-lag
(setq auto-window-vscroll nil)

;; Turn off Abbrev mode
(setq-default abbrev-mode nil)

;; Unbind Pesky Sleep Button
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

;; Disable system sounds
(setq ring-bell-function 'ignore)

;;;; Working with buffers

;;;###autoload
(defun danylo/revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive)
  (revert-buffer :ignore-auto :noconfirm))

(general-define-key
 "C-c b r" 'danylo/revert-buffer-no-confirm
 "C-x w" 'kill-buffer-and-window
 "C-c r" 'rename-buffer
 "C-c <prior>" 'previous-buffer  ; prior: page up
 "C-c <next>" 'next-buffer)      ; next: page down

;; Enable clipboard in emacs
(xterm-mouse-mode t)
(mouse-wheel-mode t)
(setq x-select-enable-clipboard t)

;;;; Backing up

;; Make sure Emacs doesn't break hard links
(setq backup-by-copying t)

;; Backup behaviour: store everything in single location
(defvar danylo/backup-dir "~/.emacs.d/backups/")
(setq backup-directory-alist (list (cons "." danylo/backup-dir)))

(defun danylo/byte-compile-init-dir ()
  "Byte-compile Emacs config."
  (interactive)
  (byte-recompile-file "~/.emacs.d/init.el" nil 0))

;;; ..:: Searching ::..

(defvar danylo/num-completion-candidates 15
  "How many completion candidates to display, tops.")

(use-package ivy-prescient
  ;; https://github.com/raxod502/prescient.el
  ;; Simple but effective sorting and filtering for Emacs.
  ;; For Ivy.
  :config
  (ivy-prescient-mode +1)
  ;; Save your command history on disk, so the sorting gets more
  ;; intelligent over time
  (prescient-persist-mode +1)
  )

(use-package selectrum-prescient
  ;; https://github.com/raxod502/prescient.el
  ;; Simple but effective sorting and filtering for Emacs.
  ;; For Selectrum.
  :config
  (selectrum-prescient-mode +1)
  ;; Save your command history on disk, so the sorting gets more
  ;; intelligent over time
  (prescient-persist-mode +1)
  )

;;;###autoload
(defun danylo/ivy-with-thing-at-point (cmd)
  "Auto-populate search with current thing at point.
Source: https://github.com/abo-abo/swiper/issues/1068"
  (let ((ivy-initial-inputs-alist
         (list
          (cons cmd (thing-at-point 'symbol)))))
    (funcall cmd)))

;;;###autoload
(defun danylo/swiper-thing-at-point ()
    "Put thing at point in swiper buffer"
    (interactive)
    (deactivate-mark)
    (danylo/ivy-with-thing-at-point 'swiper))

(use-package swiper
  ;; https://github.com/abo-abo/swiper
  ;; Swiper - isearch with an overview
  :after (ivy-prescient mini-frame)
  :bind (("M-i" . danylo/swiper-thing-at-point))
  :init
  (setq ;; Remove the default "^" in search string
	;; Source: https://emacs.stackexchange.com/a/38842/13661
	ivy-initial-inputs-alist nil
	ivy-height danylo/num-completion-candidates
	;; Wrap-around scroll C-n and C-p
	ivy-wrap t
	counsel-switch-buffer-preview-virtual-buffers nil
	ivy-truncate-lines t)
  (add-hook 'ivy-mode-hook
	    (lambda ()
	      (define-key ivy-mode-map (kbd "M-n") 'ivy-next-history-element)
	      (define-key ivy-mode-map (kbd "M-p") 'ivy-previous-history-element)))
  ;; After configuring `mini-frame-show-parameters` adjust the height
  ;; according to number of candidates
  (setf (alist-get 'height mini-frame-show-parameters) ivy-height)
  :config
  (ivy-mode 1))

(use-package selectrum
  ;; https://github.com/raxod502/selectrum
  ;; Incremental narrowing in Emacs
  :after mini-frame
  :init (setq selectrum-num-candidates-displayed
	      (- danylo/num-completion-candidates 1)
	      ;; Enable completion for projectile
	      projectile-completion-system 'default)
  :config
  (selectrum-mode +1)

  ;; Make sorting and filtering more intelligent
  (selectrum-prescient-mode +1)

  ;; Save your command history on disk, so the sorting gets more
  ;; intelligent over time
  (prescient-persist-mode +1)

  ;; After configuring `mini-frame-show-parameters` adjust the height
  ;; according to number of candidates
  (setf (alist-get 'height mini-frame-show-parameters)
	(1+ selectrum-num-candidates-displayed))
  )

;;; ..:: Code editing ::..

;; Commenting keybindings
(general-define-key
 "C-x C-;" 'comment-dwim
 "M-;" 'comment-line)

(use-package multiple-cursors
  ;; https://github.com/magnars/multiple-cursors.el
  ;; Multiple cursors for emacs.
  :bind
  (("C->" . mc/mark-next-like-this)
   ("C-<" . mc/mark-previous-like-this)
   ("C-*" . mc/mark-all-like-this)))

(use-package wgrep
  ;; https://github.com/mhayashi1120/Emacs-wgrep
  ;; Writable grep buffer and apply the changes to files.
  ;; workflow:
  ;;    1) Way 1: Do grep within project using grep-projectile
  ;;              "C-c p s g" then "C-x C-s" to save
  ;;              helm-grep results to grep buffer
  ;;       Way 2: Do grep on certain files of your choice using
  ;;              M-x rgrep
  ;;    2) In the grep buffer, active wgrep with "C-c C-p"
  ;;    3) Rename whatever you want by editing the names (e.g.
  ;;       use replace-string or multiple-curscors (see above))
  ;;    4) "C-c C-e" to save changes or "C-c C-k" to discard
  ;;       changes or "C-x C-q" to exit wgrep and prompt whether
  ;;       to save changes
  )

(use-package move-text
  ;; https://github.com/emacsfodder/move-text
  ;; Move current line or region up or down
  :config
  (move-text-default-bindings))

(use-package redo+
  ;;  Redo/undo system for Emacs
  :ensure nil
  :quelpa ((redo+ :fetcher github
		  :repo "emacsmirror/redo-plus"))
  :bind (("C-/" . undo)
	 ("C-?" . redo)))

(use-package hl-todo
  ;; https://github.com/tarsius/hl-todo
  ;; Highlight TODO keywords
  :config
  (global-hl-todo-mode)
  )

(use-package rainbow-mode
  ;; https://github.com/emacsmirror/rainbow-mode
  ;; Colorize color names in buffers
  :hook ((LaTeX-mode . (lambda () (rainbow-mode 1)))
	 (emacs-lisp-mode . (lambda () (rainbow-mode 1))))
  )

(use-package filladapt
  ;; https://elpa.gnu.org/packages/filladapt.html
  ;; Enhance the behavior of Emacs' Auto Fill mode
  :init (setq-default filladapt-mode t))

(use-package so-long
  ;; https://www.emacswiki.org/emacs/SoLong
  ;; Improve performance for long lines
  :config
  (global-so-long-mode 1))

;; Remove a significant contributor to line scan slowness
(setq bidi-display-reordering nil)

;; Delete trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Activate nlinum-mode (line numbers on the left)
(general-define-key
 "C-x n l" 'nlinum-mode)

;;;; Section delimiters

;;;###autoload
(defun danylo/section-msg (left msg right start end)
  "Section delimiters for comment.
LEFT and RIGHT are the section delimineters.
MSG is the section name.
START and END give the start/end of current selection."
  (interactive "r")
  (if (use-region-p)
      (let ((regionp (buffer-substring start end)))
	(delete-region start end)
	(insert (format "%s %s %s" left regionp right)))
    (insert (insert (format "%s %s %s" left msg right))))
  )

;;;###autoload
(defun danylo/code-section (start end)
  "Section delimiters for comment."
  (interactive "r")
  (danylo/section-msg "..::" "SECTION" "::.." start end))

;;;###autoload
(defun danylo/code-subsection (start end)
  "Subsection delimiters for comment."
  (interactive "r")
  (danylo/section-msg ">>" "SUBSECTION" "<<" start end))

;;;###autoload
(defun danylo/code-subsubsection (start end)
  "Subsubsection delimiters for comment."
  (interactive "r")
  (danylo/section-msg "@" "SUBSUBSECTION" "@" start end))

;; Duplicate line
(global-set-key (kbd "C-c d")
		(kbd "C-a C-SPC C-e M-w RET SPC C-SPC C-a <backspace> C-y"))

;; Require file ending with a newline
(setq require-final-newline t)

;;;; Delete words without putting them into kill ring

;;;###autoload
(defun danylo/forward-delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument, do this that many times.
This command does not push text to `kill-ring'."
  (interactive "p")
  (delete-region
   (point)
   (progn
     (forward-word arg)
     (point))))

;;;###autoload
(defun danylo/backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (backward-word arg) (point))))

(general-define-key
 "M-d" 'danylo/forward-delete-word
 "M-<backspace>" 'danylo/backward-delete-word)

;;; ..:: Window management ::..

;;;; >> Movement across windows <<

(general-define-key
 "C-<left>" 'windmove-left
 "C-<right>" 'windmove-right
 "C-<up>" 'windmove-up
 "C-<down>" 'windmove-down)

;;;; >> Resizing windows <<

(use-package windsize
  ;; https://github.com/grammati/windsize
  ;; Easy resizing of emacs windows
  :init (setq windsize-cols 1
	      windsize-rows 1)
  :config
  (windsize-default-keybindings))

;;;; >> Swapping windows between buffers <<

(use-package buffer-move
  ;; https://github.com/lukhas/buffer-move
  ;; Swap buffers between windows
  :bind (("S-M-<up>" . buf-move-up)
	 ("S-M-<down>" . buf-move-down)
	 ("S-M-<left>" . buf-move-left)
	 ("S-M-<right>" . buf-move-right)))

;; winner-mode, which lets you go back (C-c <left>) and forward (C-c <right>) in
;; window layout history
(when (fboundp 'winner-mode) (winner-mode 1))

;;;; >> Buffer menu <<

(general-define-key
 "C-x C-b" 'buffer-menu)

;; Move cursor to minibuffer (useful if lost focus, due to mouse click)

;;;###autoload
(defun danylo/switch-to-minibuffer-window ()
  "switch to minibuffer window (if active)"
  (interactive)
  (when (active-minibuffer-window)
    (select-frame-set-input-focus (window-frame (active-minibuffer-window)))
    (select-window (active-minibuffer-window))))

(general-define-key
 "C-x c b" 'danylo/switch-to-minibuffer-window)

;;;; >> Minibuffer in mini-frame <<

(use-package mini-frame
  ;; https://github.com/muffinmad/emacs-mini-frame
  ;; Show minibuffer in child frame on read-from-minibuffer
  :init (setq mini-frame-show-parameters '((top . 10)
					   (width . 0.7)
					   (left . 0.5))
	      mini-frame-create-lazy nil
	      mini-frame-color-shift-step 0)

  ;; workaround bug#44080, should be fixed in version 27.2 and above, see #169
  (define-advice fit-frame-to-buffer
      (:around (f &rest args) dont-skip-ws-for-mini-frame)
    (cl-letf* ((orig (symbol-function #'window-text-pixel-size))
               ((symbol-function #'window-text-pixel-size)
		(lambda (win from to &rest args)
                  (apply orig
			 (append (list win from
                                       (if (and (window-minibuffer-p win)
						(frame-root-window-p win)
						(eq t to))
                                           nil
					 to))
				 args)))))
      (apply f args)))
  :config
  (mini-frame-mode +1))

;;; ..:: Terminal emulator ::..

;;;###autoload
(defun danylo/run-terminator-here ()
  "Run terminal from current buffer"
  (interactive "@")
  (shell-command (concat "terminator > /dev/null 2>&1 & disown") nil nil))

(general-define-key
 "C-c t e" 'ansi-term
 "C-c t r" 'danylo/run-terminator-here)

;; Always use bash
(defadvice ansi-term (before force-bash)
  (interactive (list "/bin/bash")))
(ad-activate 'ansi-term)

;;;###autoload
(defun danylo/ansi-term-use-utf8 ()
  "Display of certain characters and control codes to UTF-8"
  (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))
(add-hook 'term-exec-hook 'danylo/ansi-term-use-utf8)

;; Clickable URLs
(add-hook 'term-mode-hook (lambda () (goto-address-mode)))

;; Make that typing exit in ansi-term (which exits the shell) also closes
;; the buffer
(defadvice term-sentinel (around my-advice-term-sentinel (proc msg))
  (if (memq (process-status proc)
	    '(signal exit))
      (let ((buffer (process-buffer proc)))
	ad-do-it
	(kill-buffer buffer))
    ad-do-it))
(ad-activate 'term-sentinel)

(defun danylo/set-no-process-query-on-exit ()
  "No 'are you sure' query on exit"
  (let ((proc (get-buffer-process (current-buffer))))
    (when (processp proc)
      (set-process-query-on-exit-flag proc nil))))
(add-hook 'term-exec-hook 'danylo/set-no-process-query-on-exit)

;; Key bindings

;;;###autoload


;;;###autoload
(defun danylo/switch-to-term-char-mode ()
  (interactive)
  (term-char-mode)
  (if buffer-read-only (toggle-read-only)))

(with-eval-after-load "term"
  (define-key term-raw-map (kbd "C-c r") 'rename-buffer)
  ;; Make sure typical key combos work in term-char-mode
  (define-key term-raw-map (kbd "M-x") 'nil)
  (define-key term-raw-map (kbd "M-&") 'nil)
  (define-key term-raw-map (kbd "M-!") 'nil)
  ;; Make sure C-c t e launches a new ansi-term buffer when current buffer is
  ;; also ansi-term
  (define-key term-raw-map (kbd "C-c t e") 'nil)
  ;; Word deletion fix
  (defun danylo/term-send-Mbackspace ()
    (interactive) (term-send-raw-string "\e\d"))
  (define-key term-raw-map (kbd "C-w") 'danylo/term-send-Mbackspace)
  (define-key term-raw-map (kbd "M-<backspace>") 'danylo/term-send-Mbackspace)
  ;; Completion via S-SPC as well as TAB
  (defun danylo/term-send-tab () (interactive) (term-send-raw-string "\t"))
  (define-key term-raw-map (kbd "S-SPC") 'danylo/term-send-tab)
  ;; Switch between char and line move
  (defun danylo/switch-to-term-line-mode () (interactive) (term-line-mode)
	 (if (not buffer-read-only) (toggle-read-only)))
  (defun danylo/switch-to-term-char-mode () (interactive) (term-char-mode)
	 (if (buffer-read-only) (toggle-read-only)))
  (define-key term-mode-map (kbd "C-c t c") 'danylo/switch-to-term-char-mode)
  (define-key term-raw-map (kbd "C-c t l") 'danylo/switch-to-term-line-mode)
  ;; Copy/paste native Emacs keystrokes
  (define-key term-raw-map (kbd "C-k") 'term-send-raw)
  (define-key term-raw-map (kbd "C-y") 'term-paste)
  ;; Ensure that scrolling doesn't break on output
  (setq term-scroll-to-bottom-on-output t)
  ;; Max history
  (setq term-buffer-maximum-size 50000)
  ;; Make sure window movement keys are not captured by terminal
  (define-key term-raw-map (kbd "C-<up>") 'nil)
  (define-key term-raw-map (kbd "C-<down>") 'nil)
  (define-key term-raw-map (kbd "C-<left>") 'nil)
  (define-key term-raw-map (kbd "C-<right>") 'nil)
  ;; Cursor movement key bindings
  (defun danylo/term-send-delete-word-forward ()
    (interactive) (term-send-raw-string "\ed"))
  (defun danylo/term-send-delete-word-backward ()
    (interactive) (term-send-raw-string "\e\C-h"))
  (defun danylo/term-send-m-right ()
    (interactive) (term-send-raw-string "\e[1;3C"))
  (defun danylo/term-send-m-left ()
    (interactive) (term-send-raw-string "\e[1;3D"))
  (define-key term-raw-map (kbd "C-<del>")
    'danylo/term-send-delete-word-forward)
  (define-key term-raw-map (kbd "C-<backspace>")
    'danylo/term-send-delete-word-backward)
  (define-key term-raw-map (kbd "M-<right>") 'danylo/term-send-m-right)
  (define-key term-raw-map (kbd "M-<left>") 'danylo/term-send-m-left))

;; Turn off line wrap
(add-hook 'term-mode-hook (lambda () (setq truncate-lines t)))

;; Bi-directional text support problem fix, which seems to be the cause of text
;; jumbling when going back commands in ansi-term. This fixes it, yay!
(add-hook 'term-mode-hook (lambda () (setq bidi-paragraph-direction 'left-to-right)))

;;; ..:: Theming and code aesthetics ::..

;; Auto-create matching closing parentheses
(electric-pair-mode 1)

(use-package rainbow-delimiters
  ;; https://github.com/Fanael/rainbow-delimiters
  ;; Highlights delimiters such as parentheses, brackets or braces
  ;; according to their depth.
  :hook ((prog-mode . rainbow-delimiters-mode)))

(use-package solaire-mode
  ;; https://github.com/hlissner/emacs-solaire-mode
  ;; Distinguish file-visiting buffers with slightly different background
  :hook ((change-major-mode . turn-on-solaire-mode))
  :config
  (solaire-global-mode +1))

(use-package nlinum
  ;; https://elpa.gnu.org/packages/nlinum.html
  ;; Show line numbers in the margin
  :config
  (setq nlinum-highlight-current-line nil))
(general-define-key
 "C-x n l" nlinum-mode)

(use-package doom-themes
  ;; https://github.com/hlissner/emacs-doom-themes
  ;; An opinionated pack of modern color-themes
  :config
  (load-theme 'doom-one t)
  (unless (window-system)
    (set-face-attribute 'region nil :background "#666"))
  )

(add-hook 'after-init-hook
	  (lambda ()
	    (when (window-system)
	      (set-face-attribute 'default nil
				  :family "Fira Code"
				  :height 110
				  :weight 'normal
				  :width 'normal))))

(use-package fira-code-mode
  ;; https://github.com/jming422/fira-code-mode
  ;; Fira Code ligatures using prettify-symbols
  :ensure t
  :custom (fira-code-mode-disabled-ligatures
	   '("[]" "#{" "#(" "#_" "#_(" "x" "&&"))
  :config
  (add-hook 'prog-mode-hook (lambda () (when (window-system) (fira-code-mode))))
  )

;; Speed up font-lock mode speed (can causes laggy scrolling)
(setq font-lock-support-mode 'jit-lock-mode
      jit-lock-stealth-time 16
      jit-lock-defer-contextually t
      jit-lock-contextually nil
      jit-lock-stealth-nice 0.5
      font-lock-maximum-decoration 1)

;;;###autoload
(defun danylo/modeline-setup ()
  "Configure the modeline."
  (setq telephone-line-primary-left-separator 'telephone-line-cubed-left
	telephone-line-secondary-left-separator 'telephone-line-cubed-hollow-left
	telephone-line-primary-right-separator 'telephone-line-cubed-right
	telephone-line-secondary-right-separator 'telephone-line-cubed-hollow-right)

  (defface my-telephone-yellow
    '((t (:foreground "black" :background "yellow"))) "")

  (add-to-list 'telephone-line-faces
	       '(yellow . (my-telephone-yellow . telephone-line-accent-inactive)))

  (setq telephone-line-lhs
	'((yellow . (telephone-line-vc-segment))
	  (nil    . (telephone-line-buffer-segment))))

  (setq telephone-line-rhs
	'((nil    . (telephone-line-misc-info-segment))
	  (accent . (telephone-line-major-mode-segment))
	  (evil   . (telephone-line-airline-position-segment))))

  ;; Activate
  (telephone-line-mode 1)
  )

(use-package telephone-line
  ;; https://github.com/dbordak/telephone-line
  ;; Telephone Line is a new implementation of Powerline for emacs
  :after (doom-themes)
  :config
  (add-hook 'after-init-hook (danylo/modeline-setup))
  )

(use-package all-the-icons
  ;; https://github.com/domtronn/all-the-icons.el
  ;; Pretty icons
  :config
  (unless (member "all-the-icons" (font-family-list))
    (all-the-icons-install-fonts t))
  ;; Fix MATLAB icon
  (setq all-the-icons-icon-alist
	(add-to-list 'all-the-icons-icon-alist
		     '("\\.m$" all-the-icons-fileicon "matlab"
		       :face all-the-icons-orange)))
  )

(use-package mic-paren
  ;; https://github.com/emacsattic/mic-paren
  ;; Advanced highlighting of matching parentheses
  :config
  (paren-activate))

;;; ..:: Syntax checking ::..

(use-package flycheck
  ;; https://github.com/flycheck/flycheck
  ;; A modern on-the-fly syntax checking extension
  :hook ((c-mode-common . flycheck-mode)
	 (python-mode . flycheck-mode)
	 (sh-mode . flycheck-mode))
  :bind (("C-c f l" . flycheck-list-errors)
	 ("C-c f e" . flycheck-display-error-at-point)
	 ("C-c f v" . flycheck-verify-setup))
  :init
  (setq flycheck-enabled-checkers '(c/c++-gcc)
	flycheck-check-syntax-automatically '(mode-enabled save)
	flycheck-display-errors-delay 0.1))

;;; ..:: Completion ::..

(use-package company
  ;; https://github.com/company-mode/company-mode
  ;;  Modular in-buffer completion framework for Emacs
  :hook ((c-mode-common . company-mode)
	 (emacs-lisp-mode . company-mode)
	 (comint-mode . company-mode)
	 (LaTeX-mode . company-mode)
	 (sh-mode . company-mode)
	 (matlab-mode . company-mode))
  :bind (("<S-SPC>" . company-complete))
  :init
  (setq company-dabbrev-downcase 0
	company-async-timeout 10
	company-require-match 'never
	company-minimum-prefix-length 0
	company-auto-complete nil
	company-idle-delay nil
	company-auto-select-first-candidate nil)
  )

(use-package company-shell
  ;; https://github.com/Alexander-Miller/company-shell
  ;; company mode completion backends for your shell functions
  :config
  (add-to-list 'company-backends 'company-shell)
  )

;;; ..:: File management ::..

(use-package projectile
  ;; https://github.com/bbatsov/projectile
  ;; Project interaction library offering tools to operate on a project level
  :init (setq projectile-enable-caching nil)
  :bind (:map projectile-mode-map
	      ("C-c p" . projectile-command-map))
  :config
  (setq projectile-globally-ignored-directories
	(append '(".svn" ".git")
		projectile-globally-ignored-directories)
	projectile-globally-ignored-files
	(append '(".DS_Store" ".gitignore")
		projectile-globally-ignored-files))
  (projectile-global-mode))

;;; ..:: Git ::..

(use-package magit
  ;; https://github.com/magit/magit
  ;; An interface to the version control system Git
  :bind (("C-x g" . magit-status))
  )

;;; ..:: Language server protocol ::..

;; Increase the amount of data which Emacs reads from the process
;; Source: https://emacs-lsp.github.io/lsp-mode/page/performance/
(setq read-process-output-max (* 1024 1024 10)) ;; 10mb

(use-package lsp-mode
  ;; https://github.com/emacs-lsp/lsp-mode
  ;; Emacs client/library for the Language Server Protocol
  :init
  (setq lsp-keymap-prefix "C-c l"
	;; Turn off the annoying progress spinner
	lsp-progress-via-spinner nil
	;; Turn off LSP-mode imenu
	lsp-enable-imenu nil
	;; Optional: fine-tune lsp-idle-delay. This variable determines how
	;; often lsp-mode will refresh the highlights, lenses, links, etc
	;; while you type.
	lsp-idle-delay 0.5
	;; Use clangd instead
	;; https://www.reddit.com/r/cpp/comments/cafj21/ccls_clangd_and_cquery/
	;; lsp-disabled-clients '(ccls)
	;; lsp-clients-clangd-executable "clangd"
	;; Use company-capf for completion. Although company-lsp also
	;; supports caching lsp-mode’s company-capf does that by default. To
	;; achieve that uninstall company-lsp or put these lines in your
	;; config:
	lsp-prefer-capf t
	lsp-prefer-flymake nil
	;; Linting
	lsp-diagnostic-package :none
	;; Diagnostics turn off
	lsp-modeline-diagnostics-scope :none
	;; Other niceties
	lsp-signature-auto-activate nil
	lsp-signature-doc-lines 1
	lsp-eldoc-enable-hover nil ;; Show variable info on hover
	eldoc-idle-delay 0 ;; Delay before showing variable info on hover
	lsp-enable-semantic-highlighting t
	lsp-enable-snippet nil ;; Enable arguments completion
	)
  :hook
  ((lsp-mode . (lambda ()
		 (setq company-minimum-prefix-length 1)
		 (push 'company-capf company-backends))))
  )

;;; ..:: C/C++ ::..

;;;; Code editing
(add-hook 'c-mode-common-hook (lambda () (setq c-auto-newline nil)))

(use-package modern-cpp-font-lock
  ;; https://github.com/ludwigpacifici/modern-cpp-font-lock
  ;; C++ font-lock for Emacs
  :hook ((c++-mode . modern-c++-font-lock-mode)))

(use-package srefactor
  ;; https://github.com/tuhdo/semantic-refactor
  ;; C/C++ refactoring tool based on Semantic parser framework
  :bind (:map c-mode-base-map
	      ("M-RET" . srefactor-refactor-at-point))
  :hook ((c-mode-common . semantic-mode))
  :init
  (setq srefactor-ui-menu-show-help nil))

(use-package ccls
  ;; https://github.com/MaskRay/emacs-ccls
  ;; Emacs client for ccls, a C/C++ language server
  :after lsp-mode
  :hook ((c-mode-common . lsp))
  :init
  (setq ccls-executable "ccls" ; Must be on PATH: assume /usr/local/bin/ccls
        ))

;;; ..:: Python ::..

;;;; Code editing

(setq python-indent-guess-indent-offset t
      python-indent-guess-indent-offset-verbose nil)

;;;;; Imenu setup

;;;###autoload
(defun danylo/python-imenu ()
  (interactive)
  (let ((python-imenu (imenu--generic-function
		       imenu-generic-expression)))
    (append python-imenu)))

;;;###autoload
(defun danylo/python-imenu-hooks ()
  (interactive)
  (setq imenu-generic-expression
	'(("Function" "^[[:blank:]]*def \\(.*\\).*(.*$" 1)
	  ("Class" "^class \\(.*\\).*:$" 1)))
  (setq imenu-create-index-function 'danylo/python-imenu)
  ;; Rescan the buffer as contents are added
  (setq imenu-auto-rescan t)
  )

(add-hook 'python-mode-hook 'danylo/python-imenu-hooks)

;;;; LSP

(use-package lsp-pyright
  ;; https://github.com/emacs-lsp/lsp-pyright
  ;; lsp-mode client leveraging Pyright language server
  :hook ((python-mode . (lambda () (require 'lsp-pyright) (lsp))))
  :init
  (setq
   ;; Do not print "analyzing... Done" in the minibuffer
   ;; (I find it distracting)
   lsp-pyright-show-analyzing nil
   ;; Disable auto-import completions
   lsp-pyright-auto-import-completions nil
   ))

;; Configure Flycheck
(add-hook 'python-mode-hook
	  (lambda ()
	    (setq flycheck-enabled-checkers '(python-flake8))
	    (setq flycheck-disabled-checkers
		  '(python-mypy
		    python-pylint
		    python-pyright))))

;;;###autoload
(defun danylo/lsp-eldoc-toggle-hover ()
  "Toggle variable info"
  (interactive)
  (if lsp-eldoc-enable-hover
      (progn (setq lsp-eldoc-enable-hover nil)
	     (eldoc-mode -1))
    (progn (setq lsp-eldoc-enable-hover t)
	   (eldoc-mode 1))))

;;;###autoload
(defun danylo/lsp-variable-info-message (string)
  "Display variable info"
  (message "%s %s"
	   (propertize (all-the-icons-faicon "info")
		       'face `(:family ,(all-the-icons-faicon-family)
				       :height 1.0
				       :background nil
				       :foreground "yellow")
		       'display '(raise -0.05))
	   (propertize string 'face '(:background nil
						  :foreground "yellow"))))

;;;###autoload
(defun danylo/lsp-display-variable-info ()
  (interactive)
  (if (not (looking-at "[[:space:]\n]"))
      (when (lsp--capability :hoverProvider)
        (lsp-request-async
         "textDocument/hover"
         (lsp--text-document-position-params)
         (-lambda ((hover &as &Hover? :range? :contents))
	   (when hover
	     (danylo/lsp-variable-info-message
	      (and contents (lsp--render-on-hover-content
			     contents
			     lsp-eldoc-render-all)))))
         :error-handler #'ignore
         :mode 'tick
         :cancel-token :eldoc-hover))))

(general-define-key
 :prefix "C-c l"
 :keymaps 'lsp-mode-map
 "i" 'danylo/lsp-eldoc-toggle-hover
 "v" 'danylo/lsp-display-variable-info)

;;;; Python shell interaction

(defcustom danylo/python-shell-type "ipython"
  "Which shell type to use?."
  :type 'string)

(defcustom danylo/python-buffer-name "*PythonProcess*"
  "Name of the Python buffer."
  :type 'string)

(defcustom danylo/python-shell-position '((side . right))
  "Position of the python shell."
  :type 'alist)

;;;###autoload
(defun danylo/python-check-open-shell ()
  "Check if a shell is running."
  (if (get-buffer danylo/python-buffer-name) t nil))

;;;###autoload
(defun danylo/python-shell ()
  "Create a Python shell.
Placed in the current buffer."
  (interactive)
  (if (danylo/python-check-open-shell)
      (progn
	;; The Python shell buffer exists
	(if (get-buffer-window danylo/python-buffer-name)
	    ;; The buffer is already displayed, switch to it
	    (progn
	      (pop-to-buffer danylo/python-buffer-name))
	  ;; The buffer is hidden, show it
	  (progn
	    (switch-to-buffer danylo/python-buffer-name)
	    (display-buffer-in-side-window
	     danylo/python-buffer-name danylo/python-shell-position)
	    (switch-to-buffer (other-buffer))
	    ))
	)
    (progn
      ;; The Python shell buffer does not exist
      (let ((this-buffer (buffer-name)))
	(progn
	  ;; Create a shell buffer
	  (ansi-term danylo/python-shell-type)
	  (rename-buffer danylo/python-buffer-name)
	  ;; Create a side window to hold the vterm buffer
	  (display-buffer-in-side-window
	   danylo/python-buffer-name danylo/python-shell-position)
	  ;; Switch back to current buffer
	  (switch-to-buffer this-buffer)
	  )
	)
      )
    )
  )

;;;###autoload
(defun danylo/python-run-file ()
  "Run the current file in the python shell.
If there is to python shell open, prints a message to inform."
  (interactive)
  (if (danylo/python-check-open-shell)
      (progn
	;; Send a run command for the current file
	(let ((file-name (file-name-nondirectory (buffer-file-name)))
	      (this-buffer (buffer-name)))
	  (progn
	    (with-current-buffer danylo/python-buffer-name
	      (term-send-raw-string (format "%%run -i %s\n" file-name))))
	  )
	)
    (message "No Python buffer.")))

;;;###autoload
(defun danylo/python-config ()
  ;; Key bindings
  (define-key python-mode-map (kbd "C-c C-p") 'danylo/python-shell)
  (define-key python-mode-map (kbd "C-c C-l") 'danylo/python-run-file)
  ;; Hide shell buffers from buffer list, so they may only be accessed
  ;; through the above key bindings
  ;; See https://github.com/abo-abo/swiper/issues/644
  (let ((buf-name (replace-regexp-in-string
		   "*+" "\\\\*" danylo/python-buffer-name t t)))
    (setq ivy-ignore-buffers `(,buf-name))))

(add-hook 'python-mode-hook (lambda () (danylo/python-config)))

;;; ..:: Julia ::..

(use-package ess
  ;; https://github.com/emacs-ess/ESS
  ;; Emacs Speaks Statistics
  :hook (ess-julia-mode . (lambda () (setq set-mark-command-repeat-pop t)))
  :init (setq ess-use-company t
	      ess-tab-complete-in-script t))

(with-eval-after-load "ess-julia"
  (define-key ess-julia-mode-map (kbd "C-u C-j") 'run-ess-julia)
  (define-key ess-julia-mode-map (kbd "S-SPC") 'complete-symbol)
  (define-key ess-julia-mode-map (kbd "C-u C-x C-;") 'uncomment-region)
  (define-key ess-julia-mode-map (kbd "C-u C-c C-l") 'julia-latexsub-or-indent)
  (define-key ess-julia-mode-map (kbd "C-u C-SPC") (lambda () (set-mark-command -1)))
  (define-key inferior-ess-julia-mode-map
    (kbd "C-u C-c C-l") 'julia-latexsub-or-indent))

;;; ..:: MATLAB ::..

(use-package s
  ;; https://github.com/magnars/s.el
  ;; The long lost Emacs string manipulation library.
  )

(use-package matlab
  ;; https://github.com/dmalyuta/matlab-emacs
  ;; MATLAB and Emacs integration
  :ensure nil
  :quelpa ((matlab :fetcher github
		   :repo "dmalyuta/matlab-emacs"))
  :init
  (setq matlab-verify-on-save-flag nil
	matlab-indent-function-body nil)
  )

;;;###autoload
(defun danylo/matlab-view-doc ()
  "Look up the matlab help info and show in another buffer."
  (interactive)
  (let* ((word (doc-matlab-grab-current-word)))
    (matlab-shell-describe-command word)))

(use-package matlab-mode
  ;; https://github.com/dmalyuta/matlab-mode
  ;; An emacs matlab mode
  :after company
  :ensure nil
  :quelpa ((matlab-mode :fetcher github
			:repo "dmalyuta/matlab-mode"))
  :bind (:map matlab-mode-map
	      ("M-s" . matlab-shell)
	      ("C-c h" . danylo/matlab-view-doc)
	      ("C-c s" . matlab-jump-to-definition-of-word-at-cursor)
	      :map matlab-shell-mode-map
	      ("S-SPC" . matlab-shell-tab)
	      ("C-c h" . danylo/matlab-view-doc))
  :hook ((matlab-mode . (lambda () (setq-local company-backends
					       '((company-files
						  company-matlab
						  company-dabbrev)))))))

;;; ..:: Lisp ::..

;; Turn off (by default) function signatures popping up in minibuffer
(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    (eldoc-mode 0)))

;;; ..:: LaTeX ::..

(use-package tex
  ;; https://elpa.gnu.org/packages/auctex.html
  ;; Package for writing and formatting TeX files
  :ensure auctex
  :hook ((LaTeX-mode . LaTeX-math-mode)
	 (LaTeX-mode . turn-on-reftex)
	 (LaTeX-mode . TeX-source-correlate-mode)
	 (LaTeX-mode . TeX-PDF-mode)
	 (LaTeX-mode . TeX-source-correlate-mode)
	 (LaTeX-mode . auto-fill-mode)
	 (LaTeX-mode . (lambda () (setq fill-column 80)))
	 ;; View program
	 (LaTeX-mode . (lambda ()
			 (setq TeX-view-program-list
			       '(("Evince"
				  "evince --page-index=%(outpage) %o")))
			 (setq TeX-view-program-selection
			       '((output-pdf "Evince")))))
	 ;; Other environments
	 (LaTeX-mode . (lambda ()
			 (LaTeX-add-environments "equation*")
			 (LaTeX-add-environments "tikzpicture")
			 (LaTeX-add-environments "pgfonlayer")))
	 ;; Line-breaking math
	 (LaTeX-mode . (lambda ()
			 (add-to-list 'fill-nobreak-predicate
				      'texmathp))))
  :init
  (setq TeX-source-correlate-method 'synctex
	TeX-auto-save t
	TeX-parse-self t
	TeX-auto-regexp-list 'TeX-auto-full-regexp-list
	TeX-auto-parse-length 999999
	TeX-save-query nil
	TeX-master nil
	reftex-plug-into-AUCTeX t
	LaTeX-electric-left-right-brace t
	TeX-electric-math '("$" . "$"))
  :bind (:map LaTeX-mode-map
	      ("M-s" . ispell-word)
	      ("C-x C-<backspace>" . electric-pair-delete-pair))
  :config
  (require 'latex)
  ;; Shell-escape compilation
  (eval-after-load 'tex
    '(setcdr (assoc "LaTeX" TeX-command-list)
	     '("%`%l%(mode) -shell-escape%' %t"
	       TeX-run-TeX nil (latex-mode doctex-mode) :help "Run LaTeX")))

  ;; Nomenclature compilation
  (eval-after-load "tex"
    '(add-to-list 'TeX-command-list
		  '("Nomenclature"
		    "makeindex %s.nlo -s nomencl.ist -o %s.nls"
		    (lambda (name command file)
		      (TeX-run-compile name command file)
		      (TeX-process-set-variable
		       file 'TeX-command-next TeX-command-default))
		    nil t :help "Create nomenclature file")))

  ;; Correct environment selection and indentation
  (add-to-list 'LaTeX-verbatim-environments "pycode")
  (add-to-list 'LaTeX-verbatim-environments "pykzmathinline")
  (add-to-list 'LaTeX-verbatim-environments "pykzmathblock")
  (add-to-list 'LaTeX-verbatim-environments "@pie@shell")
  (add-to-list 'LaTeX-indent-environment-list '("pycode" current-indentation))
  (add-to-list 'LaTeX-indent-environment-list '("pykzmathblock" current-indentation))
  (add-to-list 'LaTeX-indent-environment-list '("@pie@shell" current-indentation)))

(use-package company-auctex
  ;; https://github.com/alexeyr/company-auctex
  ;; company-mode autocompletion for auctex
  :after company
  :hook ((LaTeX-mode . (lambda ()
			 ;; Make completion case sensitive
			 (setq company-dabbrev-downcase nil))))
  :bind (:map LaTeX-mode-map
	      ("C-c m" . company-auctex-macros)
	      ("C-c e" . company-auctex-environments)
	      ("C-c l" . company-auctex-labels)
	      ("C-c s" . company-auctex-symbols))
  :config
  (company-auctex-init)
  )

;;; ..:: Gnuplot ::..

(use-package gnuplot
  ;; https://github.com/emacsorphanage/gnuplot
  ;; A major mode for Emacs for interacting with Gnuplot
  :mode (("\\.gp$" . gnuplot-mode)
	 ("\\.gnu$" . gnuplot-mode))
  :config
  ;; these lines enable the use of gnuplot mode
  (autoload 'gnuplot-mode "gnuplot" "gnuplot major mode" t)
  (autoload 'gnuplot-make-buffer "gnuplot" "open a buffer in gnuplot mode" t))

;;; ..:: YAML ::..

(use-package yaml-mode
  ;; https://github.com/yoshiki/yaml-mode
  ;; Major mode for editing files in the YAML data serialization format.
  :mode (("\\.yml$" . yaml-mode)
	 ("\\.yaml$" . yaml-mode)))

;;; ..:: Markdown ::..

(use-package markdown-mode
  ;; https://github.com/jrblevin/markdown-mode
  ;; Markdown editing major mode
  :mode (("README\\.md\\'" . gfm-mode)
	 ("\\.md\\'" . markdown-mode)
	 ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))
