;;; init.el
;; To byte-compile: [M-x byte-compile-init-dir]

;; No garbage collection at startup
(setq gc-cons-threshold most-positive-fixnum)

;;; ..:: Package management ::..

(require 'package)
(setq package-enable-at-startup nil)
(setq package-check-signature nil)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

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

;;; ..:: Customization variables in init.el ::..

(defgroup danylo nil
  "My customization variables for the init.el file."
  :group 'local)

(defcustom danylo/gc-cons-threshold `,(* 1024 1024 100)
  "Name of ivy candidate list buffer"
  :type 'integer
  :group 'danylo)

(defcustom danylo/gc-collect-print t
  "Print message in minibuffer on garbage collection."
  :type 'boolean
  :group 'danylo)

(defcustom danylo/python-shell-type "ipython"
  "Which shell type to use for Python."
  :type 'string
  :group 'danylo)

(defcustom danylo/julia-shell-type "julia"
  "Which shell type to use for Julia."
  :type 'string
  :group 'danylo)

(defcustom danylo/python-buffer-name "*PythonProcess*"
  "Name of the Python shell buffer."
  :type 'string
  :group 'danylo)

(defcustom danylo/julia-buffer-name "*JuliaProcess*"
  "Name of the Julia shell buffer."
  :type 'string
  :group 'danylo)

(defcustom danylo/ivy-window-name "*ivy-candidate-window*"
  "Name of ivy candidate list buffer."
  :type 'string
  :group 'danylo)

(defcustom danylo/latex-preview-scale 1.3
  "Size of the latex preview in Org mode."
  :type 'float
  :group 'danylo)

(defcustom danylo/latex-equation-envs
  `,(concat "equation\\|align\\|alignat"
	    "\\|multline\\|subequations"
	    "\\|optimization\\|gather")
  "All types of LaTeX equation environments to be used for font locking"
  :group 'danylo)

(defface danylo/latex-equation-face-main
  '((t (:foreground "orange"
		    :weight bold
		    :inherit default)))
  "Face for org-mode equations."
  :group 'org-faces)

(defface danylo/latex-equation-face-faded
  '((t (:foreground "#7d5c1f"
		    :weight bold
		    :inherit default)))
  "Face for org-mode equation delimiters."
  :group 'org-faces)

(defface danylo/telephone-yellow
  '((t (:foreground "black"
		    :background "yellow")))
  "Face for mode line."
  :group 'danylo)

;;; ..:: General helper functions ::..

;;;###autoload
(defun danylo/fa-icon (icon &optional fg)
  "Fontawesome icon with proper formatting for minibuffer"
  (unless fg
    (setq fg `,(face-attribute 'default :foreground)))
  (if (window-system)
      (propertize (all-the-icons-faicon icon)
		  'face `(:family ,(all-the-icons-faicon-family)
				  :height 1.0
				  :foreground ,fg)
		  'display '(raise -0.05))
    ""))

;;; ..:: Garbage collection ::..

;; 100MB of garbage collection space once running
(add-hook 'after-init-hook
	  (lambda () (setq gc-cons-threshold danylo/gc-cons-threshold)))

;; Garbage collection message

;;;###autoload
(defun danylo/gc-message ()
  "Garbage collection message."
  (when (and danylo/gc-collect-print
	     (not (active-minibuffer-window)))
    (let ((message-log-max nil))
      ;; Print "<TRASH_ICON> GC"
      (message "%s %s" (danylo/fa-icon "trash-o" "#464c5d")
	       (propertize "GC" 'face '(:foreground "#464c5d")))
      )))

(add-hook 'post-gc-hook (lambda () (danylo/gc-message)))

(setq garbage-collection-messages nil)

(use-package gcmh
  ;; https://github.com/emacsmirror/gcmh"
  ;; The Garbage Collector Magic Hack
  :init (setq gcmh-high-cons-threshold danylo/gc-cons-threshold)
  :config
  (gcmh-mode 1))

;;; ..:: Keybinding management ::..

(use-package general
  ;; https://github.com/noctuid/general.el
  ;; More convenient key definitions in emacs
  )

;;; ..:: General usability ::..

;; Remove GUI elements
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(tooltip-mode 0)
(blink-cursor-mode 0)

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

;; Move by paragraph
(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-p") 'backward-paragraph)

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

;;;; Dired (directory listing)

(use-package dired+
  ;;  Redo/undo system for Emacs
  :ensure nil
  :quelpa ((dired+ :fetcher url
		   :url "https://raw.githubusercontent.com/emacsmirror/dired-plus/master/dired+.el"))
  :bind (:map dired-mode-map
	      ("M-i" . nil))
  :init
  (setq dired-listing-switches "-alh"
	diredp-hide-details-initially-flag nil)
  :config
  (diredp-toggle-find-file-reuse-dir t))

;;;; Search Google

(use-package google-this
  ;; https://github.com/Malabarba/emacs-google-this
  ;; A set of emacs functions and bindings to google under point.
  :config
  (google-this-mode 1))

;;;; Asynchronous behaviour

(use-package async
  ;; https://github.com/jwiegley/emacs-async
  ;; Simple library for asynchronous processing in Emacs
  )

;;; ..:: Searching ::..

(defvar danylo/num-completion-candidates 15
  "How many completion candidates to display, tops.")

;;;###autoload
(defun danylo/swiper-thing-at-point ()
    "Put thing at point in swiper buffer"
    (interactive)
    (deactivate-mark)
    (swiper (thing-at-point 'symbol)))

;;;###autoload
(defun danylo/counsel-switch-buffer-no-preview ()
  "Switch to another buffer without preview."
  (interactive)
  (ivy-switch-buffer))

;;;###autoload
(defun danylo/ivy-display-function-window (text)
  "Show ivy candidate completion list in a temporary buffer, like Helm."
  (when (> (length text) 0)
    (let ((buffer (get-buffer-create danylo/ivy-window-name)))
      (with-current-buffer buffer
	(setq cursor-type nil) ;; Hide cursor
	(let ((inhibit-read-only t))
	  (erase-buffer)
	  (setq danylo/completion-candidate-list
		;; Remove the first character, which is '\n' (blank line)
		(substring text 1 nil))
	  (insert danylo/completion-candidate-list)
	  )
	)
      (with-ivy-window
	(let ((window (selected-window)))
	  (if (window-parameter window 'window-side)
	      ;; In side window
	      ;; Cannot use display-buffer-below-selected because a
	      ;; side window cannot be split (most likely)
	      (display-buffer
	       buffer
	       `((display-buffer-reuse-window display-buffer-pop-up-window)
		 (window-height . ,(ivy--height (ivy-state-caller ivy-last)))))
	    ;; In regular window
	    (display-buffer
	     buffer
	     `((display-buffer-reuse-window display-buffer-below-selected)
	       (window-height . ,(ivy--height (ivy-state-caller ivy-last))))))))
      )
    )
  )

(defun danylo/side-window-tmp ()
  (interactive)
  (display-buffer-in-side-window (get-buffer "*Messages*") '((side . right))))

(use-package ivy-prescient
  ;; https://github.com/raxod502/prescient.el
  ;; Simple but effective sorting and filtering for Emacs.
  ;; For Ivy.
  :config
  (ivy-prescient-mode +1)
  ;; Save your command history on disk, so the sorting gets more
  ;; intelligent over time
  (prescient-persist-mode +1))

(use-package counsel)
(use-package swiper)
(use-package ivy
  ;; https://github.com/abo-abo/swiper
  ;; Ivy - a generic completion frontend for Emacs
  :after (company counsel swiper ivy-prescient)
  :bind (("M-i" . danylo/swiper-thing-at-point)
	 ("C-x b" . danylo/counsel-switch-buffer-no-preview)
	 ("M-x" . counsel-M-x)
	 ("C-c q" . counsel-semantic-or-imenu)
	 :map company-mode-map
	 ("S-SPC" . counsel-company)
	 :map ivy-minibuffer-map
	 ("C-l" . ivy-backward-delete-char)
	 ("TAB" . ivy-alt-done))
  :init
  (setq ivy-display-functions-alist
	'((t . danylo/ivy-display-function-window))
	ivy-use-virtual-buffers nil
	enable-recursive-minibuffers t
	;; Remove the default "^" in search string
	;; Source: https://emacs.stackexchange.com/a/38842/13661
	ivy-initial-inputs-alist nil
	ivy-height danylo/num-completion-candidates
	;; Wrap-around scroll C-n and C-p
	ivy-wrap t
	counsel-switch-buffer-preview-virtual-buffers nil
	ivy-truncate-lines t
	ivy-display-style 'fancy)
  (custom-set-faces
   '(ivy-minibuffer-match-face-1
     ((t (:background "orange"
		      :foreground "black"
		      :weight normal))))
   '(swiper-match-face-1
     ((t (:background "orange"
		      :foreground "black")))))
  (add-to-list 'ivy-ignore-buffers '"\\\\*ivy-")
  :config
  (ivy-mode 1))

(use-package helm
  ;; https://emacs-helm.github.io/helm/
  ;; Emacs incremental completion and selection narrowing framework
  :bind (("C-x C-f" . helm-find-files)
	 :map helm-map
	 ("TAB" . helm-execute-persistent-action))
  :init (setq helm-display-buffer-default-height
	      danylo/num-completion-candidates
	      helm-mode-line-string ""
	      helm-comp-read-mode-line ""
	      helm-buffer-max-length 30
	      helm-buffers-truncate-lines nil
	      helm-split-window-in-side-p t
	      helm-move-to-line-cycle-in-source t
	      helm-echo-input-in-header-line nil
	      helm-display-header-line nil
	      helm-buffers-fuzzy-matching t
	      helm-ff-file-name-history-use-recentf t
	      helm-recentf-matching t
	      history-delete-duplicates t)
  (add-hook 'helm-find-files-after-init-hook
	    (lambda ()
	      (set-face-attribute 'helm-ff-directory nil
				  :foreground "yellow"
				  :bold t
				  :extend nil)))
  (add-to-list 'ivy-ignore-buffers '"\\\\*helm")
  :config
  (helm-mode 1))

(use-package counsel-projectile
  ;; https://github.com/ericdanan/counsel-projectile
  ;; Ivy UI for Projectile
  :init (setq projectile-completion-system 'ivy))

(use-package counsel-gtags
  ;; https://github.com/syohex/emacs-counsel-gtags
  ;; GNU Global with ivy completion
  :bind
  (:map c-mode-base-map
	("M-." . counsel-gtags-dwim)))

(use-package ivy-xref
  ;; https://github.com/alexmurray/ivy-xref
  ;; Ivy interface for xref results
  :init
  (setq xref-show-definitions-function #'ivy-xref-show-defs))

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

;;;###autoload
(defun danylo/undo-no-gc ()
  "Undo function. Turns off GC prior to undoing."
  (interactive)
  (let ((gc-cons-threshold most-positive-fixnum))
    ;; GC "turned off" in this block by setting gc-cons-threshold very
    ;; high
    (undo-tree-undo)))

(use-package undo-tree
  ;; http://www.dr-qubit.org/undo-tree/undo-tree.el
  ;; Treat undo history as a tree
  :bind (:map undo-tree-map
	      ("C-/" . danylo/undo-no-gc)))

(add-hook 'after-init-hook
	  (lambda ()
	    (require 'undo-tree)
	    (global-undo-tree-mode)))

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
(with-eval-after-load "nlinum"
  (general-define-key
   "C-x n l" 'nlinum-mode))

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
    (insert (format "%s %s %s" left msg right)))
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

;;;###autoload
(defun danylo/set-no-process-query-on-exit ()
  "No 'are you sure' query on exit"
  (let ((proc (get-buffer-process (current-buffer))))
    (when (processp proc)
      (set-process-query-on-exit-flag proc nil))))
(add-hook 'term-exec-hook 'danylo/set-no-process-query-on-exit)

;; Key bindings

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
  (defun danylo/switch-to-term-line-mode () (interactive)
	 (term-line-mode)
	 (read-only-mode +1))
  (defun danylo/switch-to-term-char-mode () (interactive)
	 (term-char-mode)
	 (read-only-mode 0))
  (define-key term-raw-map (kbd "C-c t c") 'danylo/switch-to-term-char-mode)
  (define-key term-raw-map (kbd "C-c t l") 'danylo/switch-to-term-line-mode)
  (define-key term-mode-map (kbd "C-c t c") 'danylo/switch-to-term-char-mode)
  (define-key term-mode-map (kbd "C-c t l") 'danylo/switch-to-term-line-mode)
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

(use-package eterm-256color
  ;; https://github.com/dieggsy/eterm-256color
  ;; Customizable 256 colors for emacs term and ansi-term
  :after xterm-color
  :config
  (advice-add 'term-handle-ansi-escape
	      :around #'eterm-256color-handle-ansi-escape))

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

  (add-to-list 'telephone-line-faces
	       '(yellow . (danylo/telephone-yellow . telephone-line-accent-inactive)))

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
  (when (and (window-system)
	     (not (member "all-the-icons" (font-family-list))))
    (all-the-icons-install-fonts t))
  ;; Fix MATLAB icon
  (when (window-system)
    (setq all-the-icons-icon-alist
	  (add-to-list 'all-the-icons-icon-alist
		       '("\\.m$" all-the-icons-fileicon "matlab"
			 :face all-the-icons-orange))))
  )

(use-package mic-paren
  ;; https://github.com/emacsattic/mic-paren
  ;; Advanced highlighting of matching parentheses
  :config
  (paren-activate))

;;;; Custom minor mode to highlight LaTeX equations

;;;###autoload
(defun danylo/font-lock-extend-region ()
  "Extend the search region to include an entire block of text."
  (save-excursion
    (goto-char font-lock-beg)
    (let ((found (or (re-search-backward "\n\n" nil t) (point-min))))
      (goto-char font-lock-end)
      (when (re-search-forward "\n\n" nil t)
	(beginning-of-line)
	(setq font-lock-end (point)))
      (setq font-lock-beg found))))

;;;###autoload
(define-minor-mode danylo/latex-font-lock-mode
  "LaTeX equation font locking.
Inspired from: http://makble.com/emacs-font-lock-how-to-highlight-multiline-text"
  :lighter " latex-highlight"
  (make-variable-buffer-local 'font-lock-extra-managed-props)
  (add-to-list 'font-lock-extra-managed-props 'invisible)
  (mapcar
   (lambda (arg)
     (font-lock-add-keywords
      nil `((,(format "\\(%s\\)\\(.\\|\n\\)*?\\(%s\\)"
		      (car arg) (cdr arg))
	     (0 '(face danylo/latex-equation-face-main invisible nil) t)
	     (1 '(face danylo/latex-equation-face-faded invisible nil) t)
	     (3 '(face danylo/latex-equation-face-faded invisible nil) t)))))
   `(("\\$" . "\\$")
     ("\\$\\$" . "\\$\\$")
     (,(format "\\\\begin{\\(?:%s\\)[\\*]?}" danylo/latex-equation-envs) .
      ,(format "\\\\end{\\(?:%s\\)[\\*]?}" danylo/latex-equation-envs))))
  (set (make-local-variable 'font-lock-multiline) t)
  (add-hook 'font-lock-extend-region-functions
            'danylo/font-lock-extend-region))

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
  :bind (("S-SPC" . company-complete))
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

;;; ..:: Org ::..

;;;###autoload
(defun danylo/org-mode-setup ()
  "Configure org mode after loading it"
  (setq org-adapt-indentation nil
	org-hide-leading-stars t)
  (visual-line-mode t)
  (set-mark-command nil))

;;;###autoload
(defun danylo/org-font-setup ()
  "Org mode fonts"
  ;; Heading sizes
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font (face-attribute 'default :font)
			:weight 'regular :height (cdr face)))
  ;; Make all font fixed-pitch
  (set-face-attribute 'variable-pitch nil :inherit 'default)
  (set-face-attribute 'fixed-pitch nil :inherit 'default))

(use-package org
  ;; https://github.com/bzg/org-mode
  ;; Your life in plain text
  :hook ((org-mode . danylo/org-mode-setup)
	 (org-mode . danylo/org-font-setup)
	 (org-mode . danylo/latex-font-lock-mode))
  :bind (:map org-mode-map
	      ("M-q" . 'fill-paragraph))
  :init
  (setq org-startup-folded nil
	org-ellipsis " ▾"
	org-src-tab-acts-natively t
	org-startup-with-latex-preview nil))

(with-eval-after-load "org"
  (define-key org-mode-map (kbd "M-q") 'nil)
  (define-key org-mode-map [remap fill-paragraph] nil)
  ;; LaTeX equations preview style
  (setq org-format-latex-options (plist-put org-format-latex-options
					    :scale `,danylo/latex-preview-scale)
	org-format-latex-options (plist-put org-format-latex-options
					    :foreground "yellow")))

;;;###autoload
(defun danylo/org-mode-font ()
  "Set the org-mode general text font.
See: https://stackoverflow.com/a/12286420/4605946."
  (overlay-put (make-overlay (point-min) (point-max) nil nil t)
               'face '(:family "Fira Mono")))
(add-hook 'org-mode-hook 'danylo/org-mode-font)

;;;###autoload
(defun danylo/org-emphasize (char)
  "Emphasize text in org-mode. CHAR is the character to wrap the
  text with."
  (interactive)
  (org-emphasize (string-to-char char)))

;;;###autoload
(defun danylo/org-emphasize-equation (start end)
  "Wrap an equation in $ symbols in org-mode."
  (interactive "r")
  (if (use-region-p)
      (let ((regionp (buffer-substring start end)))
	(delete-region start end)
	(if current-prefix-arg
	    (insert (format "$$%s$$" regionp))
	  (insert (format "$%s$" regionp))))
    (if current-prefix-arg
	(progn (insert "$$$$") (backward-char 2))
      (progn (insert "$$") (backward-char 1)))))

(with-eval-after-load "org"
  ;; Make sure window movement keys are not over-written
  (define-key org-mode-map (kbd "C-S-<up>") 'nil)
  (define-key org-mode-map (kbd "C-S-<down>") 'nil)
  (define-key org-mode-map (kbd "C-S-<left>") 'nil)
  (define-key org-mode-map (kbd "C-S-<right>") 'nil)
  ;; Text emphasis
  (add-to-list 'org-emphasis-alist '("$" danylo/org-equation-face))
  (define-key org-mode-map (kbd "C-c f b")
    (lambda () (interactive) (danylo/org-emphasize "*")))
  (define-key org-mode-map (kbd "C-c f i")
    (lambda () (interactive) (danylo/org-emphasize "/")))
  (define-key org-mode-map (kbd "C-c f u")
    (lambda () (interactive) (danylo/org-emphasize "_")))
  (define-key org-mode-map (kbd "C-c f e") 'danylo/org-emphasize-equation))

(use-package org-bullets
  ;; https://github.com/sabof/org-bullets
  ;; UTF-8 bullets for org-mode
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

;;; ..:: Email ::..

(use-package mu4e
  ;; https://github.com/djcb/mu
  ;; maildir indexer/searcher + emacs mail client + guile bindings
  :load-path "/usr/local/share/emacs/site-lisp/mu4e"
  :bind (:map mu4e-headers-mode-map
	      ("x" . (lambda () (interactive) (mu4e-mark-execute-all t)))
	      :map mu4e-view-mode-map
	      ("x" . (lambda () (interactive) (mu4e-mark-execute-all t)))
	      :map mu4e-compose-mode-map
	      ("C-c C-s C-s" . 'message-send)
	      ("C-c C-a" . 'mail-add-attachment))
  :init
  (setq message-mail-user-agent t
	mail-user-agent 'mu4e-user-agent
	mu4e-completing-read-function 'completing-read ; Use 'helm' to select mailboxes
	message-kill-buffer-query nil
	mu4e-context-policy 'pick-first
	mu4e-compose-context-policy 'ask-if-none
	mu4e-confirm-quit nil
	mu4e-headers-skip-duplicates t
	mu4e-hide-index-messages t
	mu4e-view-show-addresses t
	mu4e-headers-include-related nil ; [W] to toggle
	mu4e-headers-show-threads nil
	mu4e-compose-signature-auto-include nil
	max-specpdl-size 5000 ; See djcbsoftware.nl/code/mu/mu4e/General.html
	mu4e-remove-func 'danylo/mu4e~headers-remove-handler

	;; <message HTML view>
	mu4e-view-prefer-html t
	mu4e-view-html-plaintext-ratio-heuristic most-positive-fixnum
	mu4e-html2text-command 'mu4e-shr2text
	shr-color-visible-luminance-min 60
	shr-color-visible-distance-min 5
	shr-use-colors nil

	;; <fix failed to find message error (github.com/djcb/mu/issues/1667)>
	mu4e-change-filenames-when-moving nil
	mu4e-index-cleanup t
	mu4e-index-lazy-check nil


	;; <split view settings>
	mu4e-headers-visible-lines 15
	mu4e-headers-visible-columns 50
	mu4e-split-view nil

	;; <sending smtp settings>
	sendmail-program "/usr/bin/msmtp"
	send-mail-function 'smtpmail-send-it
	message-sendmail-f-is-evil t
	message-sendmail-extra-arguments '("--read-envelope-from")
	message-send-mail-function 'message-send-mail-with-sendmail
	message-kill-buffer-on-exit t
	)
  (advice-add #'shr-colorize-region :around
	      (defun shr-no-colourise-region (&rest ignore)))
  )

(with-eval-after-load "mu4e"
  ;; Disable message sending with C-c C-s (make it more complicated to
  ;; not send messages by accident)
  (define-key mu4e-headers-mode-map (kbd "C-c C-s") 'nil))

;;;###autoload
(defun danylo/mu4e~headers-remove-handler (docid &optional skip-hook)
  "This is a patched version of mu4e~headers-remove-handler in
mu4e-headers.el. The original function throws an error after
sending a message that was previously a draft, because it tries
to delete the window that held the message editing bufer. This
function does not delete the window, which has the additional
benefit of preserving window layout."
  (when (buffer-live-p (mu4e-get-headers-buffer))
    (mu4e~headers-remove-header docid t))
  ;; if we were viewing this message, close it now.
  (when (and (mu4e~headers-view-this-message-p docid)
             (buffer-live-p (mu4e-get-view-buffer)))
    (kill-buffer (mu4e-get-view-buffer)))
  (unless skip-hook
    (run-hooks 'mu4e-message-changed-hook)))

;;;###autoload
(defun message-kill-buffer ()
  "Patched message-kill-buffer from gnus/message.el.gz.
This version deletes backup files without asking."
  (interactive)
  (when (or (not (buffer-modified-p))
	    (not message-kill-buffer-query)
	    (yes-or-no-p "Message modified; kill anyway? "))
    (let ((actions message-kill-actions)
	  (draft-article message-draft-article)
	  (auto-save-file-name buffer-auto-save-file-name)
	  (file-name buffer-file-name)
	  (modified (buffer-modified-p)))
      (setq buffer-file-name nil)
      (kill-buffer (current-buffer))
      (when (and (or (and auto-save-file-name
			  (file-exists-p auto-save-file-name))
		     (and file-name
			  (file-exists-p file-name))))
	(ignore-errors
	  (delete-file auto-save-file-name))
	(let ((message-draft-article draft-article))
	  (message-disassociate-draft)))
      (message-do-actions actions))))

;;;###autoload
(defun danylo/launch-mu4e (arg)
  "Launch mu4e, or quit it if preceded by C-u"
  (interactive "P")
  (if arg (mu4e-quit) (mu4e)))

(general-define-key
 "C-c m" 'danylo/launch-mu4e)

(with-eval-after-load "mu4e"
  (set-face-attribute 'mu4e-unread-face nil :foreground "yellow")
  (set-face-attribute 'mu4e-header-highlight-face nil :underline nil))

;; Do not back up email while writing it, which makes drafts
;; accumulate
(add-hook 'mu4e-compose-mode-hook
	  #'(lambda ()
	      (auto-save-mode -1)
	      (make-local-variable 'make-backup-files)
	      (setq make-backup-files nil)))

;;;; Load Lisp file with mu4e context setp
(load "~/.emacs.d/email.el")

;;;; Signature before message history

;;;###autoload
(defun danylo/insert-mu4e-signature ()
  "Insert the email signature."
  (when (stringp mu4e-compose-signature)
    (unless (member mu4e-compose-type '(edit resend))
      (save-excursion
	(message-goto-body)
	;; Go to the first empty line in the message body, which must
	;; be the one right after the Org-mode headers
	(setq empty-line nil)
	(while (not empty-line)
	  (let ((num-chars (- (line-end-position)
			      (line-beginning-position))))
	    (if (eq num-chars 0)
		(setq empty-line t)
	      (forward-line))))
	(insert (concat "\n\n" mu4e-compose-signature))))))
(add-hook 'mu4e-compose-mode-hook 'danylo/insert-mu4e-signature)

;;;###autoload
(defun danylo/refresh-mu4e-alert-mode-line ()
  "Show new mail in the mode line."
  (interactive)
  (mu4e-update-mail-and-index t)
  (mu4e-alert-enable-mode-line-display))

(defvar danylo/mu4e-alert-started nil
  "Indicator if mu4e-alert indicator started")

(use-package mu4e-alert
  ;; https://github.com/iqbalansari/mu4e-alert
  ;; Desktop notifications and modeline display for mu4e
  :ensure t
  :after mu4e
  :init
  ;; Enable mu4e-alert display after mu4e is started
  (add-hook 'mu4e-main-mode-hook
	    (lambda ()
	      (unless danylo/mu4e-alert-started
		(mu4e-alert-enable-mode-line-display)
		(run-with-timer 0 (* 60 5) 'danylo/refresh-mu4e-alert-mode-line)
		(setq danylo/mu4e-alert-started t)))))

;;; ..:: Git ::..

(use-package magit
  ;; https://github.com/magit/magit
  ;; An interface to the version control system Git
  :bind (("C-x g" . magit-status))
  )

;;; ..:: Shell interaction ::..

;;;###autoload
(defun danylo/shell~check-open (shell-buffer-name)
  "Check if a shell is running."
  (if (get-buffer shell-buffer-name) t nil))

;;;###autoload
(defun danylo/shell-open (shell-buffer-name shell-type &optional in-place)
  "Open a shell.
Placed in the current buffer."
  (if (danylo/shell~check-open shell-buffer-name)
      ;; The shell buffer exists
      (if in-place
	  ;; Show shell in current window
	  (switch-to-buffer shell-buffer-name)
	;; Show shell in another window
	(if (get-buffer-window shell-buffer-name)
	    ;; The buffer is already displayed, switch to it
	    (pop-to-buffer shell-buffer-name)
	  ;; The buffer is hidden, show it
	  (let ((this-window (selected-window))
		(new-window (split-window-vertically)))
	    (select-window new-window)
	    (switch-to-buffer shell-buffer-name)
	    (select-window this-window))))
    ;; The shell buffer does not exist
    (if in-place
	;; Create shell in current window
	(progn
	  (ansi-term shell-type)
	  (rename-buffer shell-buffer-name))
      ;; Create shell in new window
      (let ((this-window (selected-window))
	    (new-window (split-window-vertically)))
	(select-window new-window)
	(ansi-term shell-type)
	(rename-buffer shell-buffer-name)
	(select-window this-window)))))

;;;###autoload
(defun danylo/shell-exec (shell-buffer-name command)
  "Run the current file in the shell.
If there is no shell open, prints a message to inform."
  (if (danylo/shell~check-open shell-buffer-name)
      ;; Send a run command for the current file
      (with-current-buffer shell-buffer-name
	(term-send-raw-string (format "%s\n" command)))
    (message "No shell open.")))

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
	   (danylo/fa-icon "info" "yellow")
	   (propertize string 'face '(:background nil :foreground "yellow"))))

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

;;;###autoload
(defun danylo/python-shell-open (arg)
  "Open a Python shell."
  (interactive "P")
  (if arg
      ;; Open in current window
      (danylo/shell-open danylo/python-buffer-name
			 danylo/python-shell-type t)
    ;; Open in new window
    (danylo/shell-open danylo/python-buffer-name
		       danylo/python-shell-type)))

;;;###autoload
(defun danylo/python-shell-run-file ()
  "Run current Python file."
  (interactive)
  (let* ((file-name (file-name-nondirectory (buffer-file-name)))
	 (command (format "%%run -i %s" file-name)))
    (danylo/shell-exec danylo/python-buffer-name command)
    ))

;;;###autoload
(defun danylo/python-config ()
  ;; Key bindings
  (define-key python-mode-map (kbd "C-c C-s") 'danylo/python-shell-open)
  (define-key python-mode-map (kbd "C-c C-f") 'danylo/python-shell-run-file)
  (define-key python-mode-map (kbd "C-c C-p") nil)
  ;; Hide shell buffers from buffer list
  (let ((buf-name (replace-regexp-in-string
		   "*+" "\\\\*" danylo/python-buffer-name t t)))
    (add-to-list 'ivy-ignore-buffers `,buf-name)))
(add-hook 'python-mode-hook (lambda () (danylo/python-config)))

;;; ..:: Julia ::..

(use-package julia-mode
  ;; https://github.com/JuliaEditorSupport/julia-emacs
  ;; Major mode for the julia programming language
  :hook ((julia-mode . yas-minor-mode))
  )

(use-package lsp-julia
  ;; https://github.com/non-Jedi/lsp-julia
  ;; Julia support for lsp-mode using LanguageServer.jl
  :after lsp-mode
  :ensure nil
  :quelpa ((lsp-julia :fetcher github
                      :repo "non-Jedi/lsp-julia"
                      :files (:defaults "languageserver")))
  :hook ((julia-mode . (lambda () (require 'lsp-julia) (lsp))))
  :init (setq lsp-julia-package-dir nil
	      lsp-julia-default-environment "~/.julia/environments/v1.5"))

;;;; Julia shell interaction

;;;###autoload
(defun danylo/julia-shell-open (arg)
  "Open a Julia shell."
  (interactive "P")
  (if arg
      ;; Open in current window
      (danylo/shell-open danylo/julia-buffer-name
			 danylo/julia-shell-type t)
    ;; Open in new window
    (danylo/shell-open danylo/julia-buffer-name
		       danylo/julia-shell-type)))

;;;###autoload
(defun danylo/julia-shell-run-file ()
  "Run current Julia file."
  (interactive)
  (let* ((file-name (file-name-nondirectory (buffer-file-name)))
	 (command (format "include(\"%s\")" file-name)))
    (danylo/shell-exec danylo/julia-buffer-name command)
    ))

;;;###autoload
(defun danylo/julia-config ()
  ;; Key bindings
  (define-key julia-mode-map (kbd "C-c C-s") 'danylo/julia-shell-open)
  (define-key julia-mode-map (kbd "C-c C-f") 'danylo/julia-shell-run-file)
  (define-key julia-mode-map (kbd "C-c C-l") 'julia-latexsub-or-indent)
  ;; Hide shell buffers from buffer list
  (let ((buf-name (replace-regexp-in-string
		   "*+" "\\\\*" danylo/julia-buffer-name t t)))
    (add-to-list 'ivy-ignore-buffers `,buf-name)))
(add-hook 'julia-mode-hook (lambda () (danylo/julia-config)))

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
	 (LaTeX-mode . danylo/latex-font-lock-mode)
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
				      'texmathp)))
	 ;; Helm for candidate completion
	 (LaTeX-mode . (lambda ()
			 (require 'helm-mode)
			 (add-to-list 'helm-completing-read-handlers-alist
				      '(LaTeX-environment
					. helm-completing-read-default-handler)))))
  :init
  (setq TeX-source-correlate-method 'synctex
	TeX-source-correlate-start-server t
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
