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

;; Lisp deprecation
;; https://github.com/kiwanami/emacs-epc/issues/35
(setq byte-compile-warnings '(cl-functions))

;;; ..:: Customization variables ::..

;;;###autoload
(defun danylo/make-path (rel-path)
  "Prepend emacs home directory to relative path in the home
directory."
  (expand-file-name rel-path user-emacs-directory))

(defconst danylo/emacs-custom-lisp-dir
  `,(danylo/make-path "lisp/")
  "Location of additional custom Lisp code directory.")

(defconst danylo/emacs-backup-dir
  `,(danylo/make-path "backups/")
  "Location where backup files are saved.")

(use-package danylo-custom-variables
  ;; Customization variables for init file.
  :ensure nil
  :load-path danylo/emacs-custom-lisp-dir)

;;; ..:: General helper functions ::..

;;;###autoload
(defun danylo/fancy-icon (icon-lib icon-family icon &optional fg)
  "Icon with proper formatting for minibuffer"
  (unless fg
    (setq fg `,(face-attribute 'default :foreground)))
  (if (window-system)
      (propertize (funcall icon-lib icon)
		  'face `(:family ,(funcall icon-family)
				  :height 0.95
				  :foreground ,fg)
		  'display '(raise -0.05))
    ""))

;;;###autoload
(defun danylo/fa-icon (icon &optional fg)
  "Fontawesome icon with proper formatting."
  (danylo/fancy-icon 'all-the-icons-faicon 'all-the-icons-faicon-family
		     icon fg))

;;;###autoload
(defun danylo/octicon (icon &optional fg)
  "Octicon with proper formatting."
  (danylo/fancy-icon 'all-the-icons-octicon 'all-the-icons-octicon-family
		     icon fg))

(use-package ts
  ;; https://github.com/alphapapa/ts.el
  ;; Emacs timestamp and date-time library
  ;; **Loading this just because unpackaged needs it**
  :ensure nil
  :quelpa ((ts :fetcher github
	       :repo "alphapapa/ts.el")))

(use-package unpackaged
  ;; https://github.com/alphapapa/unpackaged.el
  ;; A collection of useful Emacs Lisp code
  ;;
  ;; Use:
  ;;   unpackaged/quelpa-use-package-upgrade: select a (use-package ...) that
  ;;       uses Quelpa and completely upgrade it to the latest online version
  ;;       (replaces all associated directories)
  :ensure nil
  :after (general hydra ts esxml)
  :init (setq danylo/use-package-always-ensure use-package-always-ensure
	      use-package-always-ensure nil)
  :quelpa ((unpackaged :fetcher github
		       :repo "alphapapa/unpackaged.el"))
  :config
  (setq use-package-always-ensure danylo/use-package-always-ensure))

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
      (danylo/print-in-minibuffer (format "%s GC" (danylo/fa-icon "trash")))
      )))

(add-hook 'post-gc-hook (lambda () (danylo/gc-message)))

(setq garbage-collection-messages nil)

(use-package gcmh
  ;; https://github.com/emacsmirror/gcmh"
  ;; The Garbage Collector Magic Hack
  :init (setq gcmh-low-cons-threshold danylo/gc-cons-threshold
	      gcmh-high-cons-threshold most-positive-fixnum)
  :config
  (gcmh-mode 1))

;;; ..:: Keybinding management ::..

(require 'bind-key)

(use-package general
  ;; https://github.com/noctuid/general.el
  ;; More convenient key definitions in emacs
  )

(use-package hydra
  ;; https://github.com/abo-abo/hydra
  ;; Make Emacs bindings that stick around
  )

;;; ..:: General usability ::..

;; Remove GUI elements
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(tooltip-mode 0)
(blink-cursor-mode 0)

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

;; Show column number
(setq-default column-number-mode t)

;; Do not resize minibuffer for long path on file save

;;;###autoload
(defun danylo/print-in-minibuffer (str)
  "Echo STR in the minibuffer."
  (with-selected-window (minibuffer-window)
    (setq cursor-type nil)
    (message (propertize str 'face `(:foreground ,danylo/faded)))
    (setq cursor-type t)))

;;;###autoload
(defun danylo/save-buffer (orig-fun &rest args)
  "Pretty print save buffer, preserver height of minibuffer."
  (let ((message-truncate-lines t)
	(this-file-name (file-name-nondirectory (buffer-file-name))))
    (let ((inhibit-message t))
      (apply orig-fun args))
    (danylo/print-in-minibuffer
     (format "%s Saved %s" (danylo/fa-icon "database") this-file-name))
    ))

(advice-add 'save-buffer :around #'danylo/save-buffer)

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
(setq-default custom-file (danylo/make-path ".custom.el"))
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
(setq backup-directory-alist (list (cons "." danylo/emacs-backup-dir)))
(setq auto-save-file-name-transforms `((".*" ,danylo/emacs-backup-dir t)))
(setq auto-save-default nil) ;; Disable auto-save (I save myself)

(defun danylo/byte-compile-init-dir ()
  "Byte-compile Emacs config."
  (interactive)
  (byte-recompile-file (danylo/make-path "init.el") nil 0)
  (byte-recompile-directory danylo/emacs-custom-lisp-dir 0))

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

;;;; Handle killing Emacs

;;;###autoload
(defun danylo/close-frame-or-kill-emacs (arg)
  "Delete frame or kill Emacs if there are only one frame.
When called with C-u prefix, force kill Emacs (all frames).
Source: https://emacs.stackexchange.com/a/50834/13661"
  (interactive "P")
  (if arg
      ;; Force kill Emacs (all frames), traditional C-x C-c style
      (save-buffers-kill-terminal)
    ;; Kill only the current frame, if there is more than one
    (if (> (length (frame-list)) 1)
	;; Just delete the current frame
	(delete-frame)
      ;; Traditional C-x C-c (kill Emacs)
      (save-buffers-kill-terminal))))

;;;###autoload
(defun danylo/make-new-frame ()
  "Make a new frame, and make sure it behaves like I want."
  (interactive)
  (make-frame-command)
  (danylo/set-whitespace-mode-colors))

(general-define-key
 "C-x C-c" nil ;; Kind of dangerous (can quit Emacs by accident)
 "C-x C-c C-c" 'danylo/close-frame-or-kill-emacs
 "C-x n f" 'danylo/make-new-frame)

;;;; Scrolling performance

(setq fast-but-imprecise-scrolling t
      scroll-conservatively 0)

;;;; eval-buffer default directory fix

;;;###autoload
(defun danylo/eval-buffer-maintain-dir (orig-fun &rest args)
  "Maintain default-directory when eval-buffer."
  (let ((current-dir default-directory))
    (apply orig-fun args)
    (setq-local default-directory current-dir)))

(advice-add 'eval-buffer :around #'danylo/eval-buffer-maintain-dir)

;;; ..:: Searching ::..

;;;###autoload
(defun danylo/side-window-jump (fun buf-name)
  "Smart go to a side window. If side window visible, jump
there. If not visible, open it but don't focus."
  (if (string= (format "%s" (current-buffer)) buf-name)
      (danylo/switch-to-last-window)
    (let ((current-window (selected-window))
	  (side-window-exists (get-buffer-window buf-name t)))
      (funcall fun)
      (unless side-window-exists
	;; Go back to current window if showing Imenu list when it was not
	;; shown already shown
	(select-window current-window)))))

;;;###autoload
(defun danylo/neotree-jump ()
  "Smart open neotree side window."
  (interactive)
  (danylo/side-window-jump 'neotree-show neo-buffer-name))

(use-package neotree
  ;; https://github.com/jaypei/emacs-neotree
  ;; A emacs tree plugin like NerdTree for Vim.
  :after (all-the-icons)
  :bind (("C-c t n" . danylo/neotree-jump))
  :init (setq neo-theme (if (display-graphic-p) 'icons 'arrow)
	      neo-window-width danylo/side-window-width
	      neo-smart-open t
	      neo-show-hidden-files t
	      neo-autorefresh t)
  (add-hook 'after-init-hook (lambda () (require 'neotree))))

(defvar danylo/imenu-list--displayed-window nil
  "The **window** who owns the saved imenu entries.")

;;;###autoload
(defun danylo/imenu-list-jump ()
  "Smart open imenu-list side window."
  (interactive)
  (setq danylo/imenu-list--displayed-window (selected-window))
  (danylo/side-window-jump 'imenu-list imenu-list-buffer-name))

(use-package imenu-list
  ;; https://github.com/bmag/imenu-list
  ;; Emacs plugin to show the current buffer's imenu entries
  :bind (:map prog-mode-map
	      ("C-c t i" . danylo/imenu-list-jump)
	      :map imenu-list-major-mode-map
	      ("C-c t i" . danylo/imenu-list-jump))
  :init (setq imenu-list-size danylo/side-window-width
	      imenu-list-position 'left
	      imenu-list-mode-line-format
	      '("%e" mode-line-front-space
		(:propertize "%b" face mode-line-buffer-id) " "
		(:eval (buffer-name imenu-list--displayed-buffer)) " "
		mode-line-end-spaces))
  (add-hook 'prog-mode-hook (lambda () (require 'imenu-list))))

;; Patches to imenu so as to navigate using the **window** that owns the
;; current Imenu, not the buffer. This way handles multiple windows showing the
;; same buffer. Otherwise, the jump happens in the wrong window than the one
;; the user was browsing.

;;;###autoload
(defun danylo/imenu-list-goto-entry (orig-fun &rest args)
  "Switch to the original buffer and display the entry under point.
Patched to use original **window** instead of buffer."
  (interactive)
  (let ((entry (imenu-list--find-entry)))
    (select-window danylo/imenu-list--displayed-window)
    (imenu entry)
    (run-hooks 'imenu-list-after-jump-hook)
    (imenu-list--show-current-entry)))
(advice-add 'imenu-list-goto-entry :around #'danylo/imenu-list-goto-entry)

;;;###autoload
(defun danylo/imenu-list-display-entry (orig-fun &rest args)
  "Display in original buffer the entry under point.
Patched to use original **window** instead of buffer."
  (interactive)
  (let ((entry (imenu-list--find-entry)))
    (save-selected-window
      (select-window danylo/imenu-list--displayed-window)
      (imenu entry)
      (run-hooks 'imenu-list-after-jump-hook)
      (imenu-list--show-current-entry))))
(advice-add 'imenu-list-display-entry :around #'danylo/imenu-list-display-entry)

;; (end of Imenu-list patches)

(defun danylo/smart-select-region (start end)
  "Select region in file, removing possible indent of all
lines according to the first line."
  (interactive "r")
  (if (use-region-p)
      (let* ((selection (buffer-substring start end))
	     (starting-spaces (progn (string-match "^\s+" selection)
				     (match-end 0)))
	     (starting-space-replace-regexp
	      `,(format "^\s\\{%d\\}" starting-spaces))
	     ;; Remove the initial spaces
	     (trimmed-selection (replace-regexp-in-string
				 starting-space-replace-regexp "" selection))
	     ;; Remove any trailing newlines
	     (trimmed-selection (replace-regexp-in-string "\n$" "" trimmed-selection)))
	trimmed-selection)
    ;; No region active
    (message "No region selected.")
    nil))

;;;###autoload
(defun danylo/swiper-thing-at-point (&optional start end)
  "Put thing at point in swiper buffer."
  (interactive (if (use-region-p) (list (region-beginning) (region-end))))
  (if start
      (let ((selection (buffer-substring start end)))
	(deactivate-mark)
	(swiper selection))
    (swiper (thing-at-point 'symbol))))

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
	  (insert (propertize danylo/completion-candidate-list
			      'display '(height 1.0))))
	(setq-local truncate-lines t)
	;; "Hack" to make sure that viewing start of line
	(move-beginning-of-line 0))
      (with-ivy-window
	(let ((window (selected-window)))
	  (if (window-parameter window 'window-side)
	      ;; In side window
	      ;; Cannot use display-buffer-below-selected because a
	      ;; side window cannot be split (most likely)
	      (display-buffer
	       buffer
	       `((display-buffer-reuse-window display-buffer-pop-up-window)
		 (window-height . ,danylo/num-completion-candidates)))
	    ;; In regular window
	    (display-buffer
	     buffer
	     `((display-buffer-reuse-window display-buffer-below-selected)
	       (window-height . ,danylo/num-completion-candidates))))))
      )))

;;;###autoload
(defun danylo/side-window-tmp ()
  (interactive)
  (display-buffer-in-side-window (get-buffer "*Messages*") '((side . right))))

;;;###autoload
(defun danylo/counsel-imenu ()
  "Jump to a buffer position indexed by imenu.
**Do not sort the items**, so that the order is as the items
appear in the file."
  (interactive)
  (ivy-read "imenu items: " (counsel--imenu-candidates)
            :preselect (thing-at-point 'symbol)
            :require-match t
            :action #'counsel-imenu-action
            :keymap counsel-imenu-map
            :history 'counsel-imenu-history
            :caller 'counsel-imenu
	    :sort nil))

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
	 ("C-c q" . danylo/counsel-imenu)
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
	ivy-display-style 'fancy
	;; Speed up Swiper when in visual-line-mode
	;; Source: https://github.com/abo-abo/swiper/issues/1952
	;;         https://github.com/abo-abo/swiper/issues/2471
	;;         https://github.com/abo-abo/swiper/issues/2588
	swiper-use-visual-line-p #'ignore
	;; Set minimum length for searching in project
	ivy-more-chars-alist '((t . 1))
	)
  (add-hook 'ivy-mode-hook
	    (lambda ()
	      (set-face-attribute 'ivy-minibuffer-match-face-2 nil
				  :background `,danylo/orange
				  :foreground `,danylo/black
				  :weight 'normal
				  :inherit 'default)
	      (set-face-attribute 'swiper-match-face-1 nil
				  :background `,danylo/orange
				  :foreground `,danylo/black
				  :weight 'normal)
	      (set-face-attribute 'ivy-current-match nil
				  :extend nil
				  :height 1.0)))
  (setq ivy-ignore-buffers
	(append ivy-ignore-buffers
		'("\\*ivy-" "\\*lsp-" "\\*quelpa-" "\\*Flycheck" "\\*Help"
		  "\\*Ilist" "\\*dashboard\\*" "\\*xref\\*" "\\*toc\\*"
		  "\\*Compile-Log\\*" "\\*CPU-Profiler-Report.*\\*"
		  "\\*TeX Help\\*" "\\*Buffer List\\*" "\\*Julia" "\\*Python"
		  "magit.*:" "\\*Backtrace\\*" "\\*Process List\\*")))
  :config
  (ivy-mode 1))

;;;###autoload
(defun danylo/company-adjust (start)
  "Adjust bounds of thing at point.
This is a 'patch' to handle things like @ and \\ in the prefix correctly."
  (save-excursion
    (goto-char start)
    (when (or (= (char-before) ?\\)
	      (= (char-before) ?@))
      (setq start (1- start)))
    start))

;;;###autoload
(defun danylo/counsel-company (orig-fun &rest args)
  "Complete using `company-candidates'.
Patched so that symbols beginning with \\, @, etc. are correctly handled."
  (interactive)
  (company-complete)
  (let ((len (cond (company-common
		    (length company-common))
		   (company-prefix
		     (length company-prefix)))
	     ))
    (when (and len (> (length company-candidates) 0))
      (setq ivy-completion-beg (- (point) len))
      (setq ivy-completion-beg (danylo/company-adjust ivy-completion-beg))
      (setq ivy-completion-end (point))
      (ivy-read "Candidate: " company-candidates
                :action #'ivy-completion-in-region-action
                :caller 'counsel-company)
      )))
(advice-add 'counsel-company :around #'danylo/counsel-company)

;;;###autoload
(defun danylo/expand-unicode (orig-fun &rest args)
  "Expand unicode symbol if necessary."
  (apply orig-fun args)
  (when (= (string-to-char (substring (nth 0 args) 0 1)) ?\\)
    (julia-latexsub)))

(advice-add 'ivy-completion-in-region-action :around #'danylo/expand-unicode)

(use-package helm
  ;; https://emacs-helm.github.io/helm/
  ;; Emacs incremental completion and selection narrowing framework
  :ensure t
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
				  :foreground `,danylo/yellow
				  :weight 'bold
				  :extend nil)
	      (set-face-attribute 'helm-candidate-number nil
				  :foreground `,danylo/black
				  :background `,danylo/yellow)
	      (set-face-attribute 'helm-selection nil
				  :extend nil)
	      (set-face-attribute 'helm-match nil
				  :background `,danylo/orange
				  :foreground `,danylo/black
				  :weight 'normal
				  :inherit 'default)))
  (add-to-list 'ivy-ignore-buffers '"\\*helm")
  :config
  (helm-mode 1))

(use-package counsel-projectile
  ;; https://github.com/ericdanan/counsel-projectile
  ;; Ivy UI for Projectile
  :init (setq projectile-completion-system 'ivy
	      counsel-projectile-ag-initial-input '(ivy-thing-at-point)))

(require 'cc-mode)
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

(use-package avy
  ;; https://github.com/abo-abo/avy
  ;; Jump to things in Emacs tree-style
  :ensure t
  :bind (("M-s" . avy-goto-word-1)
	 ("M-g f" . avy-goto-line)
	 ("C-'" . avy-isearch)))

;;; ..:: Theming and code aesthetics ::..

;; Auto-create matching closing parentheses
(electric-pair-mode 1)

(use-package rainbow-delimiters
  ;; https://github.com/Fanael/rainbow-delimiters
  ;; Highlights delimiters such as parentheses, brackets or braces
  ;; according to their depth.
  :hook ((prog-mode . rainbow-delimiters-mode)))

;;;;  Highlight matching parentheses

(setq show-paren-delay `,danylo/fontify-delay)
(show-paren-mode 1)

(use-package solaire-mode
  ;; https://github.com/hlissner/emacs-solaire-mode
  ;; Distinguish file-visiting buffers with slightly different background
  :hook ((change-major-mode . turn-on-solaire-mode))
  :config
  (solaire-global-mode +1))

;;;; Line numbering

;; Show line numbers on the left
(general-define-key
 "C-x n l" 'display-line-numbers-mode)

(add-hook 'display-line-numbers-mode-hook
	  (lambda ()
	    ;; Line highlight face
	    (set-face-attribute 'line-number-current-line nil
				  :foreground `,danylo/yellow
				  :height `,danylo/linum-height
				  :weight 'normal
				  :inherit 'default)
	    (set-face-attribute 'line-number nil
				:height `,danylo/linum-height
				:inherit 'default)))

(use-package doom-themes
  ;; https://github.com/hlissner/emacs-doom-themes
  ;; An opinionated pack of modern color-themes
  :config
  (load-theme 'doom-one t)
  (set-face-attribute 'region nil
		      :extend nil
		      :foreground `,danylo/yellow
		      :background `,danylo/dark-blue)
  (set-face-attribute 'font-lock-string-face nil
		      :foreground `,danylo/green))

(add-hook 'after-init-hook
	  (lambda ()
	    (when (window-system)
	      ;; Default Emacs font
	      (set-face-attribute 'default nil
				  :height `,danylo/font-default-height))))

;; Speed up font-lock mode speed (can causes laggy scrolling)
(setq-default font-lock-support-mode 'jit-lock-mode
	      jit-lock-contextually 'syntax-driven
	      jit-lock-stealth-time 10
	      jit-lock-stealth-nice 0.5
	      jit-lock-chunk-size 1000
	      jit-lock-stealth-load 200
	      jit-lock-antiblink-grace most-positive-fixnum
	      ;; Below: jit-lock-defer-time "pauses" fontification while the
	      ;; user is typing, as long as the time between successive
	      ;; keystrokes is <jit-lock-defer-time. This is what makes typing
	      ;; smooth even with some heavy font locking (because the font
	      ;; locking will occur during "idle" times)!
	      jit-lock-defer-time `,danylo/fontify-delay
	      font-lock-maximum-decoration nil)

;;;; Font lock debug tools

(use-package highlight-refontification
  ;; https://github.com/Lindydancer/highlight-refontification
  ;; Visualize how font-lock refontifies a buffer
  )

(use-package font-lock-profiler
  ;; https://github.com/Lindydancer/font-lock-profiler
  ;; Coverage and timing tool for font-lock keywords
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

(use-package doom-modeline
  ;; https://github.com/seagle0128/doom-modeline
  ;; A fancy and fast mode-line inspired by minimalism design.
  :init (setq doom-modeline-height 10
	      doom-modeline-bar-width 2
	      doom-modeline-major-mode-icon nil
	      doom-modeline-icon nil
	      doom-modeline-buffer-state-icon nil
	      doom-modeline-buffer-file-name-style 'file-name
	      doom-modeline-buffer-encoding nil
	      doom-modeline-mu4e t
	      inhibit-compacting-font-caches t
	      find-file-visit-truename t
	      doom-modeline-project-detection 'project
	      doom-modeline-enable-word-count nil)
  :config
  ;;;; Custom segment definitions
  (doom-modeline-def-segment danylo/matches
    "Show the number of active cursors in the buffer from `multiple-cursors'."
    (let ((meta (concat (danylo/doom-modeline-multiple-cursors))))
      meta))
  (doom-modeline-def-segment danylo/mu4e
    "Show notifications of any unread emails in `mu4e'."
    (when (and doom-modeline-mu4e
               (doom-modeline--active)
               (not doom-modeline--limited-width-p)
               (bound-and-true-p mu4e-alert-mode-line)
               (numberp mu4e-alert-mode-line)
               ;; don't display if the unread mails count is zero
               (> mu4e-alert-mode-line 0))
      (concat
       (doom-modeline-spc)
       (format "%s %s"
	       (propertize
		;; Trick: to make the alignment correct, replace spaces with
		;; the icons, which lets Emacs know that the envelope icon
		;; takes up the column amount equivalent to that many spaces
		"  "
		'face `(:inherit (doom-modeline-unread-number
				  doom-modeline-warning))
		'display
		(propertize (danylo/fa-icon "envelope")
			    'face `(:family ,(all-the-icons-faicon-family)
					    :inherit (doom-modeline-unread-number
						      doom-modeline-warning))))
	       (propertize `,(format "%s" mu4e-alert-mode-line)
			   'face '(:inherit (doom-modeline-unread-number
					     doom-modeline-warning))))
       (doom-modeline-spc))
      ))
  (doom-modeline-def-segment danylo/vcs
    "Displays the current branch, colored based on its state."
    (let ((active (doom-modeline--active)))
      (when-let ((icon doom-modeline--vcs-icon)
		 (text doom-modeline--vcs-text))
	(concat
	 (doom-modeline-spc)
	 (concat
          (if active
              icon
            (doom-modeline-propertize-icon icon 'mode-line-inactive))
          (doom-modeline-spc))
	 (if active
             text
           (propertize text 'face 'mode-line-inactive))
	 (doom-modeline-spc)))))
  ;;;; Custom modeline definitions
  ;; Default mode line
  (doom-modeline-def-modeline 'danylo/mode-line
    '(bar danylo/matches buffer-info remote-host buffer-position selection-info)
    '(danylo/mu4e input-method debug lsp major-mode danylo/vcs process))
  (add-hook 'doom-modeline-mode-hook
	    (lambda () (doom-modeline-set-modeline 'danylo/mode-line 'default)))
  ;; Helm mode line
  (doom-modeline-def-modeline 'helm
    '(bar helm-buffer-id helm-number helm-follow helm-prefix-argument) '())
  ;; Dashboard mode line
  (doom-modeline-def-modeline 'dashboard
    '(bar window-number buffer-default-directory-simple) '(danylo/mu4e))
  ;; Magit
  (doom-modeline-def-modeline 'vcs
    '(bar danylo/matches buffer-info buffer-position selection-info)
    '(danylo/mu4e gnus github debug minor-modes buffer-encoding major-mode process))
  ;; Messages and scratch buffer mode line
  (doom-modeline-def-modeline 'danylo/bare-modeline
    '(bar window-number buffer-info-simple) '(danylo/mu4e))
  (add-hook 'after-init-hook
	    (lambda ()
	      (dolist (bname '("*scratch*" "*Messages*"))
		(if (buffer-live-p (get-buffer bname))
		    (with-current-buffer bname
		      (doom-modeline-set-modeline 'danylo/bare-modeline)))))))

;; Activate the Doom modeline mode
(add-hook 'after-init-hook (lambda () (doom-modeline-mode 1)))

;;;###autoload
(defun danylo/doom-modeline-multiple-cursors ()
  "Show the number of multiple cursors."
  (cl-destructuring-bind (count . face)
      (cond ((bound-and-true-p multiple-cursors-mode)
             (cons (mc/num-cursors)
                   (if (doom-modeline--active)
		       'doom-modeline-panel
                     'mode-line-inactive)))
            ((bound-and-true-p evil-mc-cursor-list)
             (cons (length evil-mc-cursor-list)
                   (cond ((not (doom-modeline--active)) 'mode-line-inactive)
			 (evil-mc-frozen 'doom-modeline-bar)
			 ('doom-modeline-panel))))
            ((cons nil nil)))
    (when count
      (concat (propertize " mc:" 'face face)
	      (propertize (format "%d " count) 'face face)))))

;;;###autoload
(defun danylo/doom-modeline-update-vcs-icon (orig-fun &rest args)
  "Update icon of vcs state in mode-line.
Patched for my own icons."
  (setq doom-modeline--vcs-icon
        (when (and vc-mode buffer-file-name)
          (let* ((backend (vc-backend buffer-file-name))
                 (state   (vc-state buffer-file-name backend)))
            (cond ((memq state '(edited added))
		   (propertize (danylo/octicon "git-compare") 'face
   		    	       `(:family ,(all-the-icons-octicon-family)
					 :inherit doom-modeline-warning)))
                  ((eq state 'needs-merge)
                   (propertize (danylo/octicon "git-merge") 'face
   		    	       `(:family ,(all-the-icons-octicon-family)
					 :inherit doom-modeline-warning)))
                  ((eq state 'needs-update)
                   (propertize (danylo/octicon "arrow-down") 'face
   		    	       `(:family ,(all-the-icons-octicon-family)
					 :inherit doom-modeline-warning)))
                  ((memq state '(removed conflict unregistered))
                   (propertize (danylo/octicon "alert") 'face
   		    	       `(:family ,(all-the-icons-octicon-family)
					 :inherit doom-modeline-warning)))
                  (t
		   (propertize (danylo/octicon "git-branch") 'face
   		    	       `(:family ,(all-the-icons-octicon-family)
					 :inherit doom-modeline-warning))))))))

(advice-add 'doom-modeline-update-vcs-icon
	    :around #'danylo/doom-modeline-update-vcs-icon)

(use-package danylo-text-font-lock
  ;; Personal minor mode for text document highlighting
  :ensure nil
  :load-path danylo/emacs-custom-lisp-dir
  :hook ((LaTeX-mode . danylo-text-font-lock-mode)
	 (org-mode . danylo-text-font-lock-mode)))

(use-package danylo-prog-font-lock
  ;; Personal minor mode for code highlighting
  :ensure nil
  :load-path danylo/emacs-custom-lisp-dir
  :hook ((python-mode . danylo-prog-font-lock-mode)
	 (julia-mode . danylo-prog-font-lock-mode)))

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
  :init
  (add-hook 'hl-todo-mode-hook
	    (lambda ()
	      (set-face-attribute 'hl-todo nil
				  :background `,danylo/yellow
				  :weight 'bold
				  :inherit 'default)))
  :config
  (global-hl-todo-mode)
  (add-to-list 'hl-todo-keyword-faces `("TODO" . ,danylo/black))
  (add-to-list 'hl-todo-keyword-faces `("FIXME" . ,danylo/black)))

(use-package rainbow-mode
  ;; https://github.com/emacsmirror/rainbow-mode
  ;; Colorize color names in buffers
  :hook ((LaTeX-mode . (lambda () (rainbow-mode 1)))
	 (emacs-lisp-mode . (lambda () (rainbow-mode 1))))
  )

(use-package filladapt
  ;; https://elpa.gnu.org/packages/filladapt.html
  ;; Enhance the behavior of Emacs' Auto Fill mode
  :hook ((c-mode-common . (lambda ()
			    (when (featurep 'filladapt)
			      (c-setup-filladapt))))
	 (python-mode . filladapt-mode)
	 (org-mode . filladapt-mode)
	 (text-mode . filladapt-mode))
  :bind (("M-q" . 'fill-paragraph)
	 ("M-r" . 'fill-region)))

(use-package so-long
  ;; https://www.emacswiki.org/emacs/SoLong
  ;; Improve performance for long lines
  :config
  (global-so-long-mode 1))

;; Remove a significant contributor to line scan slowness
(setq-default bidi-display-reordering nil)

;; Delete trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;;; Section delimiters

;;;###autoload
(defun danylo/section-msg (left msg right start end &optional nospace)
  "Section delimiters for comment.
LEFT and RIGHT are the section delimineters.
MSG is the section name.
START and END give the start/end of current selection.
NOSPACE, if t, means that there is no spacing added between delimiters."
  (interactive "r")
  (if (use-region-p)
      (let ((regionp (buffer-substring start end)))
	(delete-region start end)
	(if nospace
	    (insert (format "%s%s%s" left regionp right))
	  (insert (format "%s %s %s" left regionp right))))
    (if nospace
	(insert (format "%s%s%s" left msg right))
      (insert (format "%s %s %s" left msg right)))))

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

;;;; Duplicate line

;;;###autoload
(defun danylo/duplicate-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated. However, if
there's a region, all lines that region covers will be duplicated."
  (interactive "p")
  (let (beg end (origin (point)))
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (let ((region (buffer-substring-no-properties beg end)))
      (dotimes (i arg)
        (goto-char end)
        (newline)
        (insert region)
        (setq end (point)))
      (goto-char (+ origin (* (length region) arg) arg)))))

(global-set-key (kbd "C-c d") 'danylo/duplicate-line-or-region)

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

;;;; Fill column (line width)

(setq-default fill-column danylo/fill-column)

;;;###autoload
(defun danylo/set-whitespace-mode-colors ()
  "Set colors for whitespace-mode for overflowing lines."
  (set-face-attribute 'whitespace-line nil
		      :foreground `,danylo/yellow
		      :background nil
		      :weight 'bold
		      :inherit 'default))

;; Indicate lines that are too wide
(use-package whitespace
  :ensure nil
  :init (setq whitespace-line-column nil
	      whitespace-style '(face lines-tail))
  (add-hook 'whitespace-mode-hook 'danylo/set-whitespace-mode-colors)
  :config
  (global-whitespace-mode t))

(require 'whitespace)
(danylo/set-whitespace-mode-colors)

;;; ..:: Window management ::..

;;;; >> Movement across windows <<

(general-define-key
 "C-<left>" 'windmove-left
 "C-<right>" 'windmove-right
 "C-<up>" 'windmove-up
 "C-<down>" 'windmove-down)

;;;###autoload
(defun danylo/switch-to-last-window ()
  "Return to the last active window."
  (interactive)
  (let ((win (get-mru-window t t t)))
    (unless win (error "Last window not found"))
    (let ((frame (window-frame win)))
      (select-frame-set-input-focus frame)
      (select-window win))))

;;;; >> Splitting windows <<

(general-define-key
 "C-x -" 'split-window-below
 "C-x |" 'split-window-right)

;;;; >> Resizing windows <<

(use-package windsize
  ;; https://github.com/grammati/windsize
  ;; Easy resizing of emacs windows
  :init (setq windsize-cols 1
	      windsize-rows 1)
  :config
  (windsize-default-keybindings))

;;;###autoload
(defun danylo/set-window-width (width)
  "Set the selected window's width."
  (let ((delta (- width (window-width))))
    (if (>= delta 0)
	(enlarge-window-horizontally delta)
      (shrink-window-horizontally (* -1 delta)))))

;;;###autoload
(defun danylo/set-window-columns-precise ()
  "Set the selected window to user-specified number of columns.
Default is 80"
  (interactive)
  (let ((desired-width
	 (let ((user-input (read-string
			    (format "input [default %d]: "
				    danylo/fill-column))))
	   (cond ((string= user-input "")
		  ;; Default column width
		  danylo/fill-column)
		 ((not (string-match "\\`[1-9][0-9]*\\'" user-input))
		  ;; Bad input - ignore
		  nil)
		 (t
		  ;; Good input - record as an integer
		  (string-to-number user-input))))))
    (when desired-width
      (danylo/set-window-width desired-width))))

(general-define-key
 "C-x ~" 'danylo/set-window-columns-precise)

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

;;;; Improve quit-window behavior (automatically delete-window sometimes)

;;;###autoload
(defun danylo/quit-and-kill-window (orig-fun &rest args)
  "Optionally also kill the window after quitting it."
  (setq delete-this-window nil)
  ;; Check if window should be deleted
  (require 'flycheck)
  (mapc
   (lambda (val)
     (setq delete-this-window
	   (or delete-this-window (string= (buffer-name) val))))
   `(,flycheck-error-list-buffer))
  ;; Quit or delete window as appropriate
  (if delete-this-window
      (progn
	(apply orig-fun args)
	(delete-window))
    (apply orig-fun args)))

(advice-add 'quit-window :around #'danylo/quit-and-kill-window)

;;; ..:: Terminal emulator ::..

;;;###autoload
(defun danylo/run-terminator-here ()
  "Run terminal from current buffer"
  (interactive "@")
  (shell-command (concat "terminator > /dev/null 2>&1 & disown") nil nil))

(general-define-key
 "C-c t e" 'ansi-term
 "C-c t r" 'danylo/run-terminator-here)

;;;###autoload
(defadvice ansi-term (before force-bash)
  "Always use bash"
  (interactive (list "/bin/bash")))

(add-hook 'after-init-hook (lambda  () (ad-activate 'ansi-term)))

;;;###autoload
(defun danylo/ansi-term-use-utf8 ()
  "Display of certain characters and control codes to UTF-8"
  (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))
(add-hook 'term-exec-hook 'danylo/ansi-term-use-utf8)

;; Clickable URLs
(add-hook 'term-mode-hook (lambda () (goto-address-mode)))

;;;###autoload
(defadvice term-sentinel (around my-advice-term-sentinel (proc msg))
  "Make that typing exit in ansi-term (which exits the shell)
also closes the buffer"
  (if (memq (process-status proc)
	    '(signal exit))
      (let ((buffer (process-buffer proc)))
	ad-do-it
	(kill-buffer buffer))
    ad-do-it))

(add-hook 'after-init-hook (lambda  () (ad-activate 'term-sentinel)))

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
  (defun danylo/toggle-term-line-char-mode ()
    (interactive)
    (if (term-in-char-mode)
	(danylo/switch-to-term-line-mode)
      (danylo/switch-to-term-char-mode)))
  (define-key term-raw-map (kbd "C-c t c") 'danylo/switch-to-term-char-mode)
  (define-key term-raw-map (kbd "C-c t l") 'danylo/switch-to-term-line-mode)
  (define-key term-raw-map (kbd "C-c t t") 'danylo/toggle-term-line-char-mode)
  (define-key term-mode-map (kbd "C-c t c") 'danylo/switch-to-term-char-mode)
  (define-key term-mode-map (kbd "C-c t l") 'danylo/switch-to-term-line-mode)
  (define-key term-mode-map (kbd "C-c t t") 'danylo/toggle-term-line-char-mode)
  ;; Copy/paste native Emacs keystrokes
  (define-key term-raw-map (kbd "C-k") 'term-send-raw)
  (define-key term-raw-map (kbd "C-y") 'term-paste)
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

;;; ..:: Syntax checking ::..

;; Size of flycheck error list
(defconst flycheck-error-list-height 10)

(use-package flycheck
  ;; https://github.com/flycheck/flycheck
  ;; A modern on-the-fly syntax checking extension
  :hook ((c-mode-common . flycheck-mode)
	 (python-mode . flycheck-mode)
	 (sh-mode . flycheck-mode)
	 (julia-mode . flycheck-mode))
  :bind (("C-c f e" . flycheck-display-error-at-point)
	 ("C-c f v" . flycheck-verify-setup)
	 ("C-c f l" . danylo/flycheck-list-errors)
	 :map flycheck-mode-map
	 ("C-c ! l" . danylo/flycheck-list-errors))
  :init
  (setq flycheck-enabled-checkers '(c/c++-gcc)
	flycheck-check-syntax-automatically '(mode-enabled save)
	flycheck-display-errors-delay 0.5
	flycheck-indication-mode 'left-fringe
	flycheck-highlighting-style `(conditional
				      ,most-positive-fixnum
				      level-face (delimiters "" "")))
  (add-hook 'flycheck-mode-hook
	    (lambda ()
	      )))

(with-eval-after-load "flycheck"
  ;; Update Flycheck fringe indicators
  ;; Inspired by Spacemacs (https://github.com/syl20bnr/spacemacs)
  (flycheck-define-error-level 'error
    :severity 2
    :overlay-category 'flycheck-error-overlay
    :fringe-bitmap 'danylo/flycheck-fringe-indicator
    :error-list-face 'flycheck-error-list-error
    :fringe-face 'flycheck-fringe-error)
  (flycheck-define-error-level 'warning
    :severity 1
    :overlay-category 'flycheck-warning-overlay
    :fringe-bitmap 'danylo/flycheck-fringe-indicator
    :error-list-face 'flycheck-error-list-warning
    :fringe-face 'flycheck-fringe-warning)
  (flycheck-define-error-level 'info
    :severity 0
    :overlay-category 'flycheck-info-overlay
    :fringe-bitmap 'danylo/flycheck-fringe-indicator
    :error-list-face 'flycheck-error-list-info
    :fringe-face 'flycheck-fringe-info)
  ;; Removes the "hatch pattern" for multi-line errors
  (define-fringe-bitmap 'flycheck-fringe-bitmap-continuation
    (vector #b0))
  ;; Update faces
  (set-face-attribute 'flycheck-fringe-error nil :foreground `,danylo/red)
  (set-face-attribute 'flycheck-error nil :underline `,danylo/red)
  (set-face-attribute 'flycheck-fringe-warning nil :foreground `,danylo/yellow)
  (set-face-attribute 'flycheck-warning nil :underline `,danylo/yellow)
  (set-face-attribute 'flycheck-fringe-info nil :foreground `,danylo/green)
  (set-face-attribute 'flycheck-info nil :foreground `,danylo/green))

;;;###autoload
(defun danylo/flycheck-list-errors ()
  "Open the list of errors in the current buffer."
  (interactive)
  (unless flycheck-mode
    (user-error "Flycheck mode not enabled"))
  ;; Create and initialize the error list
  (unless (get-buffer flycheck-error-list-buffer)
    (with-current-buffer (get-buffer-create flycheck-error-list-buffer)
      (flycheck-error-list-mode)))
  ;; Reset the error filter
  (flycheck-error-list-reset-filter)
  ;; Open the error buffer
  (let ((source (current-buffer)))
    (if (get-buffer-window flycheck-error-list-buffer)
	(progn
	  (pop-to-buffer flycheck-error-list-buffer)
	  (flycheck-error-list-set-source source))
      (let ((this-window (selected-window))
	    (new-window (split-window-vertically
			 (* -1 (max 4 (min danylo/flycheck-error-list-size
					   (/ (window-total-height) 2)))))))
	(select-window new-window)
	(switch-to-buffer flycheck-error-list-buffer)
	(flycheck-error-list-set-source source)
	(select-window this-window)))))

;;;###autoload
(defun danylo/flycheck-jump-in-buffer (orig-fun &rest args)
  "In BUFFER, jump to ERROR.
Patched so that if the last window visited is the buffer, jump to that one.
This improves the default behaviour of just jumping to any open window that
holds the buffer, which can be erratic.
The remainder of the function is a carbon-copy from Flycheck."
  (setq buffer (nth 0 args)
	error (nth 1 args))
  (if (eq (window-buffer) (get-buffer flycheck-error-list-buffer))
      (progn
	;; The patch: try first by going to last window
	(let ((current-window (selected-window)))
	  (danylo/switch-to-last-window)
	  (unless (eq (current-buffer) buffer)
	    (pop-to-buffer buffer 'other-window))))
    (switch-to-buffer buffer))
  (let ((pos (flycheck-error-pos error)))
    (unless (eq (goto-char pos) (point))
      (widen)
      (goto-char pos)))
  (flycheck-error-list-highlight-errors 'preserve-pos))
(advice-add 'flycheck-jump-in-buffer :around #'danylo/flycheck-jump-in-buffer)

;;;###autoload
(defun danylo/toggle-spellcheck ()
  "Turn on spell checking."
  (interactive)
  (if (bound-and-true-p flyspell-mode)
      ;; Turn off spellcheck
      (progn (flycheck-mode -1)
	     (flyspell-mode -1))
    ;; Turn on spellcheck
    (progn (flycheck-mode +1)
	   (flyspell-mode +1)
	   (flyspell-buffer))))

(add-hook 'flyspell-mode-hook
	  (lambda ()
	    (set-face-attribute 'flyspell-incorrect nil
				:underline `,danylo/red)
	    (set-face-attribute 'flyspell-duplicate nil
				:underline `,danylo/yellow)))

(general-define-key
 "C-c x s" 'danylo/toggle-spellcheck)

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

;;;###autoload
(defun danylo/maintain-xref-history (orig-fun &rest args)
  "Push marker to stack to that M-, works to jump back."
  (xref-push-marker-stack)
  (apply orig-fun args))

(advice-add 'counsel-projectile-ag :around #'danylo/maintain-xref-history)

(use-package projectile
  ;; https://github.com/bbatsov/projectile
  ;; Project interaction library offering tools to operate on a project level
  :init (setq projectile-enable-caching nil)
  :bind (:map projectile-mode-map
	      ("C-c p" . projectile-command-map)
	      ("C-c p s g" . counsel-projectile-ag))
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
	org-hide-leading-stars nil)
  (visual-line-mode t))

(use-package org
  ;; https://github.com/bzg/org-mode
  ;; Your life in plain text
  :hook ((org-mode . danylo/org-mode-setup))
  :bind (:map org-mode-map
	      ("C-c x s" . danylo/toggle-spellcheck)
	      ("M-." . org-open-at-point)
	      ("M-," . org-mark-ring-goto))
  :init
  (setq org-startup-folded nil
	;;org-ellipsis "..." ;; " ▾"
	org-src-tab-acts-natively t
	org-startup-with-latex-preview nil
	org-fontify-quote-and-verse-blocks t))

(with-eval-after-load "org"
  (define-key org-mode-map [remap fill-paragraph] nil)
  ;; Make sure buffer motion across windows keys are unaffected
  (define-key org-mode-map (kbd "S-M-<up>") nil)
  (define-key org-mode-map (kbd "S-M-<down>") nil)
  (define-key org-mode-map (kbd "S-M-<left>") nil)
  (define-key org-mode-map (kbd "S-M-<right>") nil)
  ;; LaTeX equations preview style
  (setq org-format-latex-options (plist-put org-format-latex-options
					    :scale `,danylo/latex-preview-scale)
	org-format-latex-options (plist-put org-format-latex-options
					    :foreground `,danylo/yellow))
  ;; Open PDF with Evince
  ;; see: https://stackoverflow.com/a/9116029/4605946
  (setcdr (assoc "\\.pdf\\'" org-file-apps) "evince %s")
  (add-to-list 'org-file-apps
	       '("\\.pdf::\\([0-9]+\\)\\'" . "evince -p %1 %s") t))

;;;###autoload
(defun danylo/org-emphasize (char)
  "Emphasize text in org-mode. CHAR is the character to wrap the
  text with."
  (interactive)
  (org-emphasize (string-to-char char)))

;;;###autoload
(defun danylo/org-emphasize-equation (&optional start end)
  "Wrap an equation in $ symbols in org-mode."
  (interactive (if (use-region-p) (list (region-beginning) (region-end))))
  (if start
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
  (define-key org-mode-map (kbd "C-c f c")
    (lambda () (interactive) (danylo/org-emphasize "~")))
  (define-key org-mode-map (kbd "C-c f e") 'danylo/org-emphasize-equation))

;;; ..:: Email ::..

(use-package mu4e
  ;; https://github.com/djcb/mu
  ;; maildir indexer/searcher + emacs mail client + guile bindings
  :ensure nil
  :load-path "/usr/local/share/emacs/site-lisp/mu4e"
  :bind (:map mu4e-headers-mode-map
	      ("x" . (lambda () (interactive) (mu4e-mark-execute-all t)))
	      ("C-c C-u" . danylo/get-mail)
	      :map mu4e-view-mode-map
	      ("x" . (lambda () (interactive) (mu4e-mark-execute-all t)))
	      :map mu4e-compose-mode-map
	      ("C-c C-s C-s" . 'message-send)
	      ("C-c C-c" . nil)
	      ("C-c C-a" . 'mail-add-attachment)
	      ("C-c x s" . danylo/toggle-spellcheck)
	      :map mu4e-main-mode-map
	      ("C-c C-u" . danylo/get-mail)
	      ("U" . danylo/get-mail))
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
	message-kill-buffer-on-exit t)
  (advice-add #'shr-colorize-region :around
	      (defun shr-no-colourise-region (&rest ignore))))

;;;###autoload
(defun danylo/launch-mu4e (arg)
  "Launch mu4e, or quit it if preceded by C-u"
  (interactive "P")
  (if arg (mu4e-quit) (mu4e)))

(general-define-key
 "C-c m" 'danylo/launch-mu4e)

(defvar danylo/got-mail nil
  "Indicator that mailbox has been updated")

;;;###autoload
(defun danylo/get-mail (&optional arg)
  "Get new email silently."
  (interactive "P")
  (unless danylo/got-mail
    (mu4e-update-mail-and-index t)
    (danylo/print-in-minibuffer
     (format "%s Refreshed inbox" (danylo/fa-icon "inbox")))
    (setq danylo/got-mail t)
    (run-with-timer danylo/get-mail-min-interval nil
		    (lambda () (setq danylo/got-mail nil)))))

;;;###autoload
(defun danylo/mu4e-error-handler (orig-fun &rest args)
  "Handler function for showing an error.
Patched for my own better error messages."
  (let ((errcode (nth 0 args))
	(errmsg (nth 1 args)))
    (cl-case errcode
      (1 (danylo/print-in-minibuffer "mu4e database locked"))
      (4 (user-error "No matches for this search query."))
      (t (error "Error %d: %s" errcode errmsg)))))

(advice-add 'mu4e-error-handler :around #'danylo/mu4e-error-handler)

(with-eval-after-load "mu4e"
  ;; Disable message sending with C-c C-s (make it more complicated to
  ;; not send messages by accident)
  (define-key mu4e-compose-mode-map (kbd "C-c C-s") 'nil)
  ;; Setup a timer to update the email inbox in the background
  (run-with-timer danylo/email-refresh-period danylo/email-refresh-period
		  (lambda ()
		    (with-temp-buffer
		      ;; Check that mu server is not already running
		      (call-process "ps" nil t nil "aux")
		      (unless (eq (call-process-region
				   nil nil "grep" nil nil nil "mu") 0)
			;; Get new mail
			(danylo/get-mail)
			;; Kill the mu process once updating has finished
			(run-with-timer
			 0.5 nil
			 (lambda ()
			   (mu4e~proc-kill)
			   (mu4e-alert-enable-mode-line-display)))))
		    )))

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
(defun danylo/message-kill-buffer (orig-fun &rest args)
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
(advice-add 'message-kill-buffer :around #'danylo/message-kill-buffer)

(with-eval-after-load "mu4e"
  (set-face-attribute 'mu4e-unread-face nil :foreground `,danylo/yellow)
  (set-face-attribute 'mu4e-header-highlight-face nil :underline nil))

;; Do not back up email while writing it, which makes drafts
;; accumulate
(add-hook 'mu4e-compose-mode-hook
	  #'(lambda ()
	      (auto-save-mode -1)
	      (make-local-variable 'make-backup-files)
	      (setq make-backup-files nil)))

(use-package danylo-email
  ;; Personal mu4e context setup.
  :ensure nil
  :load-path danylo/emacs-custom-lisp-dir)

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

(use-package mu4e-alert
  ;; https://github.com/iqbalansari/mu4e-alert
  ;; Desktop notifications and modeline display for mu4e
  :init
  (add-hook 'after-init-hook #'mu4e-alert-enable-mode-line-display))

;;; ..:: Git ::..

(use-package magit
  ;; https://github.com/magit/magit
  ;; An interface to the version control system Git
  :bind (("C-x g" . magit-status)))

;;; ..:: Shell interaction ::..

;;;###autoload
(defun danylo/shell~check-open (shell-buffer-name)
  "Check if a shell is running."
  (if (get-buffer shell-buffer-name) t nil))

;;;###autoload
(defun danylo/shell-open (shell-buffer-name shell-type &optional in-place)
  "Open a shell."
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
  "Run command in the shell.
If there is no shell open, prints a message to inform."
  (if (danylo/shell~check-open shell-buffer-name)
      ;; Send a run command for the current file
      (with-current-buffer shell-buffer-name
	(term-send-raw-string (format "%s\n" command)))
    (message "No shell open.")))

;;;###autoload
(defun danylo/shell-get-point (shell-buffer-name)
  "Get the current location of point in the shell."
  (setq current-point nil)
  (if (danylo/shell~check-open shell-buffer-name)
      (with-current-buffer shell-buffer-name
	(danylo/switch-to-term-char-mode)
	(danylo/switch-to-term-line-mode)
	(setq current-point (point))
	(danylo/switch-to-term-char-mode))
    (message "No shell open."))
  current-point)

;;;###autoload
(defun danylo/shell-get-content (shell-buffer-name start end)
  "Get the shell text between START and END point positions."
  (setq shell-content nil)
  (if (danylo/shell~check-open shell-buffer-name)
      (with-current-buffer shell-buffer-name
	(setq in-char-mode (term-in-char-mode))
	(danylo/switch-to-term-line-mode)
	(let ((start (max start (point-min)))
	      (end (min end (point-max))))
	  (setq shell-content (buffer-substring start end)))
	(when in-char-mode (danylo/switch-to-term-char-mode)))
    (message "No shell open."))
  shell-content)

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
	lsp-headerline-breadcrumb-enable nil ;; Disable headerline breadcrumbs
	)
  :hook
  ((lsp-mode . (lambda ()
		 (setq company-minimum-prefix-length 1)
		 (push 'company-capf company-backends))))
  )

;; Replace LSP spinner with a static gear (I like this better visually)

;;;###autoload
(defun danylo/lsp-gear-show (orig-fun &rest args)
  "Replace LSP spinner with a static gear, while the LSP process executes."
  (mapc
   (lambda (buf)
     (with-current-buffer buf
       (when (and buffer-file-name
		  (boundp 'lsp-mode))
	 ;; Snow process icon
	 (setq mode-line-process
	       (propertize
		"   " 'display
		(propertize (danylo/fa-icon "cogs") 'face
   			    `(:family ,(all-the-icons-faicon-family)
				      :inherit doom-modeline-warning)))))))
   (buffer-list))
  (force-mode-line-update t))

;;;###autoload
(defun danylo/lsp-gear-hide (orig-fun &rest args)
  "Hide the gear icon."
  (when (--all? (eq (lsp--workspace-status it) 'initialized)
                lsp--buffer-workspaces)
    (mapc
     (lambda (buf)
       (with-current-buffer buf
	 (when (and buffer-file-name
		    (boundp 'lsp-mode))
	   (setq mode-line-process ""))))
     (buffer-list))
    (force-mode-line-update t)))

(advice-add 'lsp--spinner-start :around #'danylo/lsp-gear-show)
(advice-add 'lsp--spinner-stop :around #'danylo/lsp-gear-hide)

;;;; Open files manually, rather than through xref

;;;###autoload
(defun danylo/ffap (&optional filename)
  "A variant of find-file-at-point without prompting.
Source: https://www.reddit.com/r/emacs/comments/676r5b/how_to_stop_
        findfileatprompting_when_there_is_a/dgsr20f?utm_source=share&utm_
        medium=web2x&context=3"
  (let* ((name (or filename (ffap-string-at-point 'file)))
         (fname (expand-file-name name)))
    (if (and name fname (file-exists-p fname))
        (find-file fname)
      (find-file-at-point filename))))

;;;###autoload
(defun danylo/xref-find-definitions (orig-fun &rest args)
  "Wraps xref-find-definitions so that we open files manually.
This avoids some errors, such as Julia LSP server failing with
relative file paths."
  (if (nth 3 (syntax-ppss))
      ;; Open file by ourselves
      (progn
	(xref-push-marker-stack) ;; Add current marker to stack, so M-, works
	(danylo/ffap))
    ;; Call xref as originally intended
    (apply orig-fun args)))

(require 'ffap)
(advice-add 'xref-find-definitions :around #'danylo/xref-find-definitions)
;; (advice-remove 'xref-find-definitions #'danylo/xref-find-definitions)

;;;; If cannot find definition, search project with AG

;;;###autoload
(defun danylo/xref-find-defintion-with-ag-fallback (orig-fun &rest args)
  "Use AG as a fallback in case xref standard command fails to
find a definion."
  (condition-case nil
      (apply orig-fun args)
      (user-error (counsel-projectile-ag))))

(advice-add 'xref-find-definitions :around
	    #'danylo/xref-find-defintion-with-ag-fallback)

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

(use-package python
  ;; Emacs built-in
  ;; Python editing major mode
  :ensure nil
  :bind (:map python-mode-map
	      ;; Make indentation automatic, especially ensures correct
	      ;; indentation in docstrings (see
	      ;; https://emacs.stackexchange.com/a/28445/13661)
	      ("RET" . 'newline-and-indent))
  :init (setq python-indent-guess-indent-offset t
	      python-indent-guess-indent-offset-verbose nil
	      python-fill-docstring-style 'pep-257-nn))

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
	   (danylo/fa-icon "info" `,danylo/yellow)
	   (propertize string 'face
		       `(:foreground ,danylo/yellow))))

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

;;;; Imenu setup

;;;###autoload
(defun danylo/python-imenu ()
  (let ((python-imenu (imenu--generic-function
		       imenu-generic-expression)))
    (append python-imenu)))

;;;###autoload
(defun danylo/python-imenu-hooks ()
  (setq imenu-generic-expression
	'(("Function" "^[[:blank:]]*def \\(.*\\).*(.*$" 1)
	  ("Class" "^class \\(.*\\).*:$" 1)))
  (setq imenu-create-index-function 'danylo/python-imenu)
  ;; Rescan the buffer as contents are added
  (setq imenu-auto-rescan t)
  )

(add-hook 'python-mode-hook 'danylo/python-imenu-hooks)

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
(defun danylo/smart-select-region (start end)
  "Select region in file, removing possible indent of all
lines according to the first line."
  (interactive "r")
  (if (use-region-p)
      (let* ((selection (buffer-substring start end))
	     (starting-spaces (progn (string-match "^\s+" selection)
				     (match-end 0)))
	     (starting-space-replace-regexp
	      `,(format "^\s\\{%d\\}" starting-spaces))
	     ;; Remove the initial spaces
	     (trimmed-selection (replace-regexp-in-string
				 starting-space-replace-regexp "" selection))
	     ;; Remove any trailing newlines
	     (trimmed-selection (replace-regexp-in-string "\n$" "" trimmed-selection)))
	trimmed-selection)
    ;; No region active
    (message "No region selected.")
    nil))

;;;###autoload
(defun danylo/python-shell-run-region (start end)
  "Run highlighted selection in file."
  (interactive "r")
  (let ((command (danylo/smart-select-region start end)))
    (when command
      (setq command (format "%s\n" command))
      (danylo/shell-exec danylo/python-buffer-name "%autoindent\n")
      (danylo/shell-exec danylo/python-buffer-name command)
      (danylo/shell-exec danylo/python-buffer-name "%autoindent\n")
      )))

;;;###autoload
(defun danylo/python-config ()
  ;; Key bindings
  (define-key python-mode-map (kbd "C-c C-s") 'danylo/python-shell-open)
  (define-key python-mode-map (kbd "C-c C-f") 'danylo/python-shell-run-file)
  (define-key python-mode-map (kbd "C-c C-r") 'danylo/python-shell-run-region)
  (define-key python-mode-map (kbd "C-c C-p") nil))
(add-hook 'python-mode-hook (lambda () (danylo/python-config)))

;;; ..:: Julia ::..

(use-package julia-mode
  ;; https://github.com/JuliaEditorSupport/julia-emacs
  ;; Major mode for the julia programming language
  :hook ((julia-mode . yas-minor-mode))
  :bind (:map julia-mode-map
	      ("C-h ." . danylo/julia-help-at-point)))

(use-package lsp-julia
  ;; https://github.com/non-Jedi/lsp-julia
  ;; Julia support for lsp-mode using LanguageServer.jl
  :after lsp-mode
  :ensure nil
  :quelpa ((lsp-julia :fetcher github
                      :repo "non-Jedi/lsp-julia"))
  :hook ((julia-mode . (lambda () (require 'lsp-julia) (lsp))))
  :init (setq lsp-julia-package-dir nil
	      lsp-julia-default-environment "~/.julia/environments/v1.5")
  :config
  (require 'lsp-julia))

(use-package julia-staticlint
  ;; https://github.com/dmalyuta/julia-staticlint
  ;; Emacs Flycheck support for StaticLint.jl
  :ensure nil
  :quelpa ((julia-staticlint :fetcher github
			     :repo "dmalyuta/julia-staticlint"
			     :files (:defaults "julia_staticlint_server.jl"
					       "julia_staticlint_client.jl")))
  :hook ((julia-mode . julia-staticlint-activate))
  :config
  (julia-staticlint-init))

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
	 (command (format "include(\"%s\");" file-name)))
    (danylo/shell-exec danylo/julia-buffer-name command)
    ))

;;;###autoload
(defun danylo/julia-shell-run-region (start end)
  "Run highlighted selection in file."
  (interactive "r")
  (let ((command (danylo/smart-select-region start end)))
    (when command
      (setq command (format "%s\n" command))
      (danylo/shell-exec danylo/julia-buffer-name command))))

;;;###autoload
(defun danylo/julia-config ()
  ;; Key bindings
  (define-key julia-mode-map (kbd "C-c C-s") 'danylo/julia-shell-open)
  (define-key julia-mode-map (kbd "C-c C-f") 'danylo/julia-shell-run-file)
  (define-key julia-mode-map (kbd "C-c C-r") 'danylo/julia-shell-run-region)
  (define-key julia-mode-map (kbd "C-c C-l") 'julia-latexsub-or-indent))
(add-hook 'julia-mode-hook (lambda () (danylo/julia-config)))

;;;; Get help at point

(defconst julia-docstring-refresh-rate 0.5
  "How often to check Julia REPL if docstring has finished printing.")

;;;###autoload
(defun danylo/julia-help-at-point (&optional start end)
  "Query Julia REPL for help about thing at point."
  (interactive (if (use-region-p) (list (region-beginning) (region-end))))
  (let ((thing (if start (buffer-substring start end)
		 (thing-at-point 'symbol)))
	(start-point (danylo/shell-get-point danylo/julia-buffer-name)))
    (when start-point
      ;; Record the help string
      (danylo/shell-exec danylo/julia-buffer-name (format "?%s" thing))
      (setq julia-help-docstring "")
      (run-with-timer julia-docstring-refresh-rate nil
		      'danylo/julia-show-help-docstring start-point
		      julia-help-docstring))))

;;;###autoload
(defun danylo/julia-show-help-docstring (start-point last-docstring)
  "Record the docstring output by the Julia REPL and put it into a help buffer.
Calls itself until the docstring has completed printing."
  ;; Get end point, removing the final "julia> " prompt
  (setq end-point (- (danylo/shell-get-point danylo/julia-buffer-name) 8))
  (setq julia-help-docstring
	(danylo/shell-get-content danylo/julia-buffer-name
				  start-point end-point))
  (if (string= julia-help-docstring last-docstring)
      ;; Show help buffer
      (progn
	(let ((this-window (selected-window))
	      (julia-help-existing-window
	       (get-buffer-window danylo/julia-help-buffer-name)))
	  (when julia-help-existing-window
	    (select-window julia-help-existing-window)
	    (kill-buffer-and-window)
	    (select-window this-window)))
	(let ((this-window (selected-window))
	      (new-window (split-window-vertically)))
	  (select-window new-window)
	  (setq julia-help-buf
		(get-buffer-create danylo/julia-help-buffer-name))
	  (switch-to-buffer julia-help-buf)
	  (insert "[Press q to quit]\n\n")
	  (insert julia-help-docstring)
	  (special-mode)
	  (danylo-julia-help-mode)
	  (goto-char (point-min))
	  ;; Go back to original window
	  (select-window this-window)
	  ;; Return to help window. Quitting will return cursor to original
	  ;; window
	  (select-window new-window)
	  ))
    ;; Run again in a little while
    (run-with-timer julia-docstring-refresh-rate nil
		    'danylo/julia-show-help-docstring start-point
		    julia-help-docstring))
  )

(define-minor-mode danylo-julia-help-mode
  "Minor mode for Julia help buffer."
  :init-value nil
  :lighter " danylo-julia-help"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "q") 'kill-buffer-and-window)
            map)
  (read-only-mode +1))

;;;; Imenu setup

;;;###autoload
(defun danylo/julia-imenu ()
  (let ((python-imenu (imenu--generic-function
		       imenu-generic-expression)))
    (append python-imenu)))

;;;###autoload
(defun danylo/julia-imenu-hooks ()
  (setq imenu-generic-expression
	'(("Function" "^[[:blank:]]*function \\(.*\\).*(.*$" 1)
	  ("Struct" "^[^#]*\s+struct\s+\\(.*?\\)$" 1)
	  ("Struct" "^struct\s+\\(.*?\\)$" 1)))
  (setq imenu-create-index-function 'danylo/julia-imenu)
  ;; Rescan the buffer as contents are added
  (setq imenu-auto-rescan t))

(add-hook 'julia-mode-hook 'danylo/julia-imenu-hooks)

;;;; Block comment

;;;###autoload
(defun danylo/julia-block-comment (start end)
  "Julia block comment."
  (interactive "r")
  (danylo/section-msg "#=\n" "" "=#" start end t))

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
	 (LaTeX-mode . (lambda ()
			 ;; View program
			 (setq TeX-view-program-list
			       '(("Evince"
				  "evince --page-index=%(outpage) %o")))
			 (setq TeX-view-program-selection
			       '((output-pdf "Evince")))
			 ;; Other environments
			 (LaTeX-add-environments "equation*")
			 (LaTeX-add-environments "tikzpicture")
			 (LaTeX-add-environments "pgfonlayer")
			 ;; Line-breaking math
			 (add-to-list 'fill-nobreak-predicate 'texmathp)
			 ;; Helm for candidate completion
			 (require 'helm-mode)
			 (add-to-list 'helm-completing-read-handlers-alist
				      '(LaTeX-environment
					. helm-completing-read-default-handler))
			 ))
	 (reftex-mode . (lambda ()
			  ;; Unset C-c /, which conflicts with google-this
			  (define-key reftex-mode-map (kbd "C-c /") 'nil)
			  (define-key reftex-mode-map (kbd "M-.")
			    'reftex-view-crossref))))
  :init (setq TeX-source-correlate-method 'synctex
	      TeX-source-correlate-start-server t
	      TeX-install-font-lock 'font-latex-setup
	      TeX-auto-save t
	      TeX-parse-self t
	      TeX-auto-regexp-list 'TeX-auto-full-regexp-list
	      TeX-auto-parse-length 999999
	      TeX-save-query nil
	      reftex-plug-into-AUCTeX t
	      LaTeX-electric-left-right-brace t
	      TeX-electric-math '("$" . "$")
	      texmathp-tex-commands '(("optimization" env-on)))
  (setq-default TeX-master nil)
  (add-to-list 'ivy-ignore-buffers '"\\*.*output\\*")
  :bind (:map LaTeX-mode-map
	      ("C-c i w" . ispell-word)
	      ("C-x C-<backspace>" . electric-pair-delete-pair)
	      ("C-c f e" . danylo/org-emphasize-equation)
	      ("C-c x s" . danylo/toggle-spellcheck))
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

;;;###autoload
(defun danylo/TeX-dwim-master (orig-fun &rest args)
  "Find a likely `TeX-master'.
Patched so that any new file by default is guessed as being its own master."
  nil)
(advice-add 'TeX-dwim-master :around #'danylo/TeX-dwim-master)

(use-package company-auctex
  ;; https://github.com/alexeyr/company-auctex
  ;; company-mode autocompletion for auctex
  :after company
  :hook ((LaTeX-mode . (lambda ()
			 ;; Make completion case sensitive
			 (setq company-dabbrev-downcase nil))))
  :bind (:map LaTeX-mode-map
	      ("C-c c" . company-auctex-macros)
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

;;; ..:: Bash ::..

(mapcar
 (lambda (arg)
   (add-to-list 'auto-mode-alist `(,arg . sh-mode)))
 '("\\.bash_aliases\\'"
   "\\.local.bashrc\\'"
   "\\.local.bashrc.private\\'"
   "\\.profile\\'"))

;;; ..:: XML ::..

(use-package esxml
  ;; https://github.com/tali713/esxml
  ;; An elisp library for working with xml, esxml and sxml.
  ;; **Loading this just because unpackaged needs it**
  )
