(let ((gc-cons-threshold most-positive-fixnum) ;; better garbage collection
      (file-name-handler-alist nil))
  ;; sensible GUI
  (menu-bar-mode -1)  ;; disable menubar
  (tool-bar-mode -1) ;; disable toolbar
  (toggle-scroll-bar -1) ;; disable scrollbar

  ;; MELPA
  (require 'package)
  (setq package-enable-at-startup nil)
  (setq package-check-signature nil)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
  (package-initialize)

  ;; boostrap 'use-package'
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))
  (eval-when-compile
    (require 'use-package))
  (setq personal-keybindings ()) ;; so that byte compiler does not complain about void-variable

;;;;;;;;;;;;;;;;; EMACS BUILT-IN

  (use-package windmove
    ;; move cursor between windows
    :demand
    :bind
    (("C-<left>" . windmove-left)
     ("C-<right>" . windmove-right)
     ("C-<up>" . windmove-up)
     ("C-<down>" . windmove-down)))

  (use-package buffer-menu
    ;; show all current buffers
    :init
    (setq Buffer-menu-name-width 25)
    (setq Buffer-menu-mode-width 6)
    (setq Buffer-menu-size-width 10)
    :bind
    (("C-x C-b" . buffer-menu)))

  (use-package cedet
    ;; collection of Emacs development environment tools
    :demand
    :bind
    (:map c-mode-base-map
	  ("C-c n" . senator-next-tag)
	  ("C-c p" . senator-previous-tag)
	  ("C-c u" . senator-go-to-up-reference)
	  ("C-c M-w" . senator-copy-tag)
	  ("C-c C-w" . senator-kill-tag)
	  ("C-c C-y" . senator-yank-tag))
    :config
    (require 'cc-mode)
    ;; Semantic
    ;; Useful commands:
    ;;   M-x company-semantic
    ;;   M-x semantic-ia-complete-symbol
    (require 'semantic)
    (global-semanticdb-minor-mode 1)
    (global-semantic-idle-scheduler-mode 1)
    (semantic-mode 1)
    (add-hook 'c-mode-hook (lambda () (global-semantic-idle-summary-mode 1)))
    (add-hook 'c++-mode-hook (lambda () (global-semantic-idle-summary-mode 1)))
    (semantic-add-system-include "/usr/include/boost")
    (semantic-add-system-include "/usr/include/eigen3")
    (semantic-add-system-include "/usr/include/pcl-1.7/")
    ;; SpeedBar
    (use-package sr-speedbar
      ;; Show speedbar in the same frame
      :ensure t
      :if window-system ;; only use in GUI
      :demand
      :bind
      (("C-c C-s" . sr-speedbar-toggle)
       :map c-mode-base-map
	    ("C-c C-s" . sr-speedbar-toggle))
      :config
      (require 'sr-speedbar)
      (setq sr-speedbar-width 30)
      (setq sr-speedbar-right-side nil)
      ;;(sr-speedbar-open)
      (defun sb-expand-curren-file ()
	"Expand current file in speedbar buffer"  
	(interactive)
	(setq current-file (buffer-file-name))
	(sr-speedbar-refresh)
	(switch-to-buffer-other-frame "*SPEEDBAR*")
	(speedbar-find-selected-file current-file)
	(speedbar-expand-line))
      )
    (which-function-mode 1) ;; show which function the cursor is in
    )

;;;;;;;;;;;;;;;;; MELPA PACKAGES

  (use-package rainbow-delimiters
    ;; highlights delimiters such as parentheses, brackets or braces according to their depth.
    :ensure t
    :config
    (require 'rainbow-delimiters)
    (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

  ;; (use-package material-theme
  ;;   ;; Custom theme, only used in GUI mode (i.e. not for 'emacs -nw')
  ;;   :ensure t
  ;;   :if window-system ;; load only when GUI
  ;;   :config
  ;;   (load-theme 'material t)
  ;;   ;; stylize the mode line
  ;;   (set-face-attribute 'mode-line nil :box nil)
  ;;   (set-face-attribute 'mode-line-inactive nil :box nil))

  (use-package zenburn-theme
    ;; the best theme there is!
    :ensure t
    :if window-system ;; load only when GUI
    :config
    (load-theme 'zenburn t)
    ;; stylize the mode line
    (set-face-attribute 'mode-line nil :box nil)
    (set-face-attribute 'mode-line-inactive nil :box nil))

  (use-package dired+
    ;; advanced Dired functionality
    :ensure t
    :init
    ;; show details by default
    (setq diredp-hide-details-initially-flag nil)
    :config
    ;; load Dired in the same buffer
    (diredp-toggle-find-file-reuse-dir 1))

  (use-package dired
    ;; built-in directory browser
    :config
    ;; move files to trash
    (setq delete-by-moving-to-trash t))

  (use-package flycheck
    ;; code error highlighting
    ;; Linux Mint:
    ;;  sudo apt-get install shellcheck # bash script linter
    :ensure t
    :defer t
    :init
    (add-hook 'c-mode-common-hook 'flycheck-mode)
    (add-hook 'sh-mode-hook 'flycheck-mode)
    :config
    ;; Check buffer on save and immediately after opening buffer
    (setq flycheck-check-syntax-automatically '(mode-enabled save)))

  (use-package company
    ;; complete anything
    :ensure t
    :bind
    (("<S-SPC>" . company-complete)
     ("C-x C-n" . company-select-next)
     ("C-x C-p" . company-select-previous))
    :init
    ;;(add-hook 'after-init-hook 'global-company-mode)
    (add-hook 'c-mode-common-hook 'company-mode)
    (add-hook 'emacs-lisp-mode-hook 'company-mode)
    :config
    (setq company-async-timeout 10) ;; set timeout to 10 seconds
    ;;(setq company-backends (delete 'company-semantic company-backends))
    ;; use C++11 by default
    ;; (require 'cc-mode)
    ;; (set 'company-clang-arguments (list "-std=c++11"))
    )

  (use-package irony
    ;; asynchronous capabilities (improces C/C++ editing experience)
    ;; Linux Mint:
    ;;  sudo apt-get install g++ clang libclang-dev
    ;;  M-x irony-install-server # when in c-mode or c++-mode
    :ensure t
    :defer t
    :init
    (add-hook 'c-mode-common-hook 'irony-mode)
    :config
    ;; replace completion-at-point and complete-symbol by irony-mode's asynchronous functions
    (defun my-irony-mode-hook ()
      (define-key irony-mode-map [remap completion-at-point]
	'irony-completion-at-point-async)
      (define-key irony-mode-map [remap complete-symbol]
	'irony-completion-at-point-async))
    (add-hook 'irony-mode-hook 'my-irony-mode-hook)
    (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
    (setq irony-additional-clang-options '(;; use C++11
					   "-std=c++11"))
    ;; additional include paths
    ;;					 "-I/home/danylo/catkin_ws/devel/include/"
    ;;					 "-I/opt/ros/kinetic/include/"))
    ;; asynchronous code linting
    (use-package flycheck-irony
      :ensure t
      :config
      ;; Enable Irony for Flycheck
      (eval-after-load 'flycheck '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup)))
    ;; asynchronous completion
    (use-package company-irony
      :ensure t
      :init
      (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)
      (eval-after-load 'company '(add-to-list 'company-backends 'company-irony))
      ;; :config
      ;; (setq company-idle-delay 0.05)
      )
    ;; enable C/C++ header completion
    (use-package company-irony-c-headers
      :ensure t
      :config
      (eval-after-load 'company '(add-to-list 'company-backends '(company-irony-c-headers company-irony)))))

  (use-package helm
    ;; incremental completion and selection narrowing framework
    ;; Useful bindinds:
    ;;  C-t : cycle helm buffer position (up/down/left/right)
    ;;  C-SPC : select completion candidate
    ;;  M-a : select all completion candidates
    ;;  M-y : show the kill ring
    ;;  C-c h i : find major definitions (e.g. of functions and variables) in C/C++, Lisp and a ton of other languages
    ;;  C-M-a and C-M-e : jump to beginning/end of function definition
    ;;  C-c h m : open man pages for symbol at point or search
    ;;  C-] : toggle to show only file names in helm buffer
    ;;  C-c r : open a file with sudo permissions
    ;;  *<STUFF> : substring search major mode in helm-mini (i.e. after C-x b)
    :ensure t
    :demand
    :bind
    (("M-x" . helm-M-x)
     ("M-y" . helm-show-kill-ring)
     ("C-x b" . helm-mini)
     ("C-x C-f" . helm-find-files))
    :config
    (require 'helm)
    (require 'helm-config)
    (helm-mode 1)
    (add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)
    (setq helm-buffer-max-length 30) ;; nil to show full name always
    (setq helm-buffers-truncate-lines nil))

  (use-package helm-swoop
    ;; Helm version of C-s (I-search) and C-r (I-search backward)
    :ensure t
    :demand
    :bind
    (("M-i" . helm-swoop)
     ("M-I" . helm-swoop-back-to-last-point)
     ("C-c M-i" . helm-multi-swoop)
     ("C-x M-i" . helm-multi-swoop-all)
     :map isearch-mode-map
     ("M-i" . helm-swoop-from-isearch))
    :config
    (setq helm-swoop-use-line-number-face t)
    (setq helm-swoop-split-direction 'split-window-vertically)
    (setq helm-swoop-split-with-multiple-windows nil))

  (use-package helm-gtags
    ;; GNU GLOBAL helm interface
    ;; Useful keys:
    ;; C-c g a : helm-gtags-tags-in-this-function, finds functions that current function calls
    ;; M-.     : helm-gtags-dwim, execute actions based on context (where point currently is)
    ;; M-,     : tags-loop-continue, go back (after pressing M-.)
    ;; C-c g h : show chronological history of tags visited (instead of pressing M-, repeatedly)
    :ensure t
    :init
    (add-hook 'c-mode-common-hook 'helm-gtags-mode)
    :bind
    (:map c-mode-base-map
	  ("C-c g a" . helm-gtags-tags-in-this-function)
	  ("C-c g h" . helm-gtags-show-stack)
	  ("C-j" . helm-gtags-select)
	  ("M-." . helm-gtags-dwim)
	  ("M-," . helm-gtags-pop-stack)
	  ("C-c o" . helm-gtags-find-tag-other-window)
	  ("C-c <" . helm-gtags-previous-history)
	  ("C-c >" . helm-gtags-next-history))
    :config
    (setq helm-gtags-ignore-case t)
    (setq helm-gtags-auto-update t)
    (setq helm-gtags-use-input-at-cursor t)
    (setq helm-gtags-pulse-at-cursor t)
    (setq helm-gtags-prefix-key "\C-cg")
    (setq helm-gtags-suggested-key-mapping t))

  (if (getenv "ROS_ROOT")
      (progn
	(message "Loading helm-ros")
	(use-package helm-ros
	  ;; An Emacs package that interfaces ROS with the helm completion facilities.
	  ;; Keybindings:
	  ;;   C-x C-r h : Starts helm-ros with all available sources.
	  ;;   C-x C-r m : Start a roscore.
	  ;;   C-x C-r i : Invalidate helm-ros cache (if you create a new file).
	  
	  ;; In ros-process buffers, you can use the following keybindings:
	  ;;   c : Interrupts the ROS process associated with the buffer.
	  ;;   k : Kills the ROS process associated with the buffer.
	  ;;   q : Closes the buffer.
	  :ensure t
	  :config
	  (require 'helm-ros)
	  (global-helm-ros-mode t)
	  ))
    (message "ROS environment variables not defined, won't load helm-ros"))

  (use-package projectile
    ;; project interaction library offering tools to operate on a project level
    ;; Useful commands:
    ;;  C-c p p   : the first thing to run when starting to work in a project
    ;;  C-c p f   : find file in current project
    ;;  C-c p g   : find file based on context (can place cursor on header file, executing command will take you to header file)
    ;;  C-c p d   : find directory in current project
    ;;  C-c p a   : switch between files with same name but different extensions (e.g. .cpp and .h)
    ;;  C-c p i   : rebuild cache (otherwise if new files made, old cache causes them to not be seen)
    ;;  C-c p f, C-SPC, C-f: create a virtual Dired buffer from files selected with C-SPC with C-c p f
    ;;  C-c p b   : switch to open buffer of file/directory that belongs to current project
    ;;  C-c p s g : find a word/string in project files  (using grep), autofill with symbol at point
    :ensure t
    :config
    (setq projectile-enable-caching nil)
    (setq projectile-globally-ignored-directories (append '(
							    ".svn"
							    ".git"
							    )
							  projectile-globally-ignored-directories))
    (setq projectile-globally-ignored-files (append '(
						      ".DS_Store"
						      ".gitignore"
						      )
						    projectile-globally-ignored-files))
    (projectile-global-mode)
    (use-package helm-projectile
      ;; Helm UI for Projectile
      :ensure t
      :config
      (setq projectile-completion-system 'helm)
      (helm-projectile-on)
      (setq projectile-switch-project-action 'helm-projectile)))

  (use-package tex
    ;; AUCTeX
    :ensure auctex
    :init
    (setq TeX-source-correlate-method 'synctex) ;; use SyncTeX for forward/backward search between source/PDF output
    :config
    ;;(add-hook 'LaTeX-mode-hook 'flyspell-mode)
    ;;(add-hook 'LaTeX-mode-hook 'flyspell-buffer)
    (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
    (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
    (add-hook 'LaTeX-mode-hook 'visual-line-mode)
    (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
    (add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)
    (setq TeX-auto-save t)
    (setq TeX-parse-self t)
    (setq TeX-save-query nil)
    (setq-default TeX-master nil)
    (setq ispell-program-name "aspell") ; could be ispell as well, depending on your preferences
    (setq ispell-dictionary "english") ; this can obviously be set to any language your spell-checking program supports
    (setq reftex-plug-into-AUCTeX t)
    (add-hook 'LaTeX-mode-hook
	      '(lambda ()
		 (define-key LaTeX-mode-map (kbd "$") 'self-insert-command))))

  (use-package pdf-tools
    ;; advanced PDF viewing capbilities inside Emacs (DocView deplacement)
    :ensure t
    :config
    (pdf-tools-install)
    (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
    (setq TeX-view-program-selection '((output-pdf "PDF Tools"))))

  (use-package mic-paren
    ;; highlight matching parentheses
    :ensure t
    :config
    (electric-pair-mode 1) ;; auto-create matching closing bracket
    (paren-activate))

  (use-package yaml-mode
    ;; for some work with ROS
    :ensure t
    :defer t
    :init
    (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
    (add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode)))

  (use-package google-c-style
    ;; provides the Google C/C++ coding style
    :ensure t
    :config
    (add-hook 'c-mode-common-hook 'google-set-c-style))

  (use-package flycheck-google-cpplint
    ;; Add Google C++ Style checker for Flycheck
    :disabled t ;; don't use google cpplint for now... annoying warnings
    :ensure t
    :demand
    :config
    (require 'flycheck-google-cpplint)
    (require 'flycheck-irony)
    (with-eval-after-load 'flycheck
      ;; Chain javascript-jscs to run after javascript-jshint.
      (flycheck-add-next-checker 'irony '(t . c/c++-googlelint))))

  (if (display-graphic-p)
      ;; use nyan mode only if Emacs run in GUI mode (and not 'emacs -nw')
      (use-package nyan-mode
	;; adorable cat showing progress in document
	:ensure t
	:config
	(nyan-mode 1)
	(setq nyan-wavy-trail nil)
	(setq nyan-animate-nyancat t)))

  (use-package srefactor
    ;; C/C++ refactoring tool based on Semantic parser framework
    :ensure t
    :demand
    :bind
    (("M-RET o" . srefactor-lisp-one-line)
     ("M-RET m" . srefactor-lisp-format-sexp)
     ("M-RET d" . srefactor-lisp-format-defun)
     ("M-RET b" . srefactor-lisp-format-buffer)
     :map c-mode-map
     ("M-RET" . srefactor-refactor-at-point)
     :map c++-mode-map
     ("M-RET" . srefactor-refactor-at-point))
    :config
    (require 'semantic)
    (require 'srefactor)
    (require 'srefactor-lisp)
    (setq srefactor-ui-menu-show-help nil) ;; hide menu help message
    (semantic-mode 1))

  (use-package multiple-cursors
    ;; multiple cursors for emacs (live editing of same word at multiple points)
    ;; use case: edit grep buffer using wgrep for refactoring
    :ensure t
    :demand
    :bind
    (("C->" . mc/mark-next-like-this)
     ("C-<" . mc/mark-previous-like-this)
     ("C-*" . mc/mark-all-like-this))
    :config
    (require 'multiple-cursors))

  (use-package wgrep
    ;; edit a grep buffer and apply those changes to the file buffer
    ;; use-case: local/global rename variables, functions, classes, namespaces, etc.
    ;; workflow:
    ;;    1) Way 1: Do grep within project using grep-projectile "C-c p s g" then "C-x C-s" to save
    ;;              helm-grep results to grep buffer
    ;;       Way 2: Do grep on certain files of your choice using M-x rgrep
    ;;    2) In the grep buffer, active wgrep with "C-c C-p"
    ;;    3) Rename whatever you want by editing the names (e.g. use replace-string or multiple-curscors (see above))
    ;;    4) "C-c C-e" to save changes or "C-c C-k" to discard changes or "C-x C-q" to exit wgrep
    ;;       and prompt whether to save changes
    :ensure t
    :config
    (use-package wgrep-helm
      :ensure t
      :config
      (require 'wgrep)
      (require 'wgrep-helm)
      (setq wgrep-auto-save-buffer t)))

  (use-package realgud
    ;; A extensible, modular GNU Emacs front-end for interacting with external debuggers
    :ensure t)

  (use-package org
    ;; Org mode is for keeping notes, maintaining TODO lists, planning projects, and authoring documents with a fast and effective plain-text system.
    :ensure t
    :config
    (require 'org)
    (add-hook 'org-mode-hook 'turn-on-auto-fill)
    (define-key global-map "\C-cl" 'org-store-link)
    (define-key global-map "\C-ca" 'org-agenda)
    (setq org-log-done t)
    (setq org-agenda-files (list "~/Dropbox/shared_files/org/work.org"
				 "~/Dropbox/shared_files/org/home.org")))

  (use-package ecb
    ;; Emacs Code Browser
    :ensure t
    :config
    (require 'ecb)
    ;;(setq ecb-auto-activate t) ;; auto-activate ECB at startup
    (setq ecb-layout-name "left11") ;; set ECB layout
    (setq ecb-tip-of-the-day nil) ;; turn off ECB tip of the day message
    ;; also see custom-set-variables and custom-set-faces below
    )

  (use-package rainbow-mode
    ;; A minor mode for Emacs which displays strings representing colors with the color they represent as background
    :ensure t)

  (use-package auto-compile
    ;; Provides two minor modes which automatically recompile Emacs Lisp source files. Together these modes guarantee that Emacs never loads outdated byte code files.
    :ensure t
    :config
    ;;; init.el --- user init file      -*- no-byte-compile: t -*-
    (setq load-prefer-newer t)
    (package-initialize)
    (require 'auto-compile)
    (auto-compile-on-load-mode)
    (auto-compile-on-save-mode)
    )

  ;; (use-package smart-mode-line
  ;;   ;; A sexy mode-line for Emacs. It aims to be easy to read from small to large monitors by using colors, a prefix feature, and smart truncation.
  ;;   :ensure t
  ;;   :config
  ;;   (setq sml/no-confirm-load-theme t)
  ;;   ;;(setq sml/theme 'dark)
  ;;   (sml/setup))

  (use-package ess
    ;; ESS (Emacs Speaks Statistics)
    ;; for R language (and others...)
    ;; Useful keyboard commands:
    ;;   M-x R : start the R process in an (inferior) buffer
    ;;   TAB : completion at point
    ;;   C-c C-v <NAME> : get documentation about NAME
    :ensure t
    :config
    (require 'ess-site)
    (add-to-list 'auto-mode-alist '("\\.R$" . R-mode))
    (add-to-list 'auto-mode-alist '("\\.r$" . R-mode))
    (use-package ess-R-data-view
      :ensure t
      )
    )

;;;;;;;;;;;;;;;;; PERSONAL PACKAGES

  (use-package c-block-comment
    ;; automatically type C-style block comments
    ;; Use M-; to insert /* */ around the point
    ;; Use M-j to go to next line in a multi-line block comment
    :load-path "lisp/")

  (use-package setup-helm
    ;; https://github.com/tuhdo/emacs-c-ide-demo/blob/master/custom/setup-helm.el
    :load-path "lisp/")

  (use-package buffer-move
    ;; swap buffers between windows
    :load-path "lisp/"
    :bind
    (("C-S-<up>" . buf-move-up)
     ("C-S-<down>" . buf-move-down)
     ("C-S-<left>" . buf-move-left)
     ("C-S-<right>" . buf-move-right)))

  (use-package my-cursor-commands
    ;; paragraph scrolling key bindings
    :load-path "lisp/"
    :bind
    (("M-p" . xah-backward-block)
     ("M-n" . xah-forward-block)))

  (use-package delete-without-copy
    ;;  delete words without putting them into the kill ring
    :load-path "lisp/"
    :bind
    (("M-d" . my-forward-delete-word)
     ("M-<backspace>" . my-backward-delete-word)))

  (use-package sr-speedbar
    ;; makes Speedbar (source file browser) show in the current frame as a new window
    :load-path "lisp/"
    :bind
    ("C-c C-b" . sr-speedbar-toggle))

  (use-package duplicate-line
    ;; duplicate current line
    :load-path "lisp/"
    :bind
    (("C-c d" . duplicate-current-line-or-region)))

  (use-package cmake-mode
    ;; major-mode for editing CMake sources
    :load-path "lisp/"
    :config
    (add-to-list 'auto-mode-alist '("\\.cmake\\'" . cmake-mode))
    (autoload 'cmake-mode "lisp/cmake-mode.el" t))

  (use-package ros-cmake-mode
    ;; major-mode for editing CMakeLists.txt files with ROS-style indentation
    :load-path "lisp/"
    :config
    (add-to-list 'auto-mode-alist '("CMakeLists\\.txt\\'" . ros-cmake-mode))
    (autoload 'ros-cmake-mode "lisp/ros-cmake-mode.el" t))

;;;;;;;;;;;;;;;;; OTHER STUFF

  ;; default font and font size
  (add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-10"))

  ;; rename buffer shortcut
  (global-set-key (kbd "C-c r") 'rename-buffer)

  ;; winner-mode, which lets you go back (C-c <left>) and forward (C-c <right>) in window layout history
  ;; NB: NOT COMPATIBLE WITH ECB :'(
  (when (fboundp 'winner-mode)
    (winner-mode 1))

  ;; backup behaviour: store everything in single location
  (setq backup-directory-alist '(("." . "~/.emacs.d/backups/")))
  ;;(setq backup-inhibited t) ;; disable backup
  (setq backup-by-copying t) ;; make sure Emacs doesn't break hard links
  ;;(setq make-backup-files nil) ;; stop creating backup ~ files

  ;; do not truncate windows that are too narrow
  (setq truncate-partial-width-windows nil)

  ;; show tooltips in echo area
  (tooltip-mode -1)
  (setq tooltip-use-echo-area t)
  (global-set-key (kbd "C-x t") 'display-local-help) ;; show tooltip

  (setq bookmark-save-flag 1) ; everytime bookmark is changed, automatically save it

  (setq gdb-many-windows t) ;; run GDB with many windows view by default

  (setq gdb-non-stop-setting nil) ;; run GDB in all-stop mode by default (i.e. all threads stopped at breakpoint)

  (setq inhibit-splash-screen t)

  (setq inhibit-startup-message t)

  (setq initial-scratch-message ";; Change The World")

  (setq ring-bell-function 'ignore) ;; Disable system sounds

  (global-set-key (kbd "C-x C-;") 'comment-region) ;; Comment region in Lisp

  (add-to-list 'auto-mode-alist '("\\.launch?\\'" . xml-mode)) ;; xml-mode for .launch files (xml)

  (define-key global-map (kbd "RET") 'newline-and-indent)

  (put 'downcase-region 'disabled nil)

  ;; remove scrollbar any new frames
  (defun my/disable-scroll-bars (frame)
    (modify-frame-parameters frame
			     '((vertical-scroll-bars . nil)
			       (horizontal-scroll-bars . nil))))
  (add-hook 'after-make-frame-functions 'my/disable-scroll-bars)

  ;; scroll one line at a time (less "jumpy" than defaults)
  ;; (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
  ;; (setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
  ;; (setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
  ;; (setq scroll-step 1) ;; keyboard scroll one line at a time

  ;; Save/load window configurations
  (global-set-key (kbd "<f10>") '(lambda () (interactive) (jump-to-register 9)
				   (message "Windows disposition loaded"))) ;; load window config
  (global-set-key (kbd "<f9>") '(lambda () (interactive) (window-configuration-to-register 9)
				  (message "Windows disposition saved"))) ;; save window config

;;;;;; Improving ansi-term
  ;; make that C-c t e launches an ansi-term buffer in the current window
  (global-set-key (kbd "C-c t e") 'ansi-term)
  ;; avoid ansi-term asking always which shell to run (always run bash)
  (defvar my-term-shell "/bin/bash")
  (defadvice ansi-term (before force-bash)
    (interactive (list my-term-shell)))
  (ad-activate 'ansi-term)
  ;; display of certain characters and control codes to UTF-8
  (defun my-term-use-utf8 ()
    (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))
  (add-hook 'term-exec-hook 'my-term-use-utf8)
  ;; clickable URLs
  (defun my-term-hook ()
    (goto-address-mode))
  (add-hook 'term-mode-hook 'my-term-hook)
  ;; make that typing exit in ansi-term (which exits the shell) also
  ;; closes the buffer
  (defadvice term-sentinel (around my-advice-term-sentinel (proc msg))
    (if (memq (process-status proc) '(signal exit))
	(let ((buffer (process-buffer proc)))
	  ad-do-it
	  (kill-buffer buffer))
      ad-do-it))
  (ad-activate 'term-sentinel)
  ;; fix the problem of moving cursor left/right being captured by emacs
  ;; instead of underlying terminal, leading to jumbling when jumping
  ;; words then editing the middle of a command. Same for deleting
  ;; backward/forward whole words.
  (eval-after-load "term"
    '(progn
       ;; rename buffer
       (define-key term-raw-map (kbd "C-c r") 'rename-buffer)

       ;; unbind C-z, which minimizes current frame
       (define-key term-raw-map (kbd "C-z") 'nil)
       
       ;; make sure typical key combos work in term-char-mode
       (define-key term-raw-map (kbd "M-x") 'nil)
       (define-key term-raw-map (kbd "M-&") 'nil)
       (define-key term-raw-map (kbd "M-!") 'nil)

       ;; make sure C-c t e launches a new ansi-term buffer when current
       ;; buffer is also ansi-term
       (define-key term-raw-map (kbd "C-c t e") 'nil)
       
       ;; move by whole words fix
       (defun term-send-Cright () (interactive) (term-send-raw-string "\e[1;5C"))
       (defun term-send-Cleft  () (interactive) (term-send-raw-string "\e[1;5D"))
       (defun term-send-Mbackspace () (interactive)(term-send-raw-string "\e\d"))

       ;; word deletion fix
       (define-key term-raw-map (kbd "C-w") 'term-send-Mbackspace)
       (define-key term-raw-map (kbd "M-<backspace>") 'term-send-Mbackspace)     
       
       ;; switch between char and line mode with logical keystrokes make
       ;; sure that in line mode, the buffer is auto-set to read-only
       ;; such that I don't accidentally edit the terminal in
       ;; term-line-mode, thus messing up the command in term-char-mode
       ;; (the mode in which I normally work, with term-line-mode used
       ;; for scrolling up/down the buffer)
       (defun my-switch-to-term-line-mode ()
	 (interactive)
	 (term-line-mode)
	 (if (not buffer-read-only)
	     (toggle-read-only)))
       (defun my-switch-to-term-char-mode ()
	 (interactive)
	 (term-char-mode)
	 (if buffer-read-only
	     (toggle-read-only)))
       (add-hook 'term-mode-hook (lambda () (local-set-key (kbd "C-c t c") 'my-switch-to-term-char-mode)))
       (define-key term-raw-map (kbd "C-c t l") 'my-switch-to-term-line-mode)

       ;; copy/paste native Emacs keystrokes
       (define-key term-raw-map (kbd "C-k") 'term-send-raw)
       (define-key term-raw-map (kbd "C-y") 'term-paste)

       ;; ensure that scrolling doesn't break on output
       ;;(setq term-scroll-show-maximum-output t)
       (setq term-scroll-to-bottom-on-output t)

       ;; max history (# lines) to keep (0 == keep everything)
       (setq term-buffer-maximum-size 50000)))
  ;; make sure window movement keys are not captured by terminal
  (add-hook 'term-mode-hook
	    (lambda ()
	      (define-key term-raw-map (kbd "C-<up>") 'nil)
	      (define-key term-raw-map (kbd "C-<down>") 'nil)
	      (define-key term-raw-map (kbd "C-<left>") 'nil)
	      (define-key term-raw-map (kbd "C-<right>") 'nil)))

  ;; fix Semantic package issue of uncompressing/parsing tons of .eg.gz
  ;; files when editing certain buffers (primarily Emacs-Lisp, but also
  ;; the GDB buffer, for instance)
  (eval-after-load 'semantic
    (add-hook 'semantic-mode-hook
	      (lambda ()
		(dolist (x (default-value 'completion-at-point-functions))
		  (when (string-prefix-p "semantic-" (symbol-name x))
		    (remove-hook 'completion-at-point-functions x))))))

  ;; remove a significant contributor to line scan slowness
  (setq bidi-display-reordering nil)

  ;; auto-fill comments in programming modes
  ;; if you want to have automatic auto-fill:
  ;; (defun comment-auto-fill ()
  ;;       (setq-local comment-auto-fill-only-comments t)
  ;;       (auto-fill-mode 1))
  ;; (add-hook 'c-mode-common-hook (lambda () (comment-auto-fill)))
  ;; if you want to manually auto-fill (M-q), but for that to only apply to comments
  (add-hook 'c-mode-common-hook (lambda () (setq-local comment-auto-fill-only-comments t)))

  ;; Set custom variables
  (custom-set-variables
   ;; custom-set-variables was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(Man-notify-method (quote pushy))
   '(ecb-auto-expand-tag-tree (quote expand-spec))
   '(ecb-auto-expand-tag-tree-collapse-other nil)
   '(ecb-highlight-tag-with-point (quote highlight-scroll))
   '(ecb-highlight-tag-with-point-delay 0.25)
   '(ecb-layout-window-sizes
     (quote
      (("left11"
	(ecb-methods-buffer-name 0.17901234567901234 . 0.7)
	(ecb-history-buffer-name 0.17901234567901234 . 0.275)))))
   '(ecb-options-version "2.50")
   ;; ansi-term stuff
   ;; from https://snarfed.org/why_i_dont_run_shells_inside_emacs
   ;; '(comint-scroll-to-bottom-on-input t)  ;; always insert at the bottom
   ;; '(comint-scroll-to-bottom-on-output t) ;; always add output at the bottom
   ;; '(comint-scroll-show-maximum-output t) ;; scroll to show max possible output
   ;; '(comint-completion-autolist t)        ;; show completion list when ambiguous
   ;; '(comint-input-ignoredups t)           ;; no duplicates in command history
   )

  ;; set some ECB faces
  ;; see http://ecb.sourceforge.net/docs/ecb_002dfaces.html for descriptions
  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(ecb-default-highlight-face ((((class color) (background dark)) (:background "yellow" :foreground "black"))))
   '(ecb-tag-header-face ((((class color) (background dark)) (:background "yellow" :foreground "black")))))

  ;; /*  */ style comments with C-x M-; in c++-mode
  (defun my-block-comment ()
    (interactive)
    (let ((comment-start "/* ")
	  (comment-end " */"))
      (comment-dwim nil)))
  (add-hook 'c++-mode-hook (lambda () (local-set-key (kbd "C-x M-;") 'my-block-comment)))

  ;; Color roslaunch files correctly
  (add-to-list 'auto-mode-alist '("\\.launch$" . xml-mode))

  ;; ansi-term bi-directional text support problem fix, which seems to
  ;; be the cause of text jumbling when going back commands in
  ;; ansi-term. This fixes it, yay!
  (add-hook 'term-mode-hook 'my-term-mode-hook)
  (defun my-term-mode-hook ()
    ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=20611
    (setq bidi-paragraph-direction 'left-to-right))

  (eval-after-load 'term
    '(progn
       (defun my-term-send-delete-word-forward () (interactive) (term-send-raw-string "\ed"))
       (defun my-term-send-delete-word-backward () (interactive) (term-send-raw-string "\e\C-h"))
       (define-key term-raw-map [C-delete] 'my-term-send-delete-word-forward)
       (define-key term-raw-map [C-backspace] 'my-term-send-delete-word-backward)
       (defun my-term-send-forward-word () (interactive) (term-send-raw-string "\ef"))
       (defun my-term-send-backward-word () (interactive) (term-send-raw-string "\eb"))
       (define-key term-raw-map [C-left] 'my-term-send-backward-word)
       (define-key term-raw-map [C-right] 'my-term-send-forward-word)
       (defun my-term-send-m-right () (interactive) (term-send-raw-string "\e[1;3C"))
       (defun my-term-send-m-left () (interactive) (term-send-raw-string "\e[1;3D"))
       (define-key term-raw-map [M-right] 'my-term-send-m-right)
       (define-key term-raw-map [M-left] 'my-term-send-m-left)
       ))

  ;; byte-compilation for performance boost
  (defun byte-compile-init-dir ()
    "Byte-compile all your dotfiles."
    (interactive)
    (byte-recompile-file "~/.emacs.d/init.el" nil 0) ;; byte-compile init file
    (byte-recompile-directory "~/.emacs.d/lisp" 0) ;; byte-compile personal custom lisp code
    )

  ;; Open ROS .msg, .srv and .action files in gdb-script-mode for syntax highlighting
  (add-to-list 'auto-mode-alist '("\\.msg\\'" . gdb-script-mode))
  (add-to-list 'auto-mode-alist '("\\.srv\\'" . gdb-script-mode))
  (add-to-list 'auto-mode-alist '("\\.action\\'" . gdb-script-mode))

  ;; Enable C++ mode by default for .h header files
  (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

  ;; Highlight matching parentheses
  (require 'paren)
  (show-paren-mode 1)
  (setq show-paren-delay 0)
)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Man-notify-method (quote pushy))
 '(ecb-auto-expand-tag-tree (quote expand-spec))
 '(ecb-auto-expand-tag-tree-collapse-other nil)
 '(ecb-highlight-tag-with-point (quote highlight-scroll))
 '(ecb-highlight-tag-with-point-delay 0.25)
 '(ecb-layout-window-sizes
   (quote
    (("left11"
      (ecb-methods-buffer-name 0.17901234567901234 . 0.7)
      (ecb-history-buffer-name 0.17901234567901234 . 0.275)))))
 '(ecb-options-version "2.50")
 '(package-selected-packages
   (quote
    (ess-R-data-view zenburn-theme yaml-mode wgrep-helm use-package srefactor sr-speedbar realgud rainbow-mode rainbow-delimiters pdf-tools nyan-mode multiple-cursors mic-paren helm-swoop helm-ros helm-projectile helm-gtags google-c-style flycheck-irony ess ecb dired+ company-irony-c-headers company-irony auto-compile auctex))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ecb-default-highlight-face ((((class color) (background dark)) (:background "yellow" :foreground "black"))))
 '(ecb-tag-header-face ((((class color) (background dark)) (:background "yellow" :foreground "black")))))
