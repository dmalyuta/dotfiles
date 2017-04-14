(let ((gc-cons-threshold most-positive-fixnum) ;; better garbage collection
      (file-name-handler-alist nil))

;;;;;;;;;;;;;;;;; STUFF THAT NEEDS TO COME FIRST
               ;; (like mode line, so that we get the customization right away)
  
  ;; sensible GUI
  (menu-bar-mode -1)  ;; disable menubar
  (tool-bar-mode -1) ;; disable toolbar
  (toggle-scroll-bar -1) ;; disable scrollbar

  ;; Comint unset C-up C-down
  (global-unset-key (kbd "C-<up>"))
  (global-unset-key (kbd "C-<down>"))

;;;;;;;;;;;;;;;;; PACKAGE MANAGEMENT

  ;; MELPA
  (require 'package)
  (setq package-enable-at-startup nil)
  (setq package-check-signature nil)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
  (add-to-list 'package-archives '("elpy" . "https://jorgenschaefer.github.io/packages/"))
  (package-initialize)

  ;; boostrap 'use-package'
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))

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

;;;;;;;;;;;;;;;;; MELPA PACKAGES

  (use-package rainbow-delimiters
    ;; highlights delimiters such as parentheses, brackets or braces according to their depth.
    :ensure t
    :config
    (require 'rainbow-delimiters)
    (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

  (use-package zenburn-theme
    ;; the best theme there is!
    :ensure t
    :defer t
    ;;:config
    ;;(load-theme 'zenburn t t) ;; last t is for NO-ENABLE
    )

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
    (add-hook 'python-mode-hook 'flycheck-mode)
    :config
    ;; Check buffer on save and immediately after opening buffer
    (setq flycheck-check-syntax-automatically '(mode-enabled save)))
  
  (use-package company
    ;; complete anything
    :ensure t
    :bind
    (("C-:" . company-complete) ;; recommended to use TAB instead (which uses helm-company, see below)
     ("M-n" . company-select-next)
     ("M-p" . company-select-previous))
    :init
    ;;(add-hook 'after-init-hook 'global-company-mode)
    (add-hook 'c-mode-common-hook 'company-mode)
    (add-hook 'emacs-lisp-mode-hook 'company-mode)
    (add-hook 'comint-mode-hook 'company-mode)
    ;;(add-hook 'sh-mode-hook 'company-mode)
    ;;(add-hook 'python-mode-hook 'company-mode)
    :config
    ;; set timeout to 10 seconds
    (setq company-async-timeout 10)
    ;; Cancel selections by typing non-mathcing characters
    (setq company-require-match 'never)
    ;; Minimum length of word to start completion
    (setq company-minimum-prefix-length 10)
    ;; Autocomplete only when I explicitly mean to
    (setq company-auto-complete nil)
    (set 'company-idle-delay 1)
    (setq company-auto-select-first-candidate nil)
    )

  (use-package company-quickhelp
    ;; Documentation popups when idling on a completion candidate
    :ensure t
    :config
    (company-quickhelp-mode 1)
    (setq company-quickhelp-delay 0.5)
    (eval-after-load 'company
      '(define-key company-active-map (kbd "M-h") #'company-quickhelp-manual-begin))
    )

  (use-package helm-company
    ;; Helm interface for company-mode
    :ensure t
    :bind
    (("C-;" . helm-company))
    :demand
    :config
    (eval-after-load 'company
      '(progn
	 (defun indent-or-complete ()
	   (interactive)
	   (if (looking-at "\\_>")
	       (helm-company)
	     (indent-according-to-mode)))
	 ))
    )

  (use-package company-shell
    ;; company mode completion backends for your shell functions:
    :ensure t
    :config
    (add-to-list 'company-backends 'company-shell)
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
      ;;(add-hook 'irony-mode-hook 'company-irony-setup-begin-commands) ;; trigger completion after typing stuff like ->, ., etc
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

  ;; (use-package flycheck-google-cpplint
  ;;   ;; Add Google C++ Style checker for Flycheck
  ;;   :disabled t ;; don't use google cpplint for now... annoying warnings
  ;;   :ensure t
  ;;   :demand
  ;;   :config
  ;;   (require 'flycheck-google-cpplint)
  ;;   (require 'flycheck-irony)
  ;;   (with-eval-after-load 'flycheck
  ;;     ;; Chain javascript-jscs to run after javascript-jshint.
  ;;     (flycheck-add-next-checker 'irony '(t . c/c++-googlelint))))

  (use-package nyan-mode
    ;; adorable cat showing progress in document
    :ensure t
    :config
    (defun my-nyan-mode-activation (&optional frame)
      "Activate nyan-mode when emacs is a GUI"
      (interactive)
      (if (not (equal frame nil))
	  (select-frame frame))
      (if (display-graphic-p)
	  (progn
	    (nyan-mode 1)
	    (setq nyan-wavy-trail nil)
	    (setq nyan-animate-nyancat t)
	    )
	(nyan-mode 0))
      )
    (add-hook 'after-make-frame-functions 'my-nyan-mode-activation)
    (add-hook 'after-init-hook 'my-nyan-mode-activation)
    )

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

  (use-package jedi
    ;; Python autocompletion
    ;; Must run once M-x jedi:install-server
    :ensure t
    :config
    (load-file "~/.emacs.d/lisp/jedi-configuration.el")
    (require 'jedi-configuration)
    )

  (use-package fuzzy
    :ensure t)

  (use-package popup
    :ensure t)

  (use-package workgroups2
    ;; Restore layout. I use it only for keeping a persistent layout
    ;; for an emacs --daemon server
    :ensure t
    :config
    (if (daemonp)
	(workgroups-mode 1) ;; turn on workgroups mode when running as daemon
      )
    (setq wg-morph-on nil) ;; No silly workgroup switching animation

    ;; Start function
    (defun my-start-emacs ();;(frame)
      "Switch client frames of an emacs daemon to the 'server' workgroup."
      (interactive)
      ;;(select-frame frame)
      (if (daemonp)
	  (progn
	    (if (not (boundp 'server-wg))
		(progn
		  (wg-create-workgroup "server")
		  (setq server-wg (wg-current-workgroup))
		  )
	      (progn
		(setq current-wg (condition-case nil
				     (wg-current-workgroup)
				   (error nil)))
		(if (not current-wg)
		    (wg-switch-to-workgroup server-wg)
		  (if (not (eq current-wg server-wg))
		      (wg-switch-to-workgroup server-wg)
		    )
		  )
		)
	      )
	    )
	)
      )
    ;;(add-to-list 'after-make-frame-functions #'my-start-emacs)
    
    ;; Exit function
    (defun my-exit-emacs ()
      "Leaves session and saves all workgroup states. Will not continue if there is no workgroups open."
      (interactive)
      (if (daemonp)
	  (progn
	    ;;(wg-update-all-workgroups)
	    (save-buffers-kill-terminal)
	    )
	(save-buffers-kill-terminal)
	))
    (global-set-key (kbd "C-x C-c") 'my-exit-emacs)
    )

  ;; Bash scripting
  (add-hook
   'sh-mode-hook
   '(lambda ()
      (require 'popup)
      ;;(require 'fuzzy)
      (require 'auto-complete)
      (require 'auto-complete-config)
      ;;(ac-config-default)
      (global-auto-complete-mode nil)
      (add-hook 'sh-mode-hook 'auto-complete-mode)  
      (local-set-key (kbd "S-<SPC>") 'auto-complete)
      (setq ac-auto-show-menu t)
      (setq ac-dwim t)
      (setq ac-use-menu-map t)
      (setq ac-quick-help-delay 1)
      (setq ac-quick-help-height 60)
      (setq ac-disable-inline t)
      (setq ac-show-menu-immediately-on-auto-complete t)
      (setq ac-auto-start 0)
      (setq ac-candidate-menu-min 0)
      ))
  (use-package bash-completion
    :ensure t
    :config
    (autoload 'bash-completion-dynamic-complete 
      "bash-completion"
      "BASH completion hook")
    (add-hook 'shell-dynamic-complete-functions
	      'bash-completion-dynamic-complete)

    (defun ac-bash-candidates ()
      "This function is a modifed version of
bash-completion-dynamic-complete from bash-completion.el"
      (when bash-completion-enabled
	(let* ( (start (comint-line-beginning-position))
		(pos (point))
		(tokens (bash-completion-tokenize start pos))
		(open-quote (bash-completion-tokenize-open-quote tokens))
		(parsed (bash-completion-process-tokens tokens pos))
		(line (cdr (assq 'line parsed)))
		(point (cdr (assq 'point parsed)))
		(cword (cdr (assq 'cword parsed)))
		(words (cdr (assq 'words parsed)))
		(stub (nth cword words))
		(completions (bash-completion-comm line point words cword open-quote))
		;; Override configuration for comint-dynamic-simple-complete.
		;; Bash adds a space suffix automatically.
		(comint-completion-addsuffix nil) )
	  (if completions
	      completions))))
    
    (setq ac-source-bash
	  '((candidates . ac-bash-candidates)))
    
    (add-hook 'shell-mode-hook
	      (lambda()
		(setq ac-sources '(ac-source-bash))
		(auto-complete-mode)))
    )

  (use-package markdown-mode
    :ensure t
    :commands (markdown-mode gfm-mode)
    :mode (("README\\.md\\'" . gfm-mode)
	   ("\\.md\\'" . markdown-mode)
	   ("\\.markdown\\'" . markdown-mode))
    :init (setq markdown-command "multimarkdown"))

  (use-package flymd
    :ensure t
    :config
    (require 'flymd)
    (defun my-flymd-browser-function (url)
      (let ((browse-url-browser-function 'browse-url-firefox))
	(browse-url url)))
    (setq flymd-browser-open-function 'my-flymd-browser-function)
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
    (("S-M-<up>" . buf-move-up)
     ("S-M-<down>" . buf-move-down)
     ("S-M-<left>" . buf-move-left)
     ("S-M-<right>" . buf-move-right)))

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

  (use-package gdb-setup
    ;; customize GDB layout and functionality
    :load-path "lisp/")

  (use-package window-resize
    :load-path "lisp/"
    :demand
    :bind
    (
     ("S-C-<down>" . win-resize-minimize-vert)
     ("S-C-<up>" . win-resize-enlarge-vert)
     ("S-C-<left>" . win-resize-minimize-horiz)
     ("S-C-<right>" . win-resize-enlarge-horiz)
     ("S-C-<up>" . win-resize-enlarge-horiz)
     ("S-C-<down>" . win-resize-minimize-horiz)
     ("S-C-<left>" . win-resize-enlarge-vert)
     ("S-C-<right>" . win-resize-minimize-vert)
     ))

  (use-package theme-setup
    :load-path "lisp/"
    :demand
    :config
    ;; For when started with emacs or emacs -nw rather than emacs --daemon
    (add-hook 'after-init-hook 'my-normal-startup-pick-color-theme)
    ;; For when started as emacs --daemon
    (add-hook 'after-make-frame-functions 'pick-color-theme)
    )

;;;;;;;;;;;;;;;;; OTHER STUFF
  
  ;; enable clipboard in emacs
  (xterm-mouse-mode t)
  (mouse-wheel-mode t)
  (setq x-select-enable-clipboard t)

  ;; default font and font size
  (add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-10"))

  ;; rename buffer shortcut
  (global-set-key (kbd "C-c r") 'rename-buffer)

  ;; winner-mode, which lets you go back (C-c <left>) and forward (C-c <right>) in window layout history
  ;; NB: NOT COMPATIBLE WITH ECB :'(
  (when (fboundp 'winner-mode)
    (winner-mode 1))

  ;;(setq backup-inhibited t) ;; disable backup
  ;;(setq make-backup-files nil) ;; stop creating backup ~ files
  (setq backup-by-copying t) ;; make sure Emacs doesn't break hard links
  ;; backup behaviour: store everything in single location
  (defvar backup-dir "~/.emacs.d/backups/")
  (setq backup-directory-alist (list (cons "." backup-dir)))

  ;; do not truncate windows that are too narrow
  (setq truncate-partial-width-windows nil)

  ;; require file ending with a newline
  (setq require-final-newline t)

  ;; show tooltips in echo area
  (tooltip-mode -1)
  (setq tooltip-use-echo-area t)
  (global-set-key (kbd "C-x t") 'display-local-help) ;; show tooltip

  (setq bookmark-save-flag 1) ; everytime bookmark is changed, automatically save it

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

  ;; make sure window movement keys are not captured by GUD's comint
  (add-hook 'comint-mode-hook
	    (lambda ()
	      (define-key comint-mode-map (kbd "C-<up>") 'nil)
	      (define-key comint-mode-map (kbd "C-<down>") 'nil)
	      (define-key comint-mode-map (kbd "C-<left>") 'nil)
	      (define-key comint-mode-map (kbd "C-<right>") 'nil)))

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

  ;; Highlight comments like TODO
  (add-hook 'c-mode-common-hook
	    (lambda ()
	      (font-lock-add-keywords nil
				      '(("\\<\\(FIXME\\|TODO\\|BUG\\):" 1 font-lock-warning-face t)))))

  ;; Rename current file and buffer
  (defun rename-file-and-buffer ()
    "Rename the current buffer and file it is visiting."
    (interactive)
    (let ((filename (buffer-file-name)))
      (if (not (and filename (file-exists-p filename)))
	  (message "Buffer is not visiting a file!")
	(let ((new-name (read-file-name "New name: " filename)))
	  (cond
	   ((vc-backend filename) (vc-rename-file filename new-name))
	   (t
	    (rename-file filename new-name t)
	    (set-visited-file-name new-name t t)))))))
  (global-set-key (kbd "C-c f r")  'rename-file-and-buffer)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; nXML mode
  (setq auto-mode-alist (cons '("\\.xml$" . nxml-mode) auto-mode-alist))
  (setq auto-mode-alist (cons '("\\.xsl$" . nxml-mode) auto-mode-alist))
  (setq auto-mode-alist (cons '("\\.xhtml$" . nxml-mode) auto-mode-alist))
  (setq auto-mode-alist (cons '("\\.page$" . nxml-mode) auto-mode-alist))
  (autoload 'xml-mode "nxml" "XML editing mode" t)
  (eval-after-load 'rng-loc
    '(add-to-list 'rng-schema-locating-files "~/.schema/schemas.xml"))
  (global-set-key [C-return] 'completion-at-point)
  ;; Auto-set the schema
  (defun my-xml-schema-hook ()
    (when (string= (file-name-extension buffer-file-name) "ts")
      (rng-auto-set-schema))
  (add-hook 'find-file-hook 'my-xml-schema-hook)
  
)

;;;;;;;;;;;;;;;;;;;;;;; ToDos
;; Future things to think about (most important generally up top):
;;
;;       . bashdb (use realgud)
;;       . Python linting
;;       . Python debugging
;;       . Python shell

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Man-notify-method (quote pushy))
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#3F3F3F" "#CC9393" "#7F9F7F" "#F0DFAF" "#8CD0D3" "#DC8CC3" "#93E0E3" "#DCDCCC"])
 '(company-begin-commands nil)
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
 '(fci-rule-color "#383838")
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(pdf-view-midnight-colors (quote ("#DCDCCC" . "#383838")))
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   (quote
    ((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3")
 '(livedown-autostart nil) ; automatically open preview when opening markdown files
 '(livedown-open t)        ; automatically open the browser window
 '(livedown-port 1337)     ; port for livedown server
 '(livedown-browser nil)   ; browser to use
 )

(add-to-list 'load-path (expand-file-name "~/.emacs.d/emacs-livedown"))
(require 'livedown)

(let ((bg (face-attribute 'default :background)))
  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(ecb-default-highlight-face ((((class color) (background dark)) (:background "yellow" :foreground "black"))))
   '(ecb-tag-header-face ((((class color) (background dark)) (:background "yellow" :foreground "black"))))
   )
  )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ecb-default-highlight-face ((((class color) (background dark)) (:background "yellow" :foreground "black"))))
 '(ecb-tag-header-face ((((class color) (background dark)) (:background "yellow" :foreground "black")))))
