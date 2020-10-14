(setq gc-cons-threshold most-positive-fixnum) ;; no garbage collection at startup
(add-hook 'after-init-hook (lambda ()
			     ;; 16MB of garbage collection space once running
			     (setq gc-cons-threshold (eval-when-compile (* 1024 1024 100)))
			     ))
;; (setq gc-cons-percentage 0.5)
;; (setq gc-cons-threshold most-positive-fixnum)
;; (setq gc-cons-threshold 1600000)

;; Show message in mode line whenever garbage collection occurs
;; (setq garbage-collection-messages t)

(setq my-gc-collect-interval 30)
(run-with-idle-timer my-gc-collect-interval t
		     (lambda ()
		       ;; Garbage-collect after 30 seconds of idleness
		       (garbage-collect)
		       (message "%s Routine %ds garbage collection"
				(all-the-icons-faicon "trash-o") my-gc-collect-interval)))
(setq read-process-output-max (* 1024 1024 10)) ;; 10mb

;; To byte-compile the Emacs init directory, run Emacs and execute
;; M-x byte-compile-init-dir

;;;;;;;;;;;;;;;;; MOST IMPORTANT, GENERAL STUFF

;; NB: use M-x customize to search and set package variables

;; Default directory
(setq default-directory "~/")

;; Frame size
(add-to-list 'default-frame-alist '(height . 50))
(add-to-list 'default-frame-alist '(width . 100))

;; sensible GUI
(menu-bar-mode -1)  ;; disable menubar
(tool-bar-mode -1) ;; disable toolbar
(toggle-scroll-bar -1) ;; disable scrollbar

;; Comint unset C-up C-down
(global-unset-key (kbd "C-<up>"))
(global-unset-key (kbd "C-<down>"))

;; Lisp deprecation
;; https://github.com/kiwanami/emacs-epc/issues/35
(setq byte-compile-warnings '(cl-functions))

;;;;;;;;;;;;;;;;; PACKAGE MANAGEMENT

;; MELPA
(require 'package)
(setq package-enable-at-startup nil)
(setq package-check-signature nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("elpy" . "https://jorgenschaefer.github.io/packages/"))
;; (package-initialize)

;; ..:: bootstrap 'straight.el' ::..
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; ..:: boostrap 'use-package' ::..
;; (unless (package-installed-p 'use-package)
;;   (package-refresh-contents)
;;   (package-install 'use-package))
(straight-use-package 'use-package)
(use-package el-patch
  :straight t)
(require 'use-package)
(require 'bind-key)
(setq straight-enable-use-package-integration t)
(setq straight-use-package-by-default t)

;;;;;;;;;;;;;;;;; EMACS BUILT-IN

(use-package windmove
  ;; move cursor between windows
  :demand t
  :bind
  (("C-<left>" . windmove-left)
   ("C-<right>" . windmove-right)
   ("C-<up>" . windmove-up)
   ("C-<down>" . windmove-down)))

(use-package buffer-menu
  ;; show all current buffers
  :straight nil
  :init
  (setq Buffer-menu-name-width 25)
  (setq Buffer-menu-mode-width 6)
  (setq Buffer-menu-size-width 10)
  :bind
  (("C-x C-b" . buffer-menu)))

;;;;;;;;;;;;;;;;; MELPA PACKAGES

(use-package dash
  :ensure t)

(use-package dash-functional
  :ensure t)

(use-package rainbow-delimiters
  ;; highlights delimiters such as parentheses, brackets or braces according to their depth.
  :ensure t
  :config
  (require 'rainbow-delimiters)
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package solaire-mode
  :ensure t
  :config
  (require 'solaire-mode)
  ;; brighten buffers (that represent real files)
  (add-hook 'after-change-major-mode-hook #'turn-on-solaire-mode)
  ;; To enable solaire-mode unconditionally for certain modes:
  (add-hook 'ediff-prepare-buffer-hook #'solaire-mode)
  ;; ...if you use auto-revert-mode:
  (add-hook 'after-revert-hook #'turn-on-solaire-mode)
  ;; highlight the minibuffer when it is activated:
  (add-hook 'minibuffer-setup-hook #'solaire-mode-in-minibuffer)
  )

(use-package nlinum
  :ensure t
  :config
  (setq nlinum-highlight-current-line nil)
  )

(use-package doom-themes
  ;; DOOM Themes is an opinionated UI plugin and pack of themes extracted from my emacs.d, inspired by the One Dark/Light UI and syntax themes in Atom.
  :ensure t
  :config
  (require 'doom-themes)
  ;; Load the theme (doom-one, doom-dark, etc.)
  (load-theme 'doom-one t)
  ;; Enable custom neotree theme
  (doom-themes-neotree-config)
  )

(use-package neotree
  ;; Good tree-based project file browser
  ;; Need to follow up with M-x all-the-icons-install-fonts
  :ensure t
  :after (all-the-icons)
  :config
  (require 'neotree)
  (global-set-key (kbd "C-c n") 'neotree-toggle))

(use-package all-the-icons
  ;; Icons for NeoTree
  ;; Need to run M-x all-the-icons-install-fonts
  :ensure t
  :config
  (use-package all-the-icons))

(use-package flycheck
  ;; code error highlighting
  ;; Linux Mint:
  ;;  sudo apt-get install shellcheck # bash script linter
  :ensure t
  :defer t
  :init
  (add-hook 'c-mode-common-hook 'flycheck-mode)
  (add-hook 'sh-mode-hook 'flycheck-mode)
  ;; (add-hook 'python-mode-hook 'flycheck-mode)
  (setq flycheck-enabled-checkers '(c/c++-gcc))
  ;;(add-hook 'python-mode-hook 'flycheck-mode)
  :config
  ;; Check buffer on save and immediately after opening buffer
  (setq flycheck-check-syntax-automatically '(mode-enabled save))
  ;; Python
  (setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)
  (add-to-list 'flycheck-checkers 'python-flake8)
  ;; Delay in showing errors
  (setq flycheck-display-errors-delay 0.1)
  )

;; MATLAB integration
;; --------
;; To install, run in ~/.emacs.d/
;;   git clone https://github.com/dmalyuta/matlab-mode
;;   git clone https://github.com/dmalyuta/matlab-emacs
;;   cd matlab-emacs
;;   make
;; --------
(use-package matlab-load
  :straight nil
  :load-path "matlab-emacs/"
  :config
  (require 'matlab-load)
  (require 'company-matlab-shell)
  (matlab-cedet-setup)
  (setq matlab-verify-on-save-flag nil)
  (defun my-matlab-mode-hook () (setq fill-column 80))
  (setq matlab-indent-function-body nil)
  )

(use-package matlab-mode
  :straight nil
  :load-path "matlab-mode/"
  :config
  (setq default-fill-column 80)
  (require 'matlab-mode)
  (require 'matlab-server)
  )

(defun matlab-my-view-doc ()
    "look up the matlab help info and show in another buffer"
    (interactive)
    (let* ((word (doc-matlab-grab-current-word)))
      (matlab-shell-describe-command word)))

(add-to-list 'matlab-mode-hook
	     (lambda ()
	       ;; todo mode
	       (hl-todo-mode)
	       ;; bind key for starting the matlab shell
	       (local-set-key (kbd "M-s") 'matlab-shell)
	       ;; bind the key of checking document
	       (local-set-key (kbd "C-c h") 'matlab-my-view-doc)
	       ;; bind the key of jump to source code
	       (local-set-key (kbd "C-c s")
			      'matlab-jump-to-definition-of-word-at-cursor)
	       ;; set company-backends
	       (setq-local company-backends '(company-files (company-matlab company-dabbrev)))
	       ))

(add-to-list 'matlab-shell-mode-hook
	     (lambda ()
	       ;; bind key for completion
	       ;; (not company-mode)
	       ;; (define-key matlab-shell-mode-map (kbd "S-SPC") 'nil)
	       (local-set-key (kbd "S-SPC") 'matlab-shell-tab)
	       ;; bind the key of checking document
	       (local-set-key (kbd "C-c h") 'matlab-my-view-doc)))

(defun matlab-docstring ()
    "Print a default docstring for a MATLAB function."
    (interactive)
    (insert "% <FUNCTION NAME> <DESCRIPTION>\n")
    (insert "%\n")
    (insert "% Syntax:\n")
    (insert "%\n")
    (insert "% <OUT> = <FUNCTION NAME>(<IN>) : <DESCRIPTION>\n")
    (insert "%\n")
    (insert "% Inputs:\n")
    (insert "%\n")
    (insert "% <IN1> [<TYPE>] : <DESCRIPTION>\n")
    (insert "% <IN2> [<TYPE>] : <DESCRIPTION>\n")
    (insert "% \n")
    (insert "% Outputs:\n")
    (insert "%\n")
    (insert "% <OUT1> [<TYPE>] : <DESCRIPTION>\n")
    (insert "% <OUT2> [<TYPE>] : <DESCRIPTION>\n")
    (insert "%\n")
    (insert "% Other m-files required: none\n")
    (insert "% Subfunctions: none\n")
    (insert "% MAT-files required: none\n")
    )

(use-package company
  ;; complete anything
  ;;
  ;; Other useful shortcuts:
  ;;   C-M-/ runs the command dabbrev-completion (search all current buffers for
  ;;   completion candidates)
  :ensure t
  :bind
  (("<S-SPC>" . company-complete)
   )
  :init
  ;;(add-hook 'after-init-hook 'global-company-mode)
  (add-hook 'c-mode-common-hook 'company-mode)
  (add-hook 'emacs-lisp-mode-hook 'company-mode)
  (add-hook 'comint-mode-hook 'company-mode)
  (add-hook 'LaTeX-mode-hook 'company-mode)
  (add-hook 'sh-mode-hook 'company-mode)
  (add-hook 'matlab-mode-hook 'company-mode)
  ;;(add-hook 'sh-mode-hook 'company-mode)
  ;; (add-hook 'python-mode-hook 'company-mode)
  :config
  (setq company-dabbrev-downcase 0)
  ;; set timeout to 10 seconds
  (setq company-async-timeout 10)
  ;; Cancel selections by typing non-mathcing characters
  (setq company-require-match 'never)
  ;; Minimum length of word to start completion
  (setq company-minimum-prefix-length 0)
  ;; Autocomplete only when I explicitly mean to
  (setq company-auto-complete nil)
  (set 'company-idle-delay nil)
  (setq company-auto-select-first-candidate nil)
  ;; nomenclature for latex
  (eval-after-load "tex"
    '(add-to-list 'TeX-command-list
		  '("Nomenclature" "makeindex %s.nlo -s nomencl.ist -o %s.nls"
		    (lambda (name command file)
		      (TeX-run-compile name command file)
		      (TeX-process-set-variable file 'TeX-command-next TeX-command-default))
		    nil t :help "Create nomenclature file")))
  )

(use-package company-box
  ;;  A company front-end with icons
  :ensure t
  :config
  (require 'company-box)
  (add-hook 'company-mode-hook 'company-box-mode)
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
  ;; :load-path "elpa/helm-company"
  ;;:ensure t ;; Waiting for MELPA update (fix was made by maintainer to
  ;;             autofill "pattern" in response to my issue)
  :config
  (autoload 'helm-company "helm-company") ;; Not necessary if using ELPA package
  ;; (eval-after-load 'company
  ;;   '(progn
  ;;      (define-key company-mode-map (kbd "C-:") 'helm-company)
  ;;      (define-key company-active-map (kbd "C-:") 'helm-company)))
  (eval-after-load 'company
    '(progn
       (set-variable 'helm-company-initialize-pattern-with-prefix t)
       (define-key company-mode-map (kbd "<S-SPC>") 'helm-company)
       (define-key company-active-map (kbd "<S-SPC>") 'helm-company)))
  )

(use-package company-shell
  ;; company mode completion backends for your shell functions:
  :ensure t
  :config
  (add-to-list 'company-backends 'company-shell)
  )

(use-package modern-cpp-font-lock
  ;; Syntax highlighting support for "Modern C++" - until C++20 and Technical
  ;; Specification. This package aims to provide a simple highlight of the C++
  ;; language without dependency.
  :ensure t
  :config
  (add-hook 'c++-mode-hook #'modern-c++-font-lock-mode)
  )

(use-package helm
  ;; incremental completion and selection narrowing framework
  ;; Useful bindinds:
  ;;  C-t : cycle helm buffer position (up/down/left/right)
  ;;  C-SPC : select completion candidate
  ;;  M-a : select all completion candidates
  ;;  M-y : show the kill ring
  ;;  C-M-a and C-M-e : jump to beginning/end of function definition
  ;;  C-c h m : open man pages for symbol at point or search
  ;;  C-] : toggle to show only file names in helm buffer
  ;;  C-c r : open a file with sudo permissions
  ;;  C-c q : show list of classes, functions, etc
  ;;  C-c h i : find major definitions (e.g. of functions and variables) in C/C++, Lisp and a ton of other languages
  ;;  *<STUFF> : substring search major mode in helm-mini (i.e. after C-x b)
  :ensure t
  :demand
  :bind
  (("M-x" . helm-M-x)
   ("M-y" . helm-show-kill-ring)
   ("C-x b" . helm-mini)
   ("C-x C-f" . helm-find-files)
   ("C-c q" . helm-semantic-or-imenu))
  :config
  (require 'helm)
  (require 'helm-config)
  (helm-mode 1)
  (add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)
  (setq helm-buffer-max-length 30) ;; nil to show full name always
  (setq helm-buffers-truncate-lines nil)
  (setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
	helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
	helm-ff-file-name-history-use-recentf t
	helm-echo-input-in-header-line t)
  (setq helm-buffers-fuzzy-matching t
	helm-recentf-fuzzy-match    t)
  ;; Remove duplicate entries from history
  (setq history-delete-duplicates t)
  ;; (setq history-length 20)
  )

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
  ;; (message "ROS environment variables not defined, won't load helm-ros")
  )

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
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package helm-projectile
  ;; Helm UI for Projectile
  :ensure t
  :config
  (setq projectile-completion-system 'helm)
  (helm-projectile-on)
  (setq projectile-switch-project-action 'helm-projectile))

(use-package tex
  ;; AUCTeX
  ;;
  ;; Commands:
  ;;   C-c & : find citation in bib file (when cursor over \cite{...})
  ;;
  :straight nil
  :ensure auctex
  :demand
  :config
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (add-hook 'LaTeX-mode-hook 'visual-line-mode)
  (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
  (add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)
  (setq TeX-source-correlate-method 'synctex) ;; use SyncTeX for forward/backward search between source/PDF output
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq TeX-auto-regexp-list 'TeX-auto-full-regexp-list)
  (setq TeX-auto-parse-length 999999)
  (setq TeX-save-query nil)
  (setq-default TeX-master nil)
  ;; View program
  (add-hook 'LaTeX-mode-hook
	    (lambda ()
	      (setq TeX-view-program-list '(("Evince" "evince --page-index=%(outpage) %o")))
	      (setq TeX-view-program-selection '((output-pdf "Evince")))))
  (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
  ;; Spelling
  (global-set-key (kbd "M-s") 'ispell-word)
  ;; (setq ispell-program-name "aspell") ; could be ispell as well, depending on your preferences
  ;; (setq ispell-dictionary "english") ; this can obviously be set to any language your spell-checking program supports
  (setq reftex-plug-into-AUCTeX t)
  ;; Enable auto-fill mode, nice for text
  (add-hook 'LaTeX-mode-hook 'auto-fill-mode)
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (setq reftex-plug-into-AUCTeX t)
  ;; Set fill to 80 columns
  (add-hook 'LaTeX-mode-hook
	    (lambda ()
	      (setq fill-column 80)))
  ;; Completion
  ;; (add-hook 'LaTeX-mode-hook
  ;; 	      (lambda ()
  ;; 		(define-key LaTeX-mode-map (kbd "<S-SPC>") 'TeX-complete-symbol)))
  (use-package company-auctex
    :ensure t
    :config
    (require 'company-auctex)
    (company-auctex-init)
    (eval-after-load "latex"
      '(progn
	 (define-key LaTeX-mode-map (kbd "C-c m") 'nil)
	 (define-key LaTeX-mode-map (kbd "C-c e") 'nil)
	 (define-key LaTeX-mode-map (kbd "C-c l") 'nil)
	 (define-key LaTeX-mode-map (kbd "C-c s") 'nil)

	 (define-key LaTeX-mode-map (kbd "C-c m") 'company-auctex-macros)
	 (define-key LaTeX-mode-map (kbd "C-c e") 'company-auctex-environments)
	 (define-key LaTeX-mode-map (kbd "C-c l") 'company-auctex-labels)
	 (define-key LaTeX-mode-map (kbd "C-c s") 'company-auctex-symbols)
	 ))
    ;; Makke completion case sensitive
    (add-hook 'LaTeX-mode-hook (lambda ()
				 (setq company-dabbrev-downcase nil)))
    )
  ;; Other environments
  (add-hook 'LaTeX-mode-hook
	    (lambda ()
	      (LaTeX-add-environments "equation*")
	      (LaTeX-add-environments "tikzpicture")
	      (LaTeX-add-environments "pgfonlayer")
	      ))
  ;; Spell checking
  ;; (add-hook 'LaTeX-mode-hook 'flyspell-mode)
  ;; (add-hook 'LaTeX-mode-hook 'flyspell-buffer)
  ;; Line breaking math
  (add-hook 'LaTeX-mode-hook
	    (lambda ()
	      (add-to-list 'fill-nobreak-predicate 'texmathp)))
  ;; Latex mode for TikZ
  (add-to-list 'auto-mode-alist '("\\.tikz\\'" . latex-mode))
  ;; shell-escape stuff
  (eval-after-load "tex"
    '(setcdr (assoc "LaTeX" TeX-command-list)
	     '("%`%l%(mode) -shell-escape%' %t"
	       TeX-run-TeX nil (latex-mode doctex-mode) :help "Run LaTeX")
	     )
    )
  ;; latexmk
  ;; (add-hook 'LaTeX-mode-hook (lambda ()
  ;; 			       (push
  ;; 				'("latexmk" "latexmk -pdf %s" TeX-run-TeX nil t
  ;; 				  :help "Run latexmk on file")
  ;; 				TeX-command-list)))
  ;; Pairing
  ;; brace electric pair
  (setq LaTeX-electric-left-right-brace t)
  (setq TeX-electric-math '("$" . "$"))
  (add-hook 'LaTeX-mode-hook (lambda ()
			       (define-key LaTeX-mode-map (kbd "C-x C-<backspace>")
				 'electric-pair-delete-pair)))
  ;;
  ;;
  ;; Command sequence for Pythontex
  ;;
  ;;
  (with-eval-after-load "tex"
    (add-to-list 'TeX-command-list
		 '("PythonTeX" "pythontex %s" TeX-run-command nil (latex-mode)
                   :help "Run PythonTeX script")
		 t))
  (with-eval-after-load "tex"
    (add-to-list 'TeX-command-list
		 '("CSM-BibTeX" "bibtex main && bibtex sidebar" TeX-run-command nil (latex-mode)
                   :help "Run BibTeX for CSM")
		 t))
  (defun my/TeX-run-TeX-pythontex ()
    "Save current master file, run LaTeX, PythonTeX and start the viewer."
    (interactive)
    (unless (featurep 'tex-buf)
      (require 'tex-buf))
    (TeX-save-document (TeX-master-file))
    (TeX-command-sequence '("LaTeX" "PythonTeX" "LaTeX")
                          t))
  (define-key LaTeX-mode-map (kbd "C-c C-p C-l") #'my/TeX-run-TeX-pythontex)
  ;;
  ;; Customization for Pythontex
  ;;
  (add-to-list 'LaTeX-verbatim-environments "pycode")
  (add-to-list 'LaTeX-verbatim-environments "pykzmathinline")
  (add-to-list 'LaTeX-verbatim-environments "pykzmathblock")
  (add-to-list 'LaTeX-verbatim-environments "@pie@shell")
  (add-to-list 'LaTeX-indent-environment-list '("pycode" current-indentation))
  (add-to-list 'LaTeX-indent-environment-list '("pykzmathblock" current-indentation))
  (add-to-list 'LaTeX-indent-environment-list '("@pie@shell" current-indentation))
  )

;; (use-package mmm-mode
;;   :ensure t
;;   :config
;;   (require 'mmm-auto)
;;   (mmm-add-group 'latex-python
;; 		 '((latex-python-envs
;; 		    :submode python-mode
;; 		    :face mmm-default-submode-face
;; 		    :front "\\\\begin{\\(pycode\\|pykzmath\\)}\n"
;; 		    :back "\\\\end{~1}\n"
;; 		    :save-matches 1)))
;;   ;; (mmm-add-classes
;;   ;;  '((latex-python
;;   ;;     :submode python-mode
;;   ;;     :face mmm-delimiter-face
;;   ;;     :front "\\\\begin{pycode}\n"
;;   ;;     :back "\\\\end{pycode}\n")))

;;   (setq mmm-global-mode 'maybe)
;;   (mmm-add-mode-ext-class 'latex-mode nil 'latex-python)
;;   )

;; (use-package yasnippet
;;   ;; YASnippet is a template system for Emacs
;;   :ensure t
;;   :config
;;   (require 'yasnippet)
;;   (yas-global-mode 1)
;;   )

;; (use-package pdf-tools
;;   ;; advanced PDF viewing capbilities inside Emacs (DocView deplacement)
;;   :ensure t
;;   :config
;;   (pdf-tools-install)
;;   (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
;;   (setq TeX-view-program-selection '((output-pdf "PDF Tools"))))

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
  (add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))
  (add-to-list 'auto-mode-alist '("\\.srv$" . yaml-mode))
  (add-to-list 'auto-mode-alist '("\\.msg$" . yaml-mode))
  )

(use-package google-c-style
  ;; provides the Google C/C++ coding style
  :ensure t
  :config
  (add-hook 'c-mode-common-hook 'google-set-c-style))

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
  (require 'multiple-cursors)
  ;; Make sure prompts for stuff like (, [, <, delete-word, etc. default to "yes"
  ;; see https://github.com/magnars/multiple-cursors.el/issues/287#issuecomment-306571652
  (eval-after-load "multiple-cursors"
    '(progn
       (setq mc/cmds-to-run-for-all
	     (append
	      '(c-electric-star
		c-electric-brace
		c-electric-paren
		c-electric-colon
		c-electric-slash
		c-electric-lt-gt
		c-electric-pound
		c-electric-delete
		c-electric-backspace
		c-electric-semi&comma
		c-electric-delete-forward)
	      mc/cmds-to-run-for-all))))
  )

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
  :ensure t
  :defer t)

(use-package org
  ;; Org mode is for keeping notes, maintaining TODO lists, planning projects,
  ;; and authoring documents with a fast and effective plain-text system.
  ;;
  ;; Commands:
  ;;
  ;;   TAB : collapse a section
  ;;   M-RET : insert new heading at this level
  ;;   M-S-<leftarrow> : demote current subtree
  ;;   M-S-<rightarrow> : promote current subtree
  ;;   M-<uparrow> : move heading up in subtree
  ;;   M-<downarrow> : move heading down in subtree
  ;;   C-c C-o : open link at point
  ;;
  :ensure t
  :config

  (use-package org-bullets
    :ensure t
    :config
    (require 'org-bullets)
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
    )

  (require 'org)
  (add-hook 'org-mode-hook 'turn-on-auto-fill)
  (define-key global-map "\C-cl" 'org-store-link)
  (define-key global-map "\C-ca" 'org-agenda)
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.3))
  (setq org-log-done t)
  (setq org-agenda-files (list "~/Dropbox/shared_files/org/work.org"
			       "~/Dropbox/shared_files/org/home.org")))

(use-package ecb
  ;; Emacs Code Browser
  :ensure t
  :bind
  (("C-c e a" . ecb-activate)
   ("C-c e d" . ecb-deactivate)
   )
  :config
  (require 'ecb)
  ;;(setq ecb-auto-activate t) ;; auto-activate ECB at startup
  (setq ecb-layout-name "left11") ;; set ECB layout
  (setq ecb-tip-of-the-day nil) ;; turn off ECB tip of the day message
  ;; also see custom-set-variables and custom-set-faces below
  )

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

(use-package magit
  :ensure t
  :bind
  (("C-x g" . magit-status)
   )
  :config
  )

(use-package move-text
  ;; Move current line or region up or down
  :ensure t
  :config
  (move-text-default-bindings)
  )

;; (use-package undo-tree
;;   ;; Better undo/redo functionality, including undo tree browsing
;;   :ensure t
;;   :config
;;   (require 'undo-tree)
;;   (global-undo-tree-mode)
;;   (setq undo-tree-limit nil)
;;   )
(use-package redo+
  :straight nil
  :load-path "lisp/"
  :config
  (require 'redo+)
  (global-set-key (kbd "C-/") 'undo)
  (global-set-key (kbd "C-?") 'redo)
  )

(use-package column-enforce-mode
  :ensure t
  :config
  (setq-default fill-column 80)
  (setq column-enforce-column fill-column)
  (add-hook 'c-mode-common-hook 'column-enforce-mode)
  (add-hook 'python-mode-hook 'column-enforce-mode)
  (add-hook 'matlab-mode-hook 'column-enforce-mode)
  )

(use-package hl-todo
  :ensure t
  :config
  (global-hl-todo-mode)
  )

(use-package plantuml-mode
  :ensure t
  :config
  ;; Enable plantuml-mode for PlantUML files
  (add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))
  )

(use-package helm-ag
  ;; provides interfaces of The Silver Searcher with helm.
  ;; Workflow to edit multiple files:
  ;; C-c p s s : runs helm-projectile-ag, can do something like
  ;;             `-G.[cpph]$ foobar' to search for foobar in .cpp and .h files
  ;;             in the project
  ;; C-c C-e : while in the helm-ag buffer, make it into a new buffer for editing
  ;; <do your replacements...>
  ;; C-c C-c : save edits across all files and close the helm-ag buffer. Done!
  :ensure t
  :config
  )

(use-package all-the-icons
  ;; Icons font - needed for doom-modeline
  :ensure t)

(use-package doom-modeline
  ;; Sexy modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))

(use-package sage-shell-mode
  :ensure t
  :config
  (setq ac-sage-show-quick-help t))

(use-package dashboard
  ;; An extensible emacs startup screen showing you what’s most important.
  :ensure t
  :diminish dashboard-mode
  :config
  (setq dashboard-banner-logo-title "Change the world, step by step")
  ;;(setq dashboard-startup-banner "/path/to/image")
  (setq dashboard-items '((recents  . 5)
			  (bookmarks . 5)
			  (projects . 5)))
  (setq dashboard-center-content t)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (dashboard-setup-startup-hook)
  )

(use-package unfill
  ;; Opposite of Emacs fill
  :ensure t
  :config
  (require 'unfill))

(use-package ess
  ;; Emacs Speaks Statistics
  ;; For Julia
  ;; First run M-x julia to have the inferior shell.
  ;; Then useful commands:
  ;;   C-c C-j : run Julia shell
  ;;   C-c C-c : run next contiguous block of code
  ;;   C-c C-l : send file to buffer via include()
  ;;   C-c C-d C-d : help at point
  ;;   C-c C-h C-i : show all functions list, or go to function at point
  ;;   C-u C-c C-l : replace with latex character
  ;;   M-/ : cycle through completion candidates
  ;;   C-RET : execute current line and step to next line
  :ensure t
  :config
  (require 'ess-site)
  (setq ess-use-company t)
  (setq ess-tab-complete-in-script t)
  (add-hook 'ess-julia-mode-hook
	    (lambda ()
	      (define-key ess-julia-mode-map (kbd "C-u C-x C-;") 'uncomment-region)
	      (define-key ess-julia-mode-map (kbd "S-SPC") 'complete-symbol)
	      (define-key ess-julia-mode-map (kbd "C-u C-c C-l") 'julia-latexsub-or-indent)
	      (define-key ess-julia-mode-map (kbd "C-u C-j") 'run-ess-julia)
	      (define-key ess-julia-mode-map (kbd "C-u C-SPC")
		(lambda () (interactive) (set-mark-command -1))) ;; Go to previous mark
	      (define-key inferior-ess-julia-mode-map (kbd "C-u C-c C-l") 'julia-latexsub-or-indent)))
  (add-to-list 'ess-tracebug-search-path "~/julia/julia-1.2.0/share/julia/base")
  (add-hook 'ess-julia-mode-hook
	    (lambda ()
	      (setq set-mark-command-repeat-pop t)))
  )

;; LANGUAGE LINTING/IDE FEATURES
;; Use lsp-mode (Language Server Protocol)
;; see https://github.com/emacs-lsp/lsp-mode/blob/master/README.org

(use-package company-c-headers
  :ensure t
  :config
  (add-to-list 'company-backends 'company-c-headers)
  (add-to-list 'company-c-headers-path-system "/usr/include/c++/7/")
  )

(use-package ccls
  ;; ccls is a stand-alone server implementing the Language Server Protocol for
  ;; C, C++, and Objective-C language.
  :ensure t
  :config
  (require 'ccls)
  (setq ccls-executable "ccls") ;; Must be on PATH: assume /usr/local/bin/ccls
  ;; (setq lsp-clients-clangd-executable "/usr/bin/clang")
  (setq lsp-prefer-flymake nil)
  (setq-default flycheck-disabled-checkers '(c/c++-clang c/c++-cppcheck c/c++-gcc))
  :hook ((c-mode c++-mode objc-mode) .
         (lambda () (require 'ccls) (lsp)))
  )

(use-package pyvenv
  :disabled
  :ensure t
  :diminish
  :config

  (setenv "WORKON_HOME" "~/anaconda3/envs")
  ;; Show python venv name in modeline
  (setq pyvenv-mode-line-indicator
	'(pyvenv-virtual-env-name
	  ("[venv:" pyvenv-virtual-env-name "] ")))
  (pyvenv-mode t))

(use-package lsp-pyright
  :ensure t
  :straight (:host github
		   :repo "emacs-lsp/lsp-pyright"
		   :branch "master")
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp)))  ;; or lsp-deferred
  :config
  ;; Do not print "analyzing... Done" in the minibuffer
  ;; (I find it distracting)
  (setq lsp-pyright-show-analyzing nil)
  )

(use-package lsp-mode
  ;; Emacs client/library for the Language Server Protocol
  ;;y
  ;; Sometimes it is useful to restart the LSP worspace using
  ;; `M-x lsp-workspace-restart`
  ;;
  ;; Useful tools:
  ;;  M-x lsp-rename : rename all instances of a variable.
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  ;; (setq lsp-clients-clangd-executable "clangd")

  (require 'lsp-mode)

  ;; Turn off the annoying progress spinner
  (setq lsp-progress-via-spinner nil)

  ;; Turn off LSP-mode imenu
  (setq lsp-enable-imenu nil)

  ;; Optional: fine-tune lsp-idle-delay. This variable determines how often
  ;; lsp-mode will refresh the highlights, lenses, links, etc while you type.
  (setq lsp-idle-delay 0.5) ;; seconds

  ;; Use clangd instead
  ;; https://www.reddit.com/r/cpp/comments/cafj21/ccls_clangd_and_cquery/
  ;; (setq lsp-disabled-clients '(ccls))

  ;; Use user-installed pyls instead of auto-installed mspyls
  ;; (setq lsp-disabled-clients '(pyls))

  ;; Which languages to activate LSP mode for
  (add-hook 'python-mode-hook #'lsp)
  (add-hook 'c-mode-common-hook #'lsp)

  ;; Use company-capf for completion. Although company-lsp also supports caching
  ;; lsp-mode’s company-capf does that by default. To achieve that uninstall
  ;; company-lsp or put these lines in your config:
  (require 'company-capf)
  (setq lsp-prefer-capf t)
  ;; From Gitter chat for lsp-mode:
  ;; @kiennq wrote:
  ;; @dmalyuta clangd should be able to find and suggest iostream with correct
  ;; configuration for compilation database json.  You can try to use
  ;; https://github.com/rizsotto/Bear or CMAKE to generate that.
  ;;
  ;; To get both completion providers to contribute you can try to use multiple
  ;; backends, set company-backends to something like (setq
  ;; company-backends(company-capf company-c-headers))and
  ;; setlsp-completion-providerto:none so it will not change
  ;; yourcompany-backends` setup.
  ;;
  ;; Also a note about your configuration: You should put all of setq to :init
  ;; block, so it will work regardless of that setting is checked when lsp-mode
  ;; is running or just after load.  (setq lsp-completion-provider :capf)
  (push 'company-capf company-backends)
  (push 'company-c-headers company-backends)

  ;; Linting
  (setq lsp-diagnostic-package :none)
  ;; See more options via M-x customize-group lsp-pyls
  ;; (setq-default lsp-pyls-configuration-sources ["flake8"])
  ;; (setq lsp-pyls-plugins-pyflakes-enabled nil)
  ;; (setq lsp-pyls-plugins-pylint-enabled nil)
  ;; (setq lsp-pyls-plugins-mccabe-enabled nil)

  ;; Modeline mode
  (with-eval-after-load 'lsp-mode
    ;; :project/:workspace/:file
    (setq lsp-modeline-diagnostics-scope :none))

  ;; Recommended settings
  (add-hook 'lsp-mode-hook (lambda ()
			     (setq company-minimum-prefix-length 1
				   company-idle-delay 0.0)))

  ;; Other niceties
  (setq lsp-enable-semantic-highlighting t)
  (setq lsp-enable-snippet nil)  ;; Enable arguments completion
  (setq lsp-signature-auto-activate nil)
  )

;;===============================================================
;;===============================================================
;; ..:: Code linting for Python ::..
(add-hook 'python-mode-hook
	  (lambda ()
	    (flycheck-mode)
	    ;; Flake8: config file defined at the global level via
	    ;; M-x flycheck-flake8rc --> set to "~/setup.cfg"
	    ;; (setq flycheck-enabled-checkers '(python-flake8 python-pyright))
	    (setq flycheck-enabled-checkers '(python-flake8))
	    (setq flycheck-disabled-checkers '(python-mypy python-pylint python-pyright))
	    ;; Multiple checkers: flake8, then pyright
	    ;; See: https://www.flycheck.org/en/latest/user/syntax-checkers.html#configuring-checker-chains
	    ;; (flycheck-add-next-checker 'python-flake8 'python-pyright)
	    ))
(global-set-key (kbd "C-c f l") 'flycheck-list-errors) ;; List all errors
(global-set-key (kbd "C-c f e") 'flycheck-display-error-at-point) ;; Show error at current cursor position
(global-set-key (kbd "C-c f v") 'flycheck-verify-setup) ;; Show active/disabled checkers
;;===============================================================
;;===============================================================

(use-package lsp-ui
  :ensure t
  :config
  (add-hook 'lsp-mode-hook
	    (lambda ()
	      (setq lsp-ui-doc-enable nil ;; disable docs
		    ;; lsp-ui-doc-delay 1
   		    ;; lsp-ui-doc-use-childframe t
   		    lsp-ui-doc-position 'bottom
		    lsp-ui-doc-max-height 20
   		    lsp-ui-doc-include-signature t

   		    lsp-ui-sideline-enable nil
		    lsp-ui-sideline-delay 0.05
		    lsp-ui-sideline-show-code-actions nil
		    lsp-ui-sideline-show-hover nil

   		    lsp-ui-flycheck-enable t
   		    lsp-ui-flycheck-list-position 'right
   		    lsp-ui-flycheck-live-reporting t

		    lsp-ui-peek-enable nil
   		    ;; lsp-ui-peek-list-width 60
   		    ;; lsp-ui-peek-peek-height 25

		    lsp-ui-imenu-enable nil
		    ;; lsp-ui-imenu-kind-position 'top
		    )
	      ;; (local-set-key (kbd "C-c l d s") 'lsp-ui-doc-show)
	      ;; (local-set-key (kbd "C-c l d f") 'lsp-ui-doc-focus-frame)
	      ;; (local-set-key (kbd "C-c l d u") 'lsp-ui-doc-unfocus-frame)
	      (local-set-key (kbd "C-c l i") 'lsp-ui-imenu)
	      ))
  )

(use-package elpy
  ;; Elpy is an Emacs package to bring powerful Python editing to Emacs.
  ;; Use it here just for some benefits:
  ;;  - Documentation in a different buffer (via `C-c C-d`)
  :disabled
  :ensure t
  :config
  (add-hook 'python-mode-hook
	    (lambda ()
	      (local-set-key (kbd "C-c C-d") 'elpy-doc) ;; Documentation for thing at point
	      ))
  )

(use-package gnuplot
  ;; A major mode for Emacs for interacting with Gnuplot
  :ensure t
  :config
  ;; these lines enable the use of gnuplot mode
  (autoload 'gnuplot-mode "gnuplot" "gnuplot major mode" t)
  (autoload 'gnuplot-make-buffer "gnuplot" "open a buffer in gnuplot mode" t)

  ;; this line automatically causes all files with the .gp extension to be loaded into gnuplot mode
  (setq auto-mode-alist (append '(("\\.gp$" . gnuplot-mode)) auto-mode-alist))
  (setq auto-mode-alist (append '(("\\.gnu$" . gnuplot-mode)) auto-mode-alist))

  ;; This line binds the function-9 key so that it opens a buffer into gnuplot mode
  (global-set-key [(f9)] 'gnuplot-make-buffer)
  )

(use-package graphviz-dot-mode
  ;; Emacs package for working with Graphviz DOT-format files.
  :ensure t
  :config
  (setq graphviz-dot-indent-width 4))

(use-package default-text-scale
  ;; Easily adjust the font size in all Emacs frames
  ;; Key bindings:
  ;;   C-M-= : zoom in
  ;;   C-M-- : zoom out
  :ensure t
  :config
  (default-text-scale-mode)
  )

(use-package rainbow-mode
  ;; This minor mode sets background color to strings that match color names,
  ;; e.g. #0000ff is displayed in white with a blue background.
  ;; See: https://jblevins.org/log/rainbow-mode
  :ensure t
  :config
  (add-hook 'LaTeX-mode-hook
	    (lambda ()
	      (rainbow-mode 1)))
  )

;;;;;;;;;;;;;;;;; NON-MELPA PACKAGES

;; filladapt
;;
(use-package filladapt
  :ensure t
  :config
  (require 'filladapt)
  (setq-default filladapt-mode t)
  (add-hook 'c-mode-common-hook 'turn-on-filladapt-mode)
  (add-hook 'python-mode-hook 'turn-on-filladapt-mode)
  (add-hook 'c-mode-common-hook
	    (lambda ()
	      (when (featurep 'filladapt)
		(c-setup-filladapt))))
  )

;; so-long
;; Improve performance for long lines
(use-package so-long
  :straight nil
  :load-path "lisp/"
  :config
  (so-long-enable))

(defun code-section (start end)
    "Section delimiters for comment"
    (interactive "r")
    (if (use-region-p)
        (let ((regionp (buffer-substring start end)))
	  (delete-region start end)
          (insert "..:: " regionp " ::.."))
      (insert "..:: SECTION ::.."))
    )

(defun code-subsection (start end)
    "Subsection delimiters for comment"
    (interactive "r")
    (if (use-region-p)
        (let ((regionp (buffer-substring start end)))
	  (delete-region start end)
          (insert ">> " regionp " <<"))
      (insert ">> SUBSECTION <<"))
    )

(defun code-subsubsection (start end)
    "Subsubsection delimiters for comment"
    (interactive "r")
    (if (use-region-p)
        (let ((regionp (buffer-substring start end)))
	  (delete-region start end)
          (insert "@ " regionp " @"))
      (insert "@ SUBSUBSECTION @"))
    )

;; Julia integration
;; --------
;; Emacs major mode for an interactive Julia shell
;; To install, run in ~/.emacs.d/
;;   git clone https://github.com/dmalyuta/julia-shell-mode
;; --------
(add-to-list 'load-path "~/.emacs.d/julia-shell-mode")
;; (require 'julia-shell)
;; (defun my-julia-mode-hooks ()
;;   (require 'julia-shell-mode))
;; (add-hook 'julia-mode-hook 'my-julia-mode-hooks)
;; (define-key julia-mode-map (kbd "C-c C-c") 'julia-shell-run-region-or-line)
;; (define-key julia-mode-map (kbd "C-c C-s") 'julia-shell-save-and-go)
;; (define-key inferior-julia-shell-mode-map (kbd "C-u C-c C-l") 'latexsub-or-indent)

;;;;;;;;;;;;;;;;; PERSONAL PACKAGES

(use-package c-block-comment
  ;; automatically type C-style block comments
  ;; Use M-; to insert /* */ around the point
  ;; Use M-j to go to next line in a multi-line block comment
  :straight nil
  :load-path "lisp/")

(use-package setup-helm
  ;; https://github.com/tuhdo/emacs-c-ide-demo/blob/master/custom/setup-helm.el
  :straight nil
  :load-path "lisp/")

(use-package buffer-move
  ;; swap buffers between windows
  :straight nil
  :load-path "lisp/"
  :bind
  (("S-M-<up>" . buf-move-up)
   ("S-M-<down>" . buf-move-down)
   ("S-M-<left>" . buf-move-left)
   ("S-M-<right>" . buf-move-right)))

(use-package my-cursor-commands
  ;; paragraph scrolling key bindings
  :straight nil
  :load-path "lisp/"
  :bind
  (("M-p" . xah-backward-block)
   ("M-n" . xah-forward-block)))

(use-package delete-without-copy
  ;;  delete words without putting them into the kill ring
  :straight nil
  :load-path "lisp/"
  :bind
  (("M-d" . my-forward-delete-word)
   ("M-<backspace>" . my-backward-delete-word)))

(use-package sr-speedbar
  ;; makes Speedbar (source file browser) show in the current frame as a new window
  :straight nil
  :load-path "lisp/"
  :bind
  ("C-c C-b" . sr-speedbar-toggle))

(use-package duplicate-line
  ;; duplicate current line
  :straight nil
  :load-path "lisp/"
  :bind
  (("C-c d" . duplicate-current-line-or-region)))

(use-package cmake-mode
  ;; major-mode for editing CMake sources
  :straight nil
  :load-path "lisp/"
  :config
  (add-to-list 'auto-mode-alist '("\\.cmake\\'" . cmake-mode))
  (autoload 'cmake-mode "lisp/cmake-mode.el" t))

(use-package ros-cmake-mode
  ;; major-mode for editing CMakeLists.txt files with ROS-style indentation
  :straight nil
  :load-path "lisp/"
  :config
  (add-to-list 'auto-mode-alist '("CMakeLists\\.txt\\'" . ros-cmake-mode))
  (autoload 'ros-cmake-mode "lisp/ros-cmake-mode.el" t))

(use-package gdb-setup
  ;; customize GDB layout and functionality
  :straight nil
  :load-path "lisp/")

(use-package window-resize
  :straight nil
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
  :straight nil
  :load-path "lisp/"
  :demand
  :config
  ;; For when started with emacs or emacs -nw rather than emacs --daemon
  (add-hook 'after-init-hook 'my-normal-startup-pick-color-theme)
  ;; For when started as emacs --daemon
  (add-hook 'after-make-frame-functions 'pick-color-theme)
  )

;;;;;;;;;;;;;;;;; OTHER STUFF

;; Python Imenu
;; From: https://steelkiwi.com/blog/emacs-configuration-working-python/
;; https://stackoverflow.com/a/21656063/4605946
(defun my-python-imenu ()
  (interactive)
  (let ((python-imenu (imenu--generic-function imenu-generic-expression)))
    (append python-imenu)))
(defun my-python-imenu-hooks ()
  (interactive)
  (setq imenu-generic-expression
	'(("Function" "^[[:blank:]]*def \\(.*\\).*(.*$" 1)
	  ("Class" "^class \\(.*\\).*:$" 1)))
  (setq imenu-create-index-function 'my-python-imenu)
  ;; Rescan the buffer as contents are added
  (setq imenu-auto-rescan t)
  )
(add-hook 'python-mode-hook 'my-python-imenu-hooks)
(add-hook 'lsp-mode-hook 'my-python-imenu-hooks)

;; Enable semantic only for specific modes
;; E.g. Python semantic doesn't work for type-annotated code, so don't use it
;; (see https://stackoverflow.com/a/56618164/4605946)
(setq semantic-new-buffer-setup-functions
      '((c-mode                . semantic-default-c-setup)
        (c++-mode              . semantic-default-c-setup)
        (srecode-template-mode . srecode-template-setup-parser)
        (texinfo-mode          . semantic-default-texi-setup)
        ;; etc.
        ;; (makefile-automake-mode . semantic-default-make-setup)
        ;; (makefile-mode         . semantic-default-make-setup)
        ;; (makefile-gmake-mode   . semantic-default-make-setup)
        ))

;; Delete trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; No blinking for cursor
(blink-cursor-mode 0)

;; Activate nlinum-mode (line numbers on the left)
(global-set-key (kbd "C-x n l") 'nlinum-mode)

;; Fira Code font
;; Default font and font size
;; (add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-12"))
;; Fira Code
(use-package fira-code-mode
  ;; List of ligatures to turn off
  :ensure t
  :custom (fira-code-mode-disabled-ligatures
	   '("[]" "#{" "#(" "#_" "#_(" "x" "&&"))
  :hook prog-mode ;; Enables fira-code-mode automatically for programming major modes
  )
(add-hook
 'after-init-hook
 (lambda ()
   (set-face-attribute 'default nil
		       :family "Fira Code"
		       :height 110
		       :weight 'normal
		       :width 'normal)

   )
 )

;; Zoom in every buffer
(defadvice text-scale-increase (around all-buffers (arg) activate)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      ad-do-it)))

;; Try to speed up font-lock mode speed (causes laggy scrolling in C++)
(setq font-lock-support-mode 'jit-lock-mode)
(setq jit-lock-stealth-time 16
      jit-lock-defer-contextually t
      jit-lock-contextually nil
      jit-lock-stealth-nice 0.5)
(setq-default font-lock-multiline t)
(setq font-lock-maximum-decoration 1)

;; Some stuff that didn't work out, from this answer:
;; https://emacs.stackexchange.com/questions/5351/optimizing-font-lock-performance
;; (defun my-font-lock-function (start end)
;; "Set faces for font-lock between START and END.")
;; (defun my-font-lock-matcher (limit)
;;     (my-font-lock-function (point) limit)
;;    nil)
;; (setq font-lock-defaults
;;   (list
;;     ;; Note that the face specified here doesn't matter since
;;     ;; my-font-lock-matcher always returns nil and sets the face on
;;     ;; its own.
;;     `(my-font-lock-matcher (1 font-lock-keyword-face nil))))

;; Stop highlighting current line
(add-hook 'python-mode-hook
  (lambda () (setq hl-line-mode nil)))
(add-hook 'emacs-lisp-mode-hook
  (lambda () (setq hl-line-mode nil)))
(add-hook 'c-mode-common-hook
  (lambda () (setq hl-line-mode nil)))

;; ;; CEDET tools
;; (require 'cc-mode)
;; (require 'semantic)
;; (global-semanticdb-minor-mode 1)
;; (global-semantic-idle-scheduler-mode 1)
;; (semantic-mode 1)

;; No auto-newline for C/C++
(add-hook 'c-mode-common-hook
	  (lambda ()
	    (setq c-auto-newline nil)
	    ))

;; Kill buffer and window
(global-set-key (kbd "C-x w") 'kill-buffer-and-window)

;; Go back to previous buffer
(global-set-key (kbd "C-c <prior>") 'previous-buffer) ;; prior: page up
(global-set-key (kbd "C-c <next>") 'next-buffer) ;; prior: page down

;; Run terminal from current buffer
(defun run-terminator-here ()
  (interactive "@")
  (shell-command (concat "terminator > /dev/null 2>&1 & disown") nil nil))
(global-set-key (kbd "C-c t r") 'run-terminator-here)

;; Python shell
;; Make sure you are running IPython 5.7.0, because buggy for later versions
;; ``$ pip install -U ipython==5.7.0``
;;
;; Fix autoreload problem (answer by DmitrySemenov at
;; https://tinyurl.com/ipython-autoreload):
;;
;; I found a better solution that needs no emacs config: simply do
;;
;; $ ipython profile create
;;
;; that should create ipython profile in
;; $HOME/.ipython/profile_default/ipython_config.py
;; then put the following inside
;; ```
;; c = get_config()
;; c.TerminalInteractiveShell.editor = 'emacsclient'
;; c.InteractiveShellApp.extensions = [
;;      'autoreload'
;; ]
;;
;; c.InteractiveShellApp.exec_lines = []
;; c.InteractiveShellApp.exec_lines.append('%load_ext autoreload')
;; c.InteractiveShellApp.exec_lines.append('%autoreload 2')
;; ```
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i --simple-prompt --pprint")
;; Printout what file is being run
(defun ipython-print-runfile ()
  (interactive)
  "Print in comint buffer the file that is being executed"
  ;;(message "%s" (buffer-file-name))
  ;;(python-shell-send-file buffer-file-name 'nil 'nil 'nil "Hello world")
  ;; The -i in `%run -i` maintains the current Python namespace, which
  ;; means that current variables in the workspace are seen by the script
  ;; See: https://stackoverflow.com/a/13300775/4605946
  (python-shell-send-string (concat "print('%run -i " buffer-file-name "')"))
  (python-shell-send-string "print('Running... ')")
  (python-shell-send-string (concat "%run -i " buffer-file-name))
  (python-shell-send-string "print('done')")
  )
(defun ipython-print-region ()
  (interactive)
  "Print in comint buffer the region that is being executed"
  ;; (message "%s" (buffer-substring (region-beginning) (region-end)))
  (python-shell-send-string (concat "print(\"\"\"<<<Running region>>>\n" (buffer-substring (region-beginning) (region-end)) "\"\"\")"))
  (python-shell-send-string (buffer-substring (region-beginning) (region-end)))
  ;; (python-shell-send-string "print('Running... ')")
  ;; (python-shell-send-string (concat "%run " buffer-file-name))
  ;; (python-shell-send-string "print('done')")
  )
(add-hook 'python-mode-hook
	  (lambda ()
	    (define-key python-mode-map (kbd "C-c C-l")
	      'ipython-print-runfile)
	    (define-key python-mode-map (kbd "C-c C-r")
	      'ipython-print-region)))

;; Automatically reload files when they change on disk
;; (global-auto-revert-mode)

;; Source: http://www.emacswiki.org/emacs-en/download/misc-cmds.el
(defun revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive)
  (revert-buffer :ignore-auto :noconfirm))
(global-set-key (kbd "C-c b r") 'revert-buffer-no-confirm)

;; Turn off Abbrev mode
(setq-default abbrev-mode nil)

;; Print current date
(defun insert-current-date () (interactive)
       (insert (shell-command-to-string "echo -n $(date \"+%Y/%m/%d %H:%M:%S\")")))

;; Keep a history of recent files
(recentf-mode 1)
(setq-default recent-save-file "~/.emacs.d/recentf")

;; C files do not indent extern "C" { <HERE> }
(add-hook 'c-mode-common-hook
	  (lambda()
	    (c-set-offset 'inextern-lang 0)))

;; copy pwd
(defun show-file-name ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (kill-new (buffer-file-name))
  (message (buffer-file-name)))

;; fill-region hotkey
(global-set-key (kbd "M-r") 'fill-region)

;; Fill to column
(defun fill-to-end (char)
  (interactive "cFill Character:")
  (save-excursion
    (end-of-line)
    (while (< (current-column) fill-column)
      (message "column = %d, fill-colummn = %d" (current-column) fill-column)
      (insert-char char))
    (while (> (current-column) fill-column)
      (delete-backward-char 1))
    ))
(global-set-key (kbd "C-c f c") 'fill-to-end)
(global-set-key (kbd "C-c f *") (lambda () (interactive) (
							  fill-to-end ?*
								      )))

;; Semantic be quiet
;; by selectively suppressing messages, see the full technique at
;; https://superuser.com/a/1025827/512940 (answer by Bernard Hurley)
(defun suppress-messages (old-fun &rest args)
  (cl-flet ((silence (&rest args1) (ignore)))
    (advice-add 'message :around #'silence)
    (unwind-protect
         (apply old-fun args)
      (advice-remove 'message #'silence))))
(defun who-called-me? (old-fun format &rest args)
  (let ((trace nil) (n 1) (frame nil))
      (while (setf frame (backtrace-frame n))
        (setf n     (1+ n)
              trace (cons (cadr frame) trace)) )
      (apply old-fun (concat "<<%S>>\n" format) (cons trace args))))
;; (advice-add 'message :around #'who-called-me?)
(advice-add 'semantic-idle-scheduler-function :around #'suppress-messages)

;; Unbind Pesky Sleep Button
(global-unset-key [(control z)])
(global-unset-key [(control x)(control z)])

;; Fix laggy point (cursor)
;; In particular: cursor freezing when moving down (next-line) for a while
;; https://emacs.stackexchange.com/questions/28736/emacs-pointcursor-movement-lag
(setq auto-window-vscroll nil)
(setq doom-modeline-enable-word-count nil)

;; Doxymacs
;; (add-to-list 'load-path "~/.emacs.d/doxymacs/")
;; (require 'doxymacs)
;; (add-hook 'c-mode-common-hook'doxymacs-mode)
;; (defun my-doxymacs-font-lock-hook ()
;;   (if (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
;;       (doxymacs-font-lock)))
;; (add-hook 'font-lock-mode-hook 'my-doxymacs-font-lock-hook)

;; enable clipboard in emacs
(xterm-mouse-mode t)
(mouse-wheel-mode t)
(setq x-select-enable-clipboard t)

;; Kill TRAMP stuff
(global-set-key (kbd "C-c t k") 'tramp-cleanup-all-connections)

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
;; make sure window movement keys are not captured by shell
(add-hook 'matlab-shell-mode-hook
	  (lambda ()
	    (define-key matlab-shell-mode-map (kbd "C-<up>") 'nil)
	    (define-key matlab-shell-mode-map (kbd "C-<down>") 'nil)
	    (define-key matlab-shell-mode-map (kbd "C-<left>") 'nil)
	    (define-key matlab-shell-mode-map (kbd "C-<right>") 'nil)))
;; make sure window movement keys are not captured by terminal
(add-hook 'term-mode-hook
	  (lambda ()
	    (define-key term-raw-map (kbd "C-<up>") 'nil)
	    (define-key term-raw-map (kbd "C-<down>") 'nil)
	    (define-key term-raw-map (kbd "C-<left>") 'nil)
	    (define-key term-raw-map (kbd "C-<right>") 'nil)))
;; Turn off line wrap in ansi term
(add-hook 'term-mode-hook
	  (lambda ()
	    (setq truncate-lines t)))

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
;; (add-hook 'c-mode-common-hook (lambda () (setq-local comment-auto-fill-only-comments t)))

;; /*  */ style comments with C-x M-; in c++-mode
;; (defun my-block-comment ()
;;   (interactive)
;;   (let ((comment-start "/* ")
;; 	  (comment-end " */"))
;;     (comment-dwim nil)))
;; (add-hook 'c++-mode-hook (lambda () (local-set-key (kbd "C-x M-;") 'my-block-comment)))

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
  (add-hook 'find-file-hook 'my-xml-schema-hook))

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
 '(Man-notify-method 'pushy)
 '(TeX-PDF-mode t)
 '(TeX-source-correlate-method 'synctex)
 '(TeX-source-correlate-mode t)
 '(TeX-source-correlate-start-server t)
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#282c34" "#CC9393" "#7F9F7F" "#F0DFAF" "#8CD0D3" "#DC8CC3" "#93E0E3" "#DCDCCC"])
 '(company-begin-commands nil)
 '(ecb-auto-expand-tag-tree 'expand-spec)
 '(ecb-auto-expand-tag-tree-collapse-other nil)
 '(ecb-highlight-tag-with-point 'highlight-scroll)
 '(ecb-highlight-tag-with-point-delay 0.25)
 '(ecb-layout-window-sizes
   '(("left11"
      (ecb-methods-buffer-name 0.17901234567901234 . 0.7)
      (ecb-history-buffer-name 0.17901234567901234 . 0.275))))
 '(ecb-options-version "2.50")
 '(fci-rule-color "#383838")
 '(flycheck-flake8rc "~/setup.cfg")
 '(flymake-fringe-indicator-position nil)
 '(lsp-diagnostic-clean-after-change t)
 '(lsp-progress-via-spinner nil)
 '(lsp-pyls-plugins-autopep8-enabled t)
 '(lsp-pyls-plugins-flake8-config "~/setup.cfg")
 '(lsp-pyls-plugins-flake8-enabled t)
 '(lsp-pyls-plugins-flake8-exclude '("E231"))
 '(lsp-pyls-plugins-pycodestyle-enabled nil)
 '(lsp-pyls-plugins-pycodestyle-ignore '("E231"))
 '(matlab-fill-fudge 0)
 '(matlab-fill-fudge-hard-maximum 80)
 '(matlab-indent-function-body nil)
 '(matlab-shell-command-switches '("-nodesktop -nosplash"))
 '(matlab-show-mlint-warnings t)
 '(matlab-show-periodic-code-details-flag nil)
 '(mlint-programs '("/usr/local/MATLAB/R2019b/bin/glnxa64/mlint"))
 '(mmm-submode-decoration-level 0)
 '(nrepl-message-colors
   '("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3"))
 '(org-format-latex-options
   '(:foreground "yellow" :background default :scale 1.3 :html-foreground "Black" :html-background "Transparent" :html-scale 1.0 :matchers
		 ("begin" "$1" "$" "$$" "\\(" "\\[")))
 '(package-selected-packages
   '(helm-cscope lsp-pyright fira-code-mode matlab-emacs matlab-mode fast-scroll company-box default-text-scale company-graphviz-dot graphviz-dot-mode gnuplot mmm-mode helm-company org-bullets workgroups helm-lsp lsp-ui which-key dap-mode autopair julia-mode julia-emacs unfill sage-mode sage-shell-mode minimap helm-ag plantuml-mode elpy hl-todo undo-tree zoom-frm move-text magit fill-column-indicator flymd markdown-mode bash-completion workgroups2 fuzzy ess-R-data-view ess auto-compile rainbow-mode ecb realgud wgrep-helm wgrep multiple-cursors srefactor nyan-mode google-c-style yaml-mode mic-paren pdf-tools auctex helm-projectile projectile helm-ros helm-gtags helm-swoop helm company-irony-c-headers company-irony flycheck-irony irony company-shell company-quickhelp company flycheck dired+ neotree doom-themes rainbow-delimiters use-package))
 '(pdf-view-midnight-colors '("#DCDCCC" . "#383838"))
 '(safe-local-variable-values
   '((lsp-python-ms-python-executable . "/home/danylo/anaconda3/envs/py385/bin/python")
     (eval progn
	   (add-hook 'LaTeX-mode-hook
		     (lambda nil
		       (LaTeX-add-environments "Definition")
		       (LaTeX-add-environments "Theorem")
		       (LaTeX-add-environments "Fact")
		       (LaTeX-add-environments "Example")
		       (LaTeX-add-environments "Method")
		       (LaTeX-add-environments "Proof")
		       (LaTeX-add-environments "VeryImportantStuff"))))))
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   '((20 . "#BC8383")
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
     (360 . "#DC8CC3")))
 '(vc-annotate-very-old-color "#DC8CC3"))

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
 '(column-enforce-face ((t (:background "yellow" :foreground "black" :slant oblique))))
 '(ecb-default-highlight-face ((((class color) (background dark)) (:background "yellow" :foreground "black"))))
 '(ecb-tag-header-face ((((class color) (background dark)) (:background "yellow" :foreground "black"))))
 '(flymake-error ((t (:background "red" :slant italic))))
 '(flymake-warning ((t (:background "orange" :slant italic))))
 '(helm-ff-directory ((t (:foreground "yellow" :weight bold)))))
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
