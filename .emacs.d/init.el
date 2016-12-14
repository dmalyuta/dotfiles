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

;;;;;;;;;;;;;;;;; EMACS BUILT-IN

(use-package windmove
  ;; move cursor between windows
  :bind
  (("C-s-b" . windmove-left)
   ("C-s-f" . windmove-right)
   ("C-s-p" . windmove-up)
   ("C-s-n" . windmove-down)))

(use-package ansi-term
  :config
  ;; fix the problem of moving cursor left/right being captured by emacs instead
  ;; of underlying terminal, leading to jumbling when jumping words then editing
  ;; the middle of a command
  (defun term-send-Cright () (interactive) (term-send-raw-string "\e[1;5C"))
  (defun term-send-Cleft  () (interactive) (term-send-raw-string "\e[1;5D"))
  (define-key term-raw-map (kbd "C-<right>")      'term-send-Cright)
  (define-key term-raw-map (kbd "C-<left>")       'term-send-Cleft))

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
  (require 'semantic)
  (semantic-mode 1)
  (global-semanticdb-minor-mode 1)
  (global-semantic-idle-scheduler-mode 1))

;;;;;;;;;;;;;;;;; MELPA PACKAGES

(use-package zenburn-theme
  ;; theme
  :ensure t
  :config
  (load-theme 'zenburn t))

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
  (add-hook 'c++-mode-hook 'flycheck-mode)
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
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (setq company-backends (delete 'company-semantic company-backends))
  ;; use C++11 by default
  (require 'cc-mode)
  (set 'company-clang-arguments (list "-std=c++11")))

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
    :config
    (setq company-idle-delay 0))
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
  (add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages))

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
  ;; Linux Mint:
  ;;  cd ~/Downloads/
  ;;  wget -O gnu_global.tar.gz "http://tamacom.com/global/global-6.5.5.tar.gz" # or whatever the latest version is
  ;;  tar -zxvf gnu_global.tar.gz && rm gnu_global.tar.gz
  ;;  cd global-6.5.5/ # or whatever version you downloaded with wget
  ;;  sudo ./configure && sudo make && sudo make install
  ;;  rm -R ~/Downloads/gnu_global && rm -R
  ;;  cd ~/Downloads/ && rm -R global-6.5.5/
  :ensure t
  :init
  (add-hook 'c-mode-common-hook 'helm-gtags-mode)
  :bind
  (:map c-mode-base-map
	("C-c g a" . helm-gtags-tags-in-this-function)
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

(use-package projectile
  ;; project interaction library offering tools to operate on a project level
  ;; Useful commands:
  ;;  C-c p p : the first thing to run when starting to work in a project
  ;;  C-c p f : find file in current project
  ;;  C-c p g : find file based on context (can place cursor on header file, executing command will take you to header file)
  ;;  C-c p d : find directory in current project
  ;;  C-c p a : switch between files with same name but different extensions (e.g. .cpp and .h)
  ;;  C-c p i : rebuild cache (otherwise if new files made, old cache causes them to not be seen)
  ;;  C-c p f, C-SPC, C-f: create a virtual Dired buffer from files selected with C-SPC with C-c p f
  ;;  C-c p b : switch to open buffer of file/directory that belongs to current project
  ;;  C-c p s g : find a word/string in project files  (using grep), autofill with symbol at point
  :ensure t
  :config
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
  ;; Linux Mint:
  ;;  sudo apt-get install zathura # PDF output viewer to use
  :ensure auctex
  :init
  (setq TeX-source-correlate-method 'synctex) ;; use SyncTeX for forward/backward search between source/PDF output
  :config
  (add-hook 'LaTeX-mode-hook 'flyspell-mode)
  (add-hook 'LaTeX-mode-hook 'flyspell-buffer)
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
  (setq reftex-plug-into-AUCTeX t))

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

(use-package nyan-mode
  ;; adorable cat showing progress in document
  :ensure t
  :config
  (nyan-mode 1)
  (setq nyan-wavy-trail nil)
  (setq nyan-animate-nyancat t))

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
  ;;    4) "C-c C-e" to save changes to "C-c C-k" to discard changes of "C-x C-q" to exit wgrep
  ;;       and prompt whether to save changes
  :ensure t
  :config
  (use-package wgrep-helm
    :ensure t
    :config
    (require 'wgrep)
    (require 'wgrep-helm)
    (setq wgrep-auto-save-buffer t)))

;;;;;;;;;;;;;;;;; PERSONAL PACKAGES

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

(setq make-backup-files nil) ;; stop creating backup ~ files
(toggle-scroll-bar -1) ;; no scrollbar
;;(tool-bar-mode -1) ;; no toolbar
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(setq initial-scratch-message ";; Change The World")
(setq ring-bell-function 'ignore) ;; Disable system sounds
(global-set-key (kbd "C-x C-;") 'comment-region) ;; Comment region in Lisp
(add-to-list 'auto-mode-alist '("\\.launch?\\'" . xml-mode)) ;; xml-mode for .launch files (xml)
(define-key global-map (kbd "RET") 'newline-and-indent)
(put 'downcase-region 'disabled nil)
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

;;;;;;;;;;;;;;;;; NOT USED

(use-package solarized-theme
  ;; theme
  :ensure t
  :disabled
  :config
  (load-theme 'solarized-dark t))
