;;; init.el --- Emacs configuration. -*- lexical-binding: t; -*-
;;
;; Author: Danylo Malyuta
;;
;; Keywords: init
;;
;; This file is not part of GNU Emacs
;;
;;; Commentary:
;;
;;  To byte-compile: [M-x byte-compile-init-dir]
;;
;;; Code:

(defun danylo/run-gui-conditional-code (user-func)
  "Run USER-FUNC after the frame is create, passing to it a boolean that
says whether this Emacs session is running as GUI (vs in a terminal)."
  (if (not (daemonp))
      ;; Apply the function immediately
      (funcall user-func)
    ;; Apply the function on creating a new emacsclient frame.
    (add-hook 'server-after-make-frame-hook user-func)))

(defun danylo/run-after-idle-interval (dt)
  "Compute a time interval that makes the idle timer execute function after
DT seconds (floating point)."
  (+ (let ((idle-time (current-idle-time)))
       (if idle-time
           (float-time idle-time)
         0))
     dt))

;; C-g is used by Emacs at a very low-level to quit out of running code, and it
;; is also used to execite `keyboard-quit'. See
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Quitting.html. This
;; duplicate use causes issues when the two ways that C-g behaves interact. For
;; this reason, I leverage a key translation map to use:
;;   - C-/ in GUI;
;;   - C-_ in terminal (unfortunately this is what C-/ maps to in terminal
;;     emulators);
;; instead for quitting out of user code. C-g continues to assume its duplicate
;; role, but the user shouldn't use it.
(defun danylo/init-keyboard-quit-key ()
  (if (display-graphic-p)
      (define-key key-translation-map (kbd "C-/") (kbd "C-g"))
    (define-key key-translation-map (kbd "C-_") (kbd "C-g"))))
(danylo/run-gui-conditional-code #'danylo/init-keyboard-quit-key)
(define-key key-translation-map (kbd "C-g") (kbd "<f20>"))
(global-unset-key (kbd "<f20>"))

;; Frame size
(setq default-frame-alist
      (append default-frame-alist '((height . 50) (width . 100))))

;; Use GNU tar when using OSX.
(defconst is-osx (string-equal system-type "darwin")
  "Boolean if running under Mac OS X.")
(when is-osx
  (setenv "PATH" "/opt/homebrew/bin/:$PATH" t))

;; Also us Alt as meta key in OSX.
(when is-osx
  (setq mac-option-modifier 'meta))

;; No garbage collection at startup
(setq gc-cons-threshold most-positive-fixnum)

;; Increase the amount of data which Emacs reads from the process
;; Source: https://emacs-lsp.github.io/lsp-mode/page/performance/
(setq read-process-output-max (* 1024 1024 10)) ;; 10mb

;; Suppress general warnings
(setq warning-suppress-types
      '((server)
        (comp)))

;; Debug the appearance of certain startup messages.
;; (setq debug-on-message "Function .+ is already compiled")
;; (setq debug-on-message "Waiting for .*")

;; Paste the following at the desired location to early-out from the init file.
;; (with-current-buffer " *load*" (goto-char (point-max)))

;; Error and signaling debugging.
;; (defun danylo/signal-handler (err &rest data)
;;   (when (eq err 'quit)
;;     (when (> (mc/num-cursors) 1)
;;       (message "signaled: %s" err)
;;       (debug))))
;; (add-hook 'after-init-hook
;;           (lambda () (setq signal-hook-function #'danylo/signal-handler)))

;; Native compilation settings
(when (fboundp 'native-compile-async)
  (setq comp-deferred-compilation t
        warning-suppress-log-types '((comp)))
  (setq warning-suppress-types
        (append warning-suppress-types
                '((comp)))))

;;; ..:: Package management ::..

(defun package-read-from-string (str)
  "Read a Lisp expression from STR.
Signal an error if the entire string was not used.
NOTE: I have no idea why re-defining this function here works, but not having
it causes a package-desc nil error."
  (pcase-let ((`(,expr . ,offset) (read-from-string str)))
    (condition-case ()
        ;; The call to `ignore' suppresses a compiler warning.
        (progn (ignore (read-from-string str offset))
               (error "Can't read whole string"))
      (end-of-file expr))))

(require 'package)
(setq package-enable-at-startup nil)
(setq package-check-signature nil)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

;; Initialize packages.
;;
;; The helm-related definitions get around "undefined" errors in
;; helm-bufler-autoloads which uses some Helm functionality before it gets
;; loaded.
(defun helm-make-source (&rest _) nil)
(setq helm-type-buffer-actions nil)
(package-initialize)
(makunbound 'helm-type-buffer-actions)
(fmakunbound 'helm-make-source)
(makunbound 'helm-bufler-source)

;; Install use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Install Quelpa
(setq quelpa-update-melpa-p nil
      quelpa-checkout-melpa-p nil
      quelpa-upgrade-p nil
      quelpa-verbose nil
      quelpa-build-verbose nil)
(unless (package-installed-p 'quelpa)
  (with-temp-buffer
    (url-insert-file-contents
     "https://raw.githubusercontent.com/quelpa/quelpa/master/quelpa.el")
    (eval-buffer)
    (quelpa-self-upgrade)))

;; Quelpa recipe format: https://github.com/melpa/melpa#recipe-format
(quelpa
 '(quelpa-use-package
   :fetcher git
   :url "https://github.com/quelpa/quelpa-use-package.git"))
(require 'quelpa-use-package)

(setq use-package-always-ensure t)
(quelpa-use-package-activate-advice)

;;; ..:: Customization variables ::..

(eval-and-compile
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

  (defconst danylo/desktop-save-dir
    `,(danylo/make-path "desktop-save/")
    "Location where desktop (i.e. Emacs state) files are saved."))

(use-package danylo-custom-variables
  ;; Customization variables for init file.
  :load-path danylo/emacs-custom-lisp-dir)

;;; ..:: Keybinding management ::..

(require 'bind-key)

(use-package general
  ;; https://github.com/noctuid/general.el
  ;; More convenient key definitions in emacs
  )

(use-package hydra
  ;; https://github.com/abo-abo/hydra
  ;; Make Emacs bindings that stick around
  :config
  (require 'hydra)
  (define-key hydra-base-map (kbd "q") 'keyboard-quit))

(general-define-key
 :keymaps '(emacs-lisp-mode-map lisp-interaction-mode-map)
 "C-M-r" 'eval-region
 "C-M-d" 'eval-defun
 "C-M-x" 'eval-buffer
 )

(advice-add 'eval-region
            :after (lambda (&rest _)
                     (message "Evaluated region")
                     (deactivate-mark)))
(advice-add 'eval-defun
            :after (lambda (&rest _) (message "Evaluated defun")))
(advice-add 'eval-buffer
            :after (lambda (&rest _) (message "Evaluated buffer")))

;; No email composition.
(global-unset-key (kbd "C-x m"))

;;; ..:: Remote development ::..

(use-package tramp
  ;; https://elpa.gnu.org/packages/tramp.html
  ;; Transparent Remote Access, Multiple Protocol
  ;;
  ;; Performance advice:
  ;; - https://www.reddit.com/r/emacs/comments/1jatdse/im_trying_to_troubleshoot_extremely_slow_tramp/
  :init
  (setq tramp-verbose 3
        tramp-chunksize 2000
        tramp-default-method "rsync"
        tramp-archive-enabled nil
        tramp-backup-directory-alist nil
        tramp-auto-save-directory "~/.emacs.d/tramp-autosave"
        enable-remote-dir-locals t
        tramp-inhibit-errors-if-setting-file-attributes-fail t
        projectile-file-exists-remote-cache-expire most-positive-fixnum)
  ;;;;;;; Apply recommendations from
  ;;;;;;; https://coredumped.dev/2025/06/18/making-tramp-go-brrrr./
  (setq remote-file-name-inhibit-locks t
        tramp-use-scp-direct-remote-copying t
        remote-file-name-inhibit-auto-save-visited t)
  (setq tramp-copy-size-limit (* 1024 1024) ;; 1MB
        tramp-verbose 2)
  ;; Enable direct async
  (connection-local-set-profile-variables
   'remote-direct-async-process
   '((tramp-direct-async-process . t)))
  (connection-local-set-profiles
   '(:application tramp :protocol "scp")
   'remote-direct-async-process)
  (setq magit-tramp-pipe-stty-settings 'pty)
  ;; Fix remote compile
  (with-eval-after-load 'tramp
    (with-eval-after-load 'compile
      (remove-hook 'compilation-mode-hook
                   #'tramp-compile-disable-ssh-controlmaster-options)))
  ;; Don't use version control on remote files (speeds up opening)
  (setq vc-ignore-dir-regexp
        (format "\\(%s\\)\\|\\(%s\\)"
                vc-ignore-dir-regexp
                tramp-file-name-regexp))
  ;; Enable persistent connections
  (setq tramp-connection-timeout 10) ;; faster failure if host unreachable
  (setq dired-async-mode t)
  :config
  (setq tramp-remote-path '(tramp-own-remote-path)) ;; disables GIO/glib features
  )

;;;; (start patch) Don't keep remote files in recent file list.
(defun danylo/recentf-keep-default-predicate (file)
  "Return non-nil if FILE should be kept in the recent list.
Remote files are ommitted."
  (and (recentf-keep-default-predicate file)
       (not (string-match vc-ignore-dir-regexp (file-name-directory file)))))
(setq recentf-keep '(danylo/recentf-keep-default-predicate))
;;;; (end patch)

;;;; (start patch) Memoize TRAMP outputs.
(defun memoize-remote (key cache orig-fn &rest args)
  "Memoize a value if the key is a remote path."
  (if (and key
           (file-remote-p key))
      (if-let ((current (assoc key (symbol-value cache))))
          (cdr current)
        (let ((current (apply orig-fn args)))
          (set cache (cons (cons key current) (symbol-value cache)))
          current))
    (apply orig-fn args)))

;; Memoize current project
(defvar project-current-cache nil)
(defun memoize-project-current (orig &optional prompt directory)
  (memoize-remote (or directory
                      project-current-directory-override
                      default-directory)
                  'project-current-cache orig prompt directory))
(advice-add 'project-current :around #'memoize-project-current)

;; Memoize magit top level
(defvar magit-toplevel-cache nil)
(defun memoize-magit-toplevel (orig &optional directory)
  (memoize-remote (or directory default-directory)
                  'magit-toplevel-cache orig directory))
(advice-add 'magit-toplevel :around #'memoize-magit-toplevel)

;; Memoize file attributes.
(defvar file-readable-cache nil)
(defvar file-exists-cache nil)
(defvar file-symlink-cache nil)
(defvar file-attributes-cache nil)
(defun memoize-file-readable (orig &optional filename)
  ;; (memoize-remote filename 'file-readable-cache orig filename)
  t
  )
(defun memoize-file-exists (orig &optional filename)
  (memoize-remote filename 'file-exists-cache orig filename))
(defun memoize-file-symlink (orig &optional filename)
  (memoize-remote filename 'file-symlink-cache orig filename))
(defun memoize-file-attributes (orig &optional filename)
  (memoize-remote filename 'file-attributes-cache orig filename))
(advice-add 'tramp-sh-handle-file-readable-p :around #'memoize-file-readable)
(advice-add 'tramp-sh-handle-file-exists-p :around #'memoize-file-exists)
(advice-add 'tramp-sh-handle-file-symlink-p :around #'memoize-file-symlink)
(advice-add 'tramp-sh-handle-file-attributes-p :around #'memoize-file-attributes)

;; memoize vc-git-root
(defvar vc-git-root-cache nil)
(defun memoize-vc-git-root (orig file)
  (let ((value (memoize-remote (file-name-directory file) 'vc-git-root-cache orig file)))
    ;; sometimes vc-git-root returns nil even when there is a root there
    (when (null (cdr (car vc-git-root-cache)))
      (setq vc-git-root-cache (cdr vc-git-root-cache)))
    value))
(advice-add 'vc-git-root :around #'memoize-vc-git-root)

(defun danylo/clear-tramp-caches ()
  "Clear all caches for TRAMP."
  (interactive)
  (setq project-current-cache nil
        magit-toplevel-cache nil
        file-readable-cache nil
        file-exists-cache nil
        file-symlink-cache nil
        file-attributes-cache nil
        vc-git-root-cache nil
        )
  (danylo/print-in-minibuffer "Cleared TRAMP cache"))
;;;; (end patch)

(use-package clipetty
  ;; https://github.com/spudlyo/clipetty
  ;; Manipulate the system (clip)board with (e)macs from a (tty).
  :ensure t
  :hook (after-init . global-clipetty-mode)
  :init
  (setenv "SSH_TTY"))

;;; ..:: General helper functions ::..

(use-package s
  ;; https://github.com/magnars/s.el
  ;; The long lost Emacs string manipulation library.
  )

(use-package all-the-icons
  ;; https://github.com/domtronn/all-the-icons.el
  ;; Pretty icons
  :config
  (when (and (display-graphic-p)
             (not (member "all-the-icons" (font-family-list))))
    (all-the-icons-install-fonts t))
  ;; Fix MATLAB icon
  (when (display-graphic-p)
    (setq all-the-icons-icon-alist
          (add-to-list 'all-the-icons-icon-alist
                       '("\\.m$" all-the-icons-fileicon "matlab"
                         :face all-the-icons-orange))))
  )

(defun danylo/fancy-icon (icon-lib icon-family icon &optional fg)
  "Icon with proper formatting for minibuffer"
  (unless fg
    (setq fg `,(face-attribute 'default :foreground)))
  (propertize (funcall icon-lib icon)
              'face `(:family ,(funcall icon-family)
                              :height 0.95
                              :foreground ,fg)
              'display '(raise -0.05)))

(defun danylo/fa-icon (icon &optional fg)
  "Fontawesome icon with proper formatting."
  (danylo/fancy-icon 'all-the-icons-faicon 'all-the-icons-faicon-family
                     icon fg))

(defun danylo/nerd-fa-icon (icon &optional fg)
  "Nerd-icons fontawesome icon with proper formatting."
  (danylo/fancy-icon 'nerd-icons-faicon 'nerd-icons-faicon-family
                     icon fg))

(defun danylo/nerd-md-icon (icon &optional fg)
  "Nerd-icons material design icon with proper formatting."
  (danylo/fancy-icon 'nerd-icons-mdicon 'nerd-icons-mdicon-family
                     icon fg))

(defun danylo/octicon (icon &optional fg)
  "Octicon with proper formatting."
  (danylo/fancy-icon 'all-the-icons-octicon 'all-the-icons-octicon-family
                     icon fg))

(defun danylo/print-in-minibuffer (str &optional ifempty)
  "Echo STR in the minibuffer."
  (with-selected-window (minibuffer-window)
    (when (or (not ifempty)
              ;; Check that the minibuffer does not have text already
              (not (current-message)))
      (setq cursor-type nil)
      ;; Loop through the string and replace the foreground color of each
      ;; character to a faded color
      (setq danylo/~i 0)
      (while (< danylo/~i (length str))
        (setq danylo/face~props (get-text-property danylo/~i 'face str))
        (setq danylo/fg~pos (cl-position :foreground danylo/face~props))
        (if danylo/fg~pos
            (setf (nth (1+ danylo/fg~pos) danylo/face~props) `,danylo/faded)
          (setq danylo/face~props (append danylo/face~props
                                          `(:foreground ,danylo/faded))))
        (put-text-property danylo/~i (1+ danylo/~i) 'face danylo/face~props str)
        (setq danylo/~i (1+ danylo/~i)))
      ;; Print the string to minibuffer
      (message "%s" str)
      (setq cursor-type t))))

(use-package ts
  ;; https://github.com/alphapapa/ts.el
  ;; Emacs timestamp and date-time library
  ;; **Loading this just because unpackaged needs it**
  :quelpa (ts :fetcher github
              :repo "alphapapa/ts.el"))

(defvar danylo/use-package-always-ensure
  "Memory variable for use-package-always-ensure")

(use-package unpackaged
  ;; https://github.com/alphapapa/unpackaged.el
  ;; A collection of useful Emacs Lisp code
  ;;
  ;; Use:
  ;;   unpackaged/quelpa-use-package-upgrade: select a (use-package ...) that
  ;;       uses Quelpa and completely upgrade it to the latest online version
  ;;       (replaces all associated directories)
  :disabled
  :ensure nil
  :after (general hydra ts esxml)
  :init (setq danylo/use-package-always-ensure use-package-always-ensure
              use-package-always-ensure nil)
  :quelpa ((unpackaged :fetcher github
                       :repo "alphapapa/unpackaged.el"))
  :config
  (setq use-package-always-ensure danylo/use-package-always-ensure))

;;; ..:: Debugging ::..

(defconst danylo/core-minor-modes
  '(tooltip-mode
    global-eldoc-mode
    electric-indent-mode
    mouse-wheel-mode
    tool-bar-mode
    menu-bar-mode
    file-name-shadow-mode
    global-font-lock-mode
    font-lock-mode
    blink-cursor-mode
    auto-composition-mode
    auto-encryption-mode
    auto-compression-mode
    line-number-mode
    transient-mark-mode)
  "Core Emacs minor modes that are used when I run Emacs with
`emacs -Q`.")

(defconst danylo/whitelist-minor-modes
  '(global-so-long-mode
    default-text-scale-mode
    rainbow-mode
    filladapt-mode
    rainbow-delimiters-mode
    projectile-mode
    window-numbering-mode
    global-hl-todo-mode
    hl-todo-mode
    shell-dirtrack-mode
    gcmh-mode
    recentf-mode
    electric-pair-mode
    helm-mode
    google-this-mode
    danylo-text-font-lock-mode
    danylo-prog-font-lock-mode)
  "Emacs minor modes that I whitelist for being performant enough.")

(defvar danylo/mode-list-cache '()
  "Cache of my active minor modes.")

(defun danylo/disable-my-minor-modes ()
  "Dsiable all non-standard minor modes."
  (interactive)
  (mapc
   (lambda (mode-symbol)
     (condition-case nil
         (when (and (symbolp mode-symbol)
                    (symbol-value mode-symbol)
                    (functionp mode-symbol)
                    (not (member mode-symbol danylo/core-minor-modes))
                    (not (member mode-symbol danylo/whitelist-minor-modes)))
           (add-to-list 'danylo/mode-list-cache mode-symbol)
           (message "Disabling: %s" mode-symbol)
           (ignore-errors (funcall mode-symbol -1)))
       (error nil)))
   minor-mode-list))

(defun danylo/enable-my-minor-modes ()
  "Re-enable my non-standard minor modes "
  (interactive)
  (let ((remaining-modes '()))
    (mapc
     (lambda (mode-symbol)
       (progn
         (message "Enabling: %s" mode-symbol)
         (ignore-errors (funcall mode-symbol +1))))
     danylo/mode-list-cache)
    (setq danylo/mode-list-cache remaining-modes)))

;;;;; Profiling

(defvar danylo/profiler-running nil
  "Whether the CPU profiler is currently running.")

(defun danylo/toggle-profiler ()
  "Toggle Emacs CPU profiler. Start on first call, stop and report on second."
  (interactive)
  (if danylo/profiler-running
      (progn
        (profiler-stop)
        (message "Profiler stopped")
        (setq danylo/profiler-running nil)
        (profiler-report-profile (profiler-cpu-profile))
        (profiler-reset))
    (profiler-start 'cpu)
    (setq danylo/profiler-running t)
    (message "Profiler started (CPU only)")))

(general-define-key
 "C-c P" 'danylo/toggle-profiler)

;;; ..:: Garbage collection ::..

;; 100MB of garbage collection space once running
(add-hook 'after-init-hook
          (lambda () (setq gc-cons-threshold danylo/gc-cons-threshold)))

(defun danylo/gc-message ()
  "Garbage collection message."
  (when (and danylo/gc-collect-print
             (not (active-minibuffer-window)))
    (let ((message-log-max nil))
      ;; Print "<TRASH_ICON> GC"
      (danylo/print-in-minibuffer (format "%s GC" (danylo/fa-icon "trash")) t)
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

;;; ..:: Tree-sitter ::..
;;
;; Getting started:
;; https://www.masteringemacs.org/article/how-to-get-started-tree-sitter
;;
;; Check ABI version: (treesit-library-abi-version)

;; Jit font lock settings. The function `jit-lock-function' has a big effect on
;; performance, as I perceived especially in the fontification of narrowed
;; buffers (such as via `narrow-to-defun').
(setq jit-lock-defer-time (/ 1.0 30.0))

;;;; (start patch) Fontify any buffer on load.
(defcustom danylo/buffers-to-skip-auto-fontify
  '("\\*.*\\*"
    ".*magit.*"
    ".*Profiler-Report.*")
  "List of buffers to not auto-fontify on opening.")
(defun danylo/is-narrowed ()
  "Check if current buffer is narrowed."
  (or (buffer-narrowed-p)
      (fancy-narrow-active-p)
      (bound-and-true-p dired-narrow-mode)))
(defun danylo/fontify (&rest args)
  "Fontify visible part of the buffer."
  (run-with-idle-timer
   (danylo/run-after-idle-interval 0.05) nil
   (lambda ()
     (save-excursion
       (unless
           (or (cl-loop
                for regexp in danylo/buffers-to-skip-auto-fontify
                thereis (string-match regexp (buffer-name)))
               (danylo/is-narrowed)
               (derived-mode-p 'image-mode))
         (font-lock-fontify-region (window-start) (window-end)))))))
(add-hook 'window-configuration-change-hook 'danylo/fontify)
;;;; (end patch)

(use-package treesit-auto
  ;; https://github.com/renzmann/treesit-auto
  ;; Automatic installation, usage, and fallback for tree-sitter major modes.
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;; Maximally colorful font coloring.
(setq treesit-font-lock-level 4)

(add-hook 'c-mode-common-hook
          (lambda ()
            (c-ts-common-comment-setup)
            (setq-local treesit--indent-verbose danylo/treesit-verbose)))

(use-package c-ts-mode
  ;; Tree-sitter support for C and C++.
  :if (treesit-language-available-p 'c)
  :custom
  (c-ts-mode-indent-offset 4)
  (c-ts-mode-indent-style #'danylo/cpp-ts-style)
  )

;;;; Remap major modes for tree-sitter.
(setq major-mode-remap-alist
      '((c++-mode . c++-ts-mode)
        (c-mode . c-ts-mode)
        (c-or-c++-mode . c-or-c++-ts-mode)
        (python-mode . python-ts-mode)
        (sh-mode . bash-ts-mode)
        (yaml-mode . yaml-ts-mode)))
(defun c++-ts-mode-call-hook () (run-hooks 'c++-mode-hook) (run-hooks 'c-mode-common-hook))
(add-hook 'c++-ts-mode-hook #'c++-ts-mode-call-hook)
(defun c-ts-mode-call-hook () (run-hooks 'c-mode-hook) (run-hooks 'c-mode-common-hook))
(add-hook 'c-ts-mode-hook #'c-ts-mode-call-hook)
(defun python-ts-mode-call-hook () (run-hooks 'python-mode-hook))
(add-hook 'python-ts-mode-hook #'python-ts-mode-call-hook)
(defun bash-ts-mode-call-hook () (run-hooks 'sh-mode-hook))
(add-hook 'bash-ts-mode-hook #'bash-ts-mode-call-hook)
(defun yaml-ts-mode-call-hook () (run-hooks 'yaml-mode-hook))
(add-hook 'yaml-ts-mode-hook #'yaml-ts-mode-call-hook)

;;;; (start patch) Turn off traditional syntax propertization in chosen modes.
(defconst danylo/disable-syntax-propertize-modes
  '(c++-ts-mode c-ts-mode python-ts-mode)
  "List of modes where `syntax-propertize-function` should be disabled.")

;; Add the hook to all specified modes
(dolist (mode danylo/disable-syntax-propertize-modes)
  (let ((hook (intern (format "%s-hook" mode))))
    (add-hook
     hook
     (lambda () (setq-local syntax-propertize-function nil)))))
;;;; (end patch)

(use-package fringe-helper
  ;; https://github.com/nschum/fringe-helper.el
  ;; Helper functions for fringe bitmaps.
  )

(use-package treesit-fold
  ;; https://github.com/emacs-tree-sitter/treesit-fold
  ;; Code-folding using tree-sitter.
  :after (fringe-helper)
  :quelpa ((treesit-fold :fetcher github
                         :repo "emacs-tree-sitter/treesit-fold"))
  :bind (("C-c f t" . treesit-fold-toggle)
         ("C-c f a" . treesit-fold-open-all)
         ("C-c f c" . treesit-fold-close-all)
         ("C-c f i" . treesit-fold-indicators-mode))
  :init
  (setq treesit-fold-line-count-show t
        treesit-fold-indicators-priority -10)
  (global-treesit-fold-mode)
  (global-treesit-fold-indicators-mode)
  )

;;;; (start patch) Do not use treesit-fold indicators in remote buffers, in
;;;;               causes a major slowdown.
(defun danylo/treesit-fold-usable-mode-p (orig-fun &rest args)
  (unless (file-remote-p buffer-file-name)
    (apply orig-fun args)))
(advice-add 'treesit-fold-usable-mode-p
            :around #'danylo/treesit-fold-usable-mode-p)
;;;; (end patch)

;;;; (start patch) Throttle the call rate to render treesit-fold fringe indicators.
(defvar danylo/fold-indicators-refresh-idle-delay 0.25)
(defvar danylo/fold-indicators-refresh-timer nil)
(defvar danylo/last-num-windows nil)
(defvar danylo/render-treesit-fold-indicators nil)
(defun danylo/decide-to-render-treesit-fold-indicators ()
  "Set danylo/render-treesit-fold-indicators to t if the number of windows has
changed, and to nil otherwise."
  (setq danylo/render-treesit-fold-indicators nil)
  (let ((num-windows (count-windows)))
    (unless (eq num-windows danylo/last-num-windows)
      (setq danylo/last-num-windows num-windows)
      (setq danylo/render-treesit-fold-indicators t))))
(defun danylo/debounced-treesit-fold-indicators-refresh (orig-fun &rest args)
  (when danylo/fold-indicators-refresh-timer
    (cancel-timer danylo/fold-indicators-refresh-timer))
  (if (eq danylo/fold-indicators-refresh-idle-delay 0)
      (apply orig-fun args)
    (setq
     danylo/fold-indicators-refresh-timer
     (run-with-idle-timer
      (danylo/run-after-idle-interval
       danylo/fold-indicators-refresh-idle-delay) nil
      (lambda (orig-fun &rest args)
        (apply orig-fun args)) orig-fun args))))
(defun danylo/treesit-fold-indicators-render ()
  "Render indicators for all visible windows in the current frame after a window split."
  (interactive)
  (when danylo/render-treesit-fold-indicators
    (run-with-idle-timer
     (danylo/run-after-idle-interval 0.01) nil
     (lambda ()
       (let ((danylo/fold-indicators-refresh-idle-delay 0))
         (treesit-fold-indicators--size-change))))))
;;;; (end patch)

;;;; (start patch) Ensure that the fringe background matches the window
;;;;               background.
(defvar-local danylo/treesit-fold-fringe-remap-cookie nil
  "Cookie for the remapped `treesit-fold-fringe-face` in this window.")

(defun danylo/update-treesit-fold-fringe-face-for-window (window)
  "Update `treesit-fold-fringe-face` in WINDOW to match its background."
  (with-selected-window window
    (let* ((bg (face-background 'solaire-default-face nil t))
           (buffer (window-buffer window)))
      (with-current-buffer buffer
        ;; Remove old remap if it exists
        (when danylo/treesit-fold-fringe-remap-cookie
          (face-remap-remove-relative danylo/treesit-fold-fringe-remap-cookie))
        ;; Add new remap
        (setq danylo/treesit-fold-fringe-remap-cookie
              (face-remap-add-relative 'treesit-fold-fringe-face
                                       `(:background ,bg)))))))
(defun danylo/update-all-treesit-fold-fringe-faces ()
  "Update `treesit-fold-fringe-face` for all visible windows."
  (when danylo/render-treesit-fold-indicators
    (dolist (window (window-list))
      (danylo/update-treesit-fold-fringe-face-for-window window))))
;;;; (end patch)

(with-eval-after-load 'treesit-fold
  (advice-add 'treesit-fold-indicators-refresh
              :around #'danylo/debounced-treesit-fold-indicators-refresh)
  (add-hook 'window-configuration-change-hook
            #'danylo/decide-to-render-treesit-fold-indicators 1)
  (add-hook 'window-configuration-change-hook
            #'danylo/treesit-fold-indicators-render 1)
  (add-hook 'window-configuration-change-hook
            #'danylo/update-all-treesit-fold-fringe-faces 1))

;;; ..:: General usability ::..

;; Remove GUI elements
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(tooltip-mode 0)
(blink-cursor-mode 0)

;; X-event timeout
(setq x-wait-for-event-timeout nil)

;; Default directory
(setq default-directory "~/")

(defmacro without-yes-or-no (auto-answer &rest body)
  "Override `yes-or-no-p' and `y-or-n-p' to not to prompt for input and
return AUTO-ANSWER. This solution is taken from
https://stackoverflow.com/a/59509250."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) ,auto-answer))
             ((symbol-function 'y-or-n-p) (lambda (&rest _) ,auto-answer)))
     ,@body))

;; Directory to save state into via `desktop-save'
(require 'desktop)
(add-to-list 'desktop-path danylo/desktop-save-dir)
(defun danylo/desktop-save ()
  "Save Emacs state to file, so that later we can start Emacs again and
reload it."
  (interactive)
  (danylo/print-in-minibuffer
   (condition-case err
       (progn
         ;; `desktop-save' will ask if desktop should be saved if the current
         ;; Emacs session has not loaded a desktop session. We always want to
         ;; answer NO, hence we replace the yes-or-no-p function to return NIL
         ;; for just this one call.
         (without-yes-or-no nil (desktop-save danylo/desktop-save-dir))
         (format "%s Saved desktop" (danylo/nerd-fa-icon "nf-fa-save")))
     (error
      (format "%s Did not save desktop (%s)"
              (danylo/nerd-fa-icon "nf-fa-skull_crossbones")
              (error-message-string err))))))

;; Remove warnings like: ad-handle-definition: ‘ansi-term’ got redefined
(setq ad-redefinition-action 'accept)

;; Comint unset C-up C-down
(global-unset-key (kbd "C-<up>"))
(global-unset-key (kbd "C-<down>"))

;; Move by paragraph
(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-p") 'backward-paragraph)

;; Move cursor to top/middle/bottom
(global-set-key (kbd "C-;") 'move-to-window-line-top-bottom)

;; Switch between previous and this buffer.
(defvar danylo/toggle-between-buffers-value -1
  "Switching variable that determines if we go to the previous or the next buffer.")

(defun danylo/toggle-between-buffers ()
  "Toggle between this and the last visited buffer in the current window."
  (interactive)
  (let* ((prev-view (nth 0 (window-prev-buffers)))
         (prev-buffer (nth 0 prev-view))
         (prev-window-start (nth 1 prev-view))
         (prev-pos (nth 2 prev-view)))
    (switch-to-buffer prev-buffer)
    (set-window-start nil prev-window-start)
    (goto-char prev-pos)))

(defhydra hydra-toggle-between-buffers (global-map "C-x")
  "Flip to last buffer"
  ("/" danylo/toggle-between-buffers))
(hydra-set-property 'hydra-toggle-between-buffers :verbosity 0)

(use-package danylo-cursor-motion
  ;; Move cursor either one line at a time, or in large steps.
  :load-path danylo/emacs-custom-lisp-dir
  :bind (("C-p" . danylo/cursor-up-smart)
         ("C-n" . danylo/cursor-down-smart)))

;; Show column number
(setq-default column-number-mode t)

;; Do not resize minibuffer for long path on file save

(defun danylo/save-buffer (orig-fun &rest args)
  "Pretty print save buffer, preserver height of minibuffer."
  (save-excursion
    (let ((message-truncate-lines t)
          (this-file-name (file-name-nondirectory (buffer-file-name))))
      (let ((inhibit-message t))
        (apply orig-fun args))
      (danylo/print-in-minibuffer
       (format "%s Saved %s" (danylo/fa-icon "database") this-file-name))
      )))
(advice-add 'save-buffer :around #'danylo/save-buffer)

;;;; Minibuffer yes/no query confirm with enter
(fset 'yes-or-no-p 'y-or-n-p)
(defun danylo/y-or-n-p-with-return (orig-func &rest args)
  "Confirm 'yes' query in minibuffer using RET."
  (let ((query-replace-map (copy-keymap query-replace-map)))
    (define-key query-replace-map (kbd "RET") 'act)
    (apply orig-func args)))
(advice-add 'y-or-n-p :around #'danylo/y-or-n-p-with-return)

;; Better start screen
(use-package dashboard
  ;; https://github.com/emacs-dashboard/emacs-dashboard
  ;; An extensible emacs startup screen showing you what’s most important
  :diminish dashboard-mode
  :bind (:map dashboard-mode-map
              ("<up>" . nil)
              ("<down>" . nil)
              ("<left>" . nil)
              ("<right>" . nil)
              )
  :init
  (setq dashboard-banner-logo-title "Change the world, step by step"
        dashboard-startup-banner 'logo
        dashboard-items '((recents  . 5) (projects . 5) (bookmarks . 5))
        dashboard-center-content t
        dashboard-set-heading-icons t
        dashboard-set-file-icons t
        dashboard-set-footer nil
        )
  :config
  (dashboard-setup-startup-hook))

;; Shortcuts to open common useful buffers.
(general-define-key
 "C-c o s" 'scratch-buffer
 "C-c o d" 'dashboard-open
 "C-c o m" (lambda () (interactive) (switch-to-buffer "*Messages*"))
 "C-c o i" (lambda () (interactive) (find-file user-init-file)))

;; A welcome message after startup
(defun display-startup-echo-area-message ())
(add-hook 'after-init-hook (lambda () (message "Welcome back")))

(setq inhibit-splash-screen t)
(setq initial-scratch-message ";; Change The World")

;; Keep init.el clean from custom-set-variable
(setq-default custom-file (danylo/make-path ".custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;; Zoom in/out

(defvar danylo/default-font-size nil
  "The default font size when Emacs opens.")

(defun danylo/reset-font-size ()
  (interactive)
  (defvar danylo/current-font-size)
  (defvar danylo/text-increment)
  (setq danylo/current-font-size (face-attribute 'default :height)
        danylo/text-increment (- danylo/default-font-size
                                 danylo/current-font-size))
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
  (add-hook 'emacs-startup-hook
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

;;;; Parentheses pairing

(electric-pair-mode 1)
(setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)

(defvar-local danylo/electric-pair-timer nil
  "Timer object for electric pair insertion.")

(defun danylo/electric-pair-post-self-insert-function (orig-fun &rest args)
  "Throttled electric-pair-post-self-insert-function. Basically,
turn off electric-pair-mode if the user is flooding the input
with characters."
  (unless danylo/electric-pair-timer
    (apply orig-fun args)
    (unless (or (region-active-p)
                (bound-and-true-p multiple-cursors-mode))
      (electric-pair-mode 0)
      (setq danylo/electric-pair-timer
            (run-with-idle-timer
             (danylo/run-after-idle-interval 0.1) nil
             (lambda ()
               (setq danylo/electric-pair-timer nil)
               (electric-pair-mode 1)
               ))))))
(advice-add 'electric-pair-post-self-insert-function
            :around #'danylo/electric-pair-post-self-insert-function)

;;;; Better electric indentation

(defvar-local danylo/do-electric-indent t)
(defun danylo/electric-indent-jit (char)
  "Turn off electric indent temporarily during a rapid-fire
sequence of newlines."
  (if (char-equal char ?\n)
      (if danylo/do-electric-indent
          (progn
            (run-with-timer
             0.025 nil
             (lambda ()
               (setq danylo/do-electric-indent nil)
               (run-with-idle-timer
                (danylo/run-after-idle-interval 0.05) nil
                (lambda () (setq danylo/do-electric-indent t)))))
            ;; Do indent (normal mode)
            t)
        ;; Don't indent (rapid fire mode)
        'no-indent)
    ;; Let other functions decide
    nil))

(add-hook 'electric-indent-functions 'danylo/electric-indent-jit)

;; Make region selection faster
;; https://emacs.stackexchange.com/a/61764/13661
(setq select-active-regions nil)

;; Turn off the transient mark model for highlighting region
(setq transient-mark-mode t)

(use-package highlight
  ;; https://www.emacswiki.org/emacs/HighlightLibrary
  ;; Provides commands to highlight text
  )

(use-package expand-region
  ;; https://github.com/magnars/expand-region.el
  ;; Increase selected region by semantic units
  :bind (("M-=" . er/expand-region))
  :config
  (add-hook
   'find-file-hook
   (lambda ()
     ;; If the current mode has treesitter support, do not use er/mark-defun
     ;; expansion. I found that this can lead to slowness in some modes, such
     ;; as bash-ts-mode.
     (when (treesit-parser-list)
       (setq-local er/try-expand-list
                   (delete 'er/mark-defun er/try-expand-list)))))
  )

;;;; (start patch) Remove blinking highlight artifacts when expanding region.
(defun danylo/er/expand-region (orig-fun &rest args)
  "Expand region, but making sure that there is no blinking artifacts."
  (let ((out))
    (if (and (eq major-mode 'org-mode) org-modern-mode)
        (progn
          (remove-from-invisibility-spec 'org-modern)
          (setq out (apply orig-fun args))
          (add-to-invisibility-spec 'org-modern))
      (setq out (apply orig-fun args)))
    out))
(advice-add 'er/expand-region :around #'danylo/er/expand-region)
;;;; (end patch)

;; Turn off Abbrev mode
(setq-default abbrev-mode nil)

;; Unbind Pesky Sleep Button
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

;; Disable system sounds
(setq ring-bell-function 'ignore)

;;;; Working with buffers

;; Update buffers when files on disk change. Do not do this for remote files.
(require 'autorevert)
(setq auto-revert-verbose nil
      ;; Don't poll for file changes when file system notifications are
      ;; available.
      auto-revert-avoid-polling auto-revert-use-notify
      revert-without-query '(".*"))
(defun danylo/auto-revert-file-not-remote ()
  "Turn on auto-revert-mode for file-visiting buffers of local files."
  (if (and buffer-file-name
           (not (file-remote-p buffer-file-name)))
      (auto-revert-mode)))
(add-hook 'find-file-hook #'danylo/auto-revert-file-not-remote)

(defun danylo/revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive)
  (revert-buffer :ignore-auto :noconfirm))

(defun danylo/revert-all-buffers ()
  "Refreshes all buffers if they have been modified outside of this Emacs
session."
  (interactive)
  (let ((externally-modified-buffers '()))
    (dolist (buffer (buffer-list))
      (let ((file-name (buffer-file-name buffer)))
        (when (and file-name
                   (file-exists-p file-name)
                   (not (verify-visited-file-modtime buffer)))
          (add-to-list 'externally-modified-buffers buffer))))
    (let ((iter 0)
          (num-buffers (length externally-modified-buffers)))
      (while (< iter num-buffers)
        (let ((buffer (nth iter externally-modified-buffers)))
          (danylo/print-in-minibuffer
           (format "%s Refreshing [%d/%d, %d%%]: %s"
                   (danylo/nerd-fa-icon "nf-fa-refresh")
                   (1+ iter)
                   num-buffers
                   (/ (* (1+ iter) 100) num-buffers)
                   (buffer-file-name buffer)))
          (set-buffer buffer)
          (revert-buffer t t t))
        (setq iter (1+ iter)))
      (if (> num-buffers 0)
          (danylo/print-in-minibuffer
           (format "Refreshed %d buffers" num-buffers))
        (danylo/print-in-minibuffer "No buffers require refreshing")))))

(general-define-key
 "C-c b r" 'danylo/revert-buffer-no-confirm
 "C-x w" 'kill-buffer-and-window
 "C-c r" 'rename-buffer
 "C-c <prior>" 'previous-buffer  ; prior: page up
 "C-c <next>" 'next-buffer)      ; next: page down

;; Enable clipboard in emacs
(defun danylo/init-mouse ()
  (unless (display-graphic-p)
    (condition-case err
        (gpm-mouse-mode t)
      (error
       (message "%s" (error-message-string err))
       (message
        "%s gpm-mouse-mode not activated, falling back to xterm-mouse-mode"
        (danylo/fa-icon "exclamation-triangle"))
       (xterm-mouse-mode t)))
    ))
(danylo/run-gui-conditional-code #'danylo/init-mouse)

(mouse-wheel-mode t)
(setq select-enable-clipboard t)

;;;; Backing up

;; Make sure Emacs doesn't break hard links
(setq backup-by-copying t)

;; Backup behaviour: store everything in single location
(setq backup-directory-alist (list (cons "." danylo/emacs-backup-dir)))
(setq auto-save-file-name-transforms `((".*" ,danylo/emacs-backup-dir t)))
(setq auto-save-default nil) ;; Disable auto-save (I save myself)

(defun danylo/byte-compile-init-dir-no-init (&optional force-recompile)
  "Byte-compile Emacs config except for the init.el file itself. If
FORCE-RECOMPILE is t then recompile all the files (default is nil)."
  (interactive)
  (byte-recompile-file (danylo/make-path "early-init.el") force-recompile 0)
  (byte-recompile-directory danylo/emacs-custom-lisp-dir 0 force-recompile))

(defun danylo/byte-compile-init-dir (&optional force-recompile)
  "Byte-compile Emacs config. If FORCE-RECOMPILE is t then recompile all
the files (default is nil)."
  (interactive)
  (byte-recompile-file (danylo/make-path "init.el") force-recompile 0)
  (danylo/byte-compile-init-dir-no-init force-recompile))

;;;; Dired (directory listing)

(use-package dired+
  ;; Better directory browser
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

(use-package dired-subtree
  ;; https://github.com/Fuco1/dired-hacks
  ;; Insert subdirectories in a tree-like fashion
  :bind (:map dired-mode-map
              ("." . dired-subtree-insert)
              ("," . dired-subtree-remove)))

;;;; (start patch) Modify dired-subtree regex for checking if thing under point
;;;;               is a directory. The default version rigidly requires two
;;;;               space indentation, which is not always the case.
(defun danylo/dired-subtree--dired-line-is-directory-or-link-p (&rest _)
  "Return non-nil if line under point is a directory or symlink."
  (save-excursion
    (beginning-of-line)
    (looking-at "[ ]*[dl]")))
(advice-add 'dired-subtree--dired-line-is-directory-or-link-p
            :around #'danylo/dired-subtree--dired-line-is-directory-or-link-p)
;;;; (end patch)

(use-package cc-dired-sort-by
  ;; https://github.com/kickingvegas/cclisp
  ;; Emacs configuration files for Charles Choi
  :quelpa (cc-dired-sort-by
           :fetcher url
           :url "https://raw.githubusercontent.com/kickingvegas/cclisp/e5397b5b08d9b96633a2cf13b230e71e02697b3f/cc-dired-sort-by.el")
  :bind (:map dired-mode-map
              ("s" . cc/dired-sort-by)))

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

(defun danylo/close-frame-or-kill-emacs (arg)
  "Delete frame or kill Emacs if there are only one frame.
When called with C-u prefix, force kill Emacs (all frames).
Source: https://emacs.stackexchange.com/a/50834/13661"
  (interactive "P")
  (if arg
      (progn
        ;; Force kill Emacs (all frames), traditional C-x C-c style
        (danylo/desktop-save)
        (save-buffers-kill-terminal))
    ;; Kill only the current frame, if there is more than one
    (if (> (length (frame-list)) 1)
        ;; Just delete the current frame
        (condition-case nil (delete-frame) (error (save-buffers-kill-terminal)))
      ;; Traditional C-x C-c (kill Emacs)
      (danylo/desktop-save)
      (save-buffers-kill-terminal))))

(defun danylo/make-new-frame ()
  "Make a new frame, and make sure it behaves like I want."
  (interactive)
  (make-frame-command))

(general-define-key
 "C-x C-c" nil ;; Kind of dangerous (can quit Emacs by accident)
 "C-x C-c C-c" 'danylo/close-frame-or-kill-emacs
 "C-x n f" 'danylo/make-new-frame)

;; Do not ask whether to kill active processes
(setq confirm-kill-processes nil)

;;;; Scrolling behaviour

(use-package danylo-scroll
  ;; My custom scroll commands.
  :load-path danylo/emacs-custom-lisp-dir
  :bind (("C-v" . danylo/scroll-down)
         ("M-v" . danylo/scroll-up))
  :custom
  (scroll-error-top-bottom t)
  (scroll-preserve-screen-position 'always))

;;;; (start patch) Hydra triggers the `window-scroll-functions', but
;;;;               we already are updating the scrollbar via the
;;;;               `window-configuration-change-hook' since we are
;;;;               scrolling. Avoid the double-call by temporarily removing the
;;;;               scrollbar update from the `window-scroll-functions'.
(defun danylo/debounce-scrollbar ()
  (remove-hook 'window-scroll-functions 'yascroll:after-window-scroll t))
(defun danylo/undo-debounce-scrollbar ()
  (add-hook 'window-scroll-functions 'yascroll:after-window-scroll nil t))
;;;; (end patch)

(defhydra hydra-scroll-row
  (:pre danylo/debounce-scrollbar :post danylo/undo-debounce-scrollbar)
  "Move up/rown one row keeping the cursor at the same position"
  ("n" danylo/scroll-row-down "down")
  ("p" danylo/scroll-row-up "up"))

(general-define-key
 "C-q" 'hydra-scroll-row/body)

;;; Mouse wheel scroll behaviour
(mouse-wheel-mode 't)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

;;; Pixel-precision scrolling
(pixel-scroll-precision-mode 1)

;;;; eval-buffer default directory fix

(defun danylo/eval-buffer-maintain-dir (orig-fun &rest args)
  "Maintain default-directory when eval-buffer."
  (let ((current-dir default-directory))
    (apply orig-fun args)
    (setq-local default-directory current-dir)))
(advice-add 'eval-buffer :around #'danylo/eval-buffer-maintain-dir)

;;;; Rename and move files

(defun danylo/rename-file (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME.
Source: http://steve.yegge.googlepages.com/my-dot-emacs-file"
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

(defun danylo/move-file (dir)
  "Moves both current buffer and file it's visiting to DIR.
Source: http://steve.yegge.googlepages.com/my-dot-emacs-file"
  (interactive "DNew directory: ")
  (let* ((name (buffer-name))
         (filename (buffer-file-name))
         (dir
          (if (string-match dir "\\(?:/\\|\\\\)$")
              (substring dir 0 -1) dir))
         (newname (concat dir "/" name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (progn
        (copy-file filename newname 1)
        (delete-file filename)
        (set-visited-file-name newname)
        (set-buffer-modified-p nil)
        t))))

(defun danylo/copy-file-absolute-path (&optional arg)
  "Copy the absolute path of the file to clipboard."
  (interactive)
  (kill-new (buffer-file-name)))

(defun danylo/show-file-absolute-path ()
  "SHow the absolute path of the file."
  (interactive)
  (message (buffer-file-name)))

(use-package revert-buffer-all
  ;; Revert all buffers after external changes have been made.
  )

;; World clock.
(setq world-clock-list
      '(("America/Los_Angeles" "Seattle")
        ("Europe/Kiev" "Odessa")
        ("Australia/Sydney" "Sydney")
        ("Europe/Zurich" "Zurich")))

;; Get the message to be printed for C-g.
(put 'quit 'error-message
     (propertize
      (format "%s Quit" (danylo/fa-icon "stop-circle"))
      'face `(:family ,(all-the-icons-faicon-family)
                      :foreground danylo/faded)))

;;;; Mark ring

(setq mark-ring-max 6
      global-mark-ring-max 8
      set-mark-command-repeat-pop t)

(use-package back-button
  ;; https://github.com/rolandwalker/back-button
  ;; Visual navigation through mark rings in Emacs.
  :init (back-button-mode 1)
  :config
  (defhydra hydra-jump-history (global-map "M-g")
    "Move forward/back in current buffer"
    ("." back-button-local-forward "→")
    ("," back-button-local-backward "←")
    ("C-." back-button-global-forward "global →")
    ("C-," back-button-global-backward "global ←"))
  )

(use-package consult
  ;; https://github.com/minad/consult
  ;; Consulting completing-read.
  :bind (("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g g" . consult-goto-line)))
(global-unset-key (kbd "M-g M-g"))

;;;; Improve performance of long lines.
;; (global-so-long-mode 1)

;;;; No-op command for keys that we want to disable.

(defun danylo/disabled-command ()
  "A no-op function that messages user that they are trying to use a
disabled command."
  (interactive)
  (let ((message-log-max nil))
    (danylo/print-in-minibuffer
     (format "%s Disabled command"
             (danylo/nerd-fa-icon "nf-fa-skull_crossbones")))))

(general-define-key
 "<f20>" 'danylo/disabled-command)

;;;; Delete without pushing content to the kill ring.
;;;; Adapted from: http://xahlee.info/emacs/emacs/emacs_kill-ring.html

(defun danylo/delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument, do this that many times.
This command does not push text to `kill-ring'."
  (interactive "p")
  (delete-region
   (point)
   (progn
     (forward-word arg)
     (point))))

(defun danylo/backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument, do this that many times.
This command does not push text to `kill-ring'."
  (interactive "p")
  (danylo/delete-word (- arg)))

(defun danylo/delete-line ()
  "Delete text from current position to end of line char.
This command does not push text to `kill-ring'."
  (interactive)
  (delete-region
   (point)
   (progn (end-of-line 1) (point)))
  (delete-char 1))

(defun danylo/delete-line-backward ()
  "Delete text between the beginning of the line to the cursor position.
This command does not push text to `kill-ring'."
  (interactive)
  (let (p1 p2)
    (setq p1 (point))
    (beginning-of-line 1)
    (setq p2 (point))
    (delete-region p1 p2)))

(global-set-key (kbd "C-S-k") 'danylo/delete-line-backward)
(global-set-key (kbd "C-k") 'danylo/delete-line)
(global-set-key (kbd "M-d") 'danylo/delete-word)
(global-set-key (kbd "M-DEL") 'danylo/backward-delete-word)
(global-set-key (kbd "C-DEL") 'danylo/backward-delete-word)
(global-set-key (kbd "C-<backspace>") 'danylo/backward-delete-word)

;; Clear minibuffer contents
(general-define-key
 :keymaps 'minibuffer-local-map
 "C-q" 'helm-delete-minibuffer-contents)

;;;; (start patch) Apply filtering to messages.
(defun danylo/message (orig-fun &rest args)
  (condition-case nil
      (when (and args (listp args) (> (length args) 0))
        (let* ((desired-msg (apply 'format args))
               (inhibit-message
                (or
                 inhibit-message
                 (string-equal
                  desired-msg
                  "LSP :: Error from the Language Server: Request cancelled because the document was modified (Unknown error)"))))
          (apply orig-fun args)))
    (error
     ;; If an error happens, suppress it and just show the message.
     (apply orig-fun args))))
(advice-add 'message :around #'danylo/message)
;;;; (end patch)

(general-define-key
 :keymaps 'prog-mode-map
 "M-RET" 'default-indent-new-line)

;;; ..:: Searching ::..

;;;; Neotree: view the source code file tree

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

(defun danylo/neotree-jump ()
  "Smart open neotree side window."
  (interactive)
  (danylo/side-window-jump 'neotree-show neo-buffer-name))

(use-package neotree
  ;; https://github.com/jaypei/emacs-neotree
  ;; A emacs tree plugin like NerdTree for Vim.
  :after (all-the-icons)
  :bind (("C-c t n" . danylo/neotree-jump)
         :map neotree-mode-map
         ("C-c n c" . neotree-change-root)
         ("C-c n p" . neotree-copy-node)
         ("C-c n d" . neotree-delete-node)
         ("C-c n r" . neotree-rename-node))
  :init (setq neo-theme (if (display-graphic-p) 'icons 'arrow)
              neo-window-width danylo/side-window-width
              neo-smart-open t
              neo-show-hidden-files t
              neo-autorefresh nil)
  (add-hook 'after-init-hook (lambda () (require 'neotree))))

;;;; Imenu

;; Add line numbers to the matched results

(defun danylo/match-string-no-properties-with-linum (orig-fun &rest args)
  "Same as match-string-no-properties except that it appends the
line number to the string."
  (declare (side-effect-free t))
  (let* ((num (nth 0 args))
         (match-start-pos (match-beginning num))
         (total-num-lines (int-to-string
                           (length
                            (int-to-string
                             (count-lines (point-min) (point-max))))))
         (fmt `,(concat "%-" total-num-lines "d %s"))
         output)
    (if match-start-pos
        (setq
         output
         (format fmt
                 (line-number-at-pos match-start-pos)
                 (buffer-substring-no-properties
                  match-start-pos (match-end num))))
      output)))

(defun danylo/imenu-show-linum (orig-fun &rest args)
  "Match imenu to show line numbers with the matched results."
  (let (out)
    (advice-add 'match-string-no-properties :around
                #'danylo/match-string-no-properties-with-linum)
    (setq out (apply orig-fun args))
    (advice-remove 'match-string-no-properties
                   #'danylo/match-string-no-properties-with-linum)
    out))
(advice-add 'imenu--generic-function :around #'danylo/imenu-show-linum)

;;;; Imenu list: view the list of functions and classes in the file

(use-package imenu-list
  ;; https://github.com/bmag/imenu-list
  ;; Emacs plugin to show the current buffer's imenu entries
  :after org
  :bind (:map prog-mode-map
              ("C-c t i" . danylo/imenu-list-jump)
              :map org-mode-map
              ("C-c t i" . danylo/imenu-list-jump)
              :map imenu-list-major-mode-map
              ("C-c t i" . danylo/imenu-list-jump)
              ("DEL" . danylo/imenu-list-jump))
  :init (setq imenu-list-size danylo/side-window-width
              imenu-list-position 'left
              imenu-list-mode-line-format
              '("%e" mode-line-front-space
                (:propertize "%b" face mode-line-buffer-id) " "
                (:eval (buffer-name imenu-list--displayed-buffer)) " "
                mode-line-end-spaces))
  (require 'imenu-list))

(use-package imenu-anywhere
  ;; https://github.com/vspinu/imenu-anywhere
  ;; ido/ivy/helm imenu tag selection across buffers with the same
  ;; mode/project etc.
  :bind (("C-x c I" . helm-imenu-anywhere))
  )

(setq imenu-max-item-length "Unlimited")

(defvar danylo/imenu-list--displayed-window nil
  "The **window** who owns the saved imenu entries.")

(with-eval-after-load 'lsp-ui
  (defun danylo/check-if-update-imenu ()
    "Return t if imenu should be refreshed."
    (or (not (get-buffer-window lsp-ui-imenu-buffer-name))
        (not (equal (current-buffer) lsp-ui-imenu--origin))))

  (defun danylo/imenu-list-jump ()
    "Smart open imenu-list side window."
    (interactive)
    (setq danylo/imenu-list--displayed-window (selected-window))
    (danylo/side-window-jump
     (lambda ()
       ;; Update the imenu if it's not the current buffer and the buffer has
       ;; changed.
       (if (danylo/check-if-update-imenu)
           (lsp-ui-imenu)
         (select-window (get-buffer-window lsp-ui-imenu-buffer-name))))
     lsp-ui-imenu-buffer-name))
  (add-hook 'lsp-ui-imenu-mode-hook (lambda () (toggle-truncate-lines 1)))

  (defun danylo/imenu-update (&rest args)
    "Update Imenu list to reflect the current window's content."
    (when (and (get-buffer-window lsp-ui-imenu-buffer-name t)
               (not (string= (format "%s" (current-buffer)) lsp-ui-imenu-buffer-name)))
      (with-current-buffer (current-buffer)
        (when (danylo/check-if-update-imenu)
          (condition-case nil
              (lsp-ui-imenu--refresh)
            (imenu-unavailable nil))))))

  ;; Update Imenu automatically when window layout state changes
  (mapc (lambda (func)
          (advice-add func :after #'danylo/imenu-update))
        '(;; windmove-do-window-select
          ;; other-window switch-to-buffer
          ;; delete-window
          ;; quit-window
          save-buffer
          ;; delete-frame
          ;; select-window
          ))
  (add-hook 'window-state-change-hook 'danylo/imenu-update))

;; Patches to imenu so as to navigate using the **window** that owns the
;; current Imenu, not the buffer. This way handles multiple windows showing the
;; same buffer. Otherwise, the jump happens in the wrong window than the one
;; the user was browsing.

(defun danylo/imenu-list-goto-entry (orig-fun &rest args)
  "Switch to the original buffer and display the entry under point.
Patched to use original **window** instead of buffer."
  (interactive)
  (let ((entry (imenu-list--find-entry)))
    (select-window danylo/imenu-list--displayed-window)
    (imenu entry)
    (run-hooks 'imenu-list-after-jump-hook)
    (imenu-list--show-current-entry)))
(advice-add 'imenu-list-ret-dwim :around #'danylo/imenu-list-goto-entry)

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
(advice-add 'imenu-list-display-dwim :around #'danylo/imenu-list-display-entry)

(defun danylo/imenu-quit-go-back ()
  "Go back to the most recent window after quitting Imenu."
  (if danylo/imenu-list--displayed-window
      (select-window danylo/imenu-list--displayed-window)))
(advice-add 'imenu-list-quit-window :after #'danylo/imenu-quit-go-back)

;;;; Helm: search everything sledgehammer

(use-package helm
  ;; https://emacs-helm.github.io/helm/
  ;; Emacs incremental completion and selection narrowing framework
  :after company
  :bind (("M-i" . helm-swoop)
         ("C-x b" . danylo/helm-buffer-selector)
         ("C-x c i" . danylo/helm-imenu)
         ("C-x C-f" . helm-find-files)
         ("C-h f" . helm-apropos)
         ("C-h v" . helm-apropos)
         ("C-h a" . helm-apropos)
         :map company-mode-map
         ("S-<return>" . helm-company)
         :map helm-map
         ("TAB" . helm-execute-persistent-action))
  :init (setq helm-display-buffer-default-height danylo/num-completion-candidates
              helm-mode-line-string ""
              helm-comp-read-mode-line ""
              helm-buffer-max-length 30
              helm-candidate-number-limit 200
              helm-truncate-lines t
              helm-buffers-truncate-lines t
              helm-buffer-details-flag nil
              helm-source-buffer-not-found nil
              helm-split-window-inside-p t
              helm-move-to-line-cycle-in-source t
              helm-echo-input-in-header-line nil
              helm-display-header-line nil
              helm-buffers-fuzzy-matching t
              helm-ff-file-name-history-use-recentf t
              helm-recentf-matching t
              history-delete-duplicates t
              helm-buffer-skip-remote-checking t)
  (setq helm-imenu-type-faces
        '(("^\\(f(x)\\)\s*$" . danylo/imenu-function-face)
          ("^\\(var\\)\s*$" . danylo/imenu-var-face)
          ("^\\(struct\\|class\\)\s*$" . danylo/imenu-class-face)
          ("^\\(@\s*\\)\s*$" . danylo/imenu-macro-face)
          ("^\\(import\\|using\\|include\\)\s*$" . danylo/imenu-import-face)
          ("^\\(export\\)\s*$" . danylo/imenu-export-face)
          ("^\\(const\\)\s*$" . danylo/imenu-const-face)
          ("^\\([§]+\\)\s*$" . danylo/imenu-section-face)
          ("^\\(::\\)\s*$" . danylo/imenu-section-face)
          ("^\\(pkg\\)\s*$" . danylo/imenu-import-face))
        helm-imenu-delimiter " ")
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
                      :inherit 'default)
  :config
  (helm-mode 1)
  (setq helm-boring-buffer-regexp-list
        (append helm-boring-buffer-regexp-list
                '("\\*.*\\*" "magit.*"))))

(with-eval-after-load 'helm-files
  (setq helm-ff-auto-update-initial-value nil ;; disable auto expansion
        helm-ff-lynx-style-map nil            ;; disables auto-preview map
        helm-follow-mode-persistent nil
        ))

;;;; (start patch) Disable sorting of `helm-find-files` candidates under TRAMP.
(defun danylo/helm-ff-maybe-disable-sorting (orig-fun &rest args)
  "Disable sorting of `helm-find-files` candidates if using TRAMP."
  (if (file-remote-p helm-ff-default-directory)
      (car args) ;; candidate
    (apply orig-fun args)))
(advice-add 'helm-ff-sort-candidates
            :around #'danylo/helm-ff-maybe-disable-sorting)
;;;; (end patch)

(defun danylo/helm-ag-filter-extension ()
  "Search with helm-ag inside files of a given extension."
  (interactive)
  (let ((user-file-extension (read-string "File type (without .): ")))
    (let ((helm-ag-command-option (concat "-G .*\\." user-file-extension "$")))
      (helm-do-ag default-directory))))

(general-define-key
 "C-c h a f" 'danylo/helm-ag-filter-extension)

;;;; (start patch) Ensure there are no duplicate buffers in the helm-buffer-list.
(with-eval-after-load 'helm
  (defun danylo/helm-buffer-list (orig-fun &rest args)
    (delq nil (delete-dups (apply orig-fun args))))
  (advice-add 'helm-buffer-list :around #'danylo/helm-buffer-list))
;;;; (end patch)

(defun danylo/set-helm-window-height (orig-fun &rest args)
  "Make the Helm window taller."
  (let ((helm-display-buffer-default-height (/ (window-total-height) 2)))
    (apply orig-fun args)))

;; Make Helm window taller for the following Helm functions
(mapc (lambda (func)
        (advice-add func :around #'danylo/set-helm-window-height))
      '(helm-imenu helm-imenu-in-all-buffers danylo/helm-buffer-selector
                   helm-projectile-ag helm-projectile-grep
                   helm-find-files))

(defun danylo/helm-swoop-split-window-function (buf &rest _args)
  "Show Helm Swoop at bottom of current window, with the correct height."
  (let ((danylo/helm-swoop-height
         (- 0 (1+ danylo/num-completion-candidates)))
        (split-success t))
    (if helm-swoop-split-with-multiple-windows
        (condition-case nil
            (split-window-vertically danylo/helm-swoop-height)
          (error (setq split-success nil)))
      (when (one-window-p)
        (condition-case nil
            (split-window-vertically danylo/helm-swoop-height)
          (error (setq split-success nil)))))
    (if split-success
        ;; Go to helm-swoop buffer
        (other-window 1)
      (progn
        ;; Go to last active window and create a helm-swoop buffer there.
        (danylo/switch-to-last-window)
        (danylo/helm-swoop-split-window-function (current-buffer)))))
  (switch-to-buffer buf))

(defun danylo/helm-imenu ()
  "Search in all alike buffers when prefixed with C-u"
  (interactive)
  (if current-prefix-arg
      (helm-imenu-in-all-buffers)
    (helm-imenu)))

(use-package helm-swoop
  ;; https://github.com/emacsorphanage/helm-swoop
  ;; Efficiently hopping squeezed lines powered by Emacs helm interface
  :init
  (setq
   helm-swoop-split-with-multiple-windows t
   helm-swoop-split-window-function 'danylo/helm-swoop-split-window-function
   helm-swoop-flash-region-function 'pulse-momentary-highlight-region))

(use-package helm-bufler
  ;; https://github.com/alphapapa/bufler.el
  ;; To show Bufler’s grouped buffers in a Helm-specific command, a separate
  ;; helm-bufler package is available.
  ;; (Deprecated because it does not sort buffers by most recent used - which
  ;;  is important for me).
  :after (helm)
  :quelpa (helm-bufler :fetcher github :repo "alphapapa/bufler.el"
                       :files ("helm-bufler.el")))

(defun danylo/helm-buffer-selector ()
  "Run bufler nominally, or ibuffer if prefixed with C-u."
  (interactive)
  (cond ((equal current-prefix-arg '(4))
         ;; C-u
         (helm :sources '(helm-bufler-source)
               :truncate-lines nil))
        ((equal current-prefix-arg '(16))
         ;; C-u C-u
         (switch-to-buffer (read-buffer-to-switch "Switch to buffer: ")))
        (t
         ;; No C-u passed in (this is the default).
         (helm-buffers-list))
        ))

(use-package helm-ag
  ;; https://github.com/emacsorphanage/helm-ag
  ;; The silver searcher with helm interface
  ;;
  ;; Use C-c C-e to batch-edit the search results
  :init (setq helm-ag-insert-at-point 'symbol)
  :bind (("C-c h a g" . helm-do-ag)))

(use-package helm-company
  ;; https://github.com/Sodel-the-Vociferous/helm-company
  ;; Helm interface for company-mode
  )

(use-package helm-projectile
  ;; https://github.com/bbatsov/helm-projectile
  ;; Helm UI for Projectile
  :after company
  :init (setq projectile-completion-system 'helm
              projectile-enable-caching t)
  )

(use-package helm-xref
  ;; https://github.com/brotzeit/helm-xref
  ;; Helm interface for xref results
  :after company
  :init
  (setq xref-show-xrefs-function #'helm-xref-show-xrefs-27
        xref-show-definitions-function #'helm-xref-show-defs-27))

(use-package helm-unicode
  ;; https://github.com/bomgar/helm-unicode
  ;; Helm source for unicode/emoji
  )

;;;; Other things

(use-package smex
  ;; https://github.com/nonsequitur/smex
  ;; A smart M-x enhancement for Emacs
  )

(use-package helm-smex
  ;; https://github.com/ptrv/helm-smex
  ;; Helm interface for smex
  :after smex
  :bind (("M-x" . helm-M-x)
         ("M-X" . helm-smex-major-mode-commands))
  )

(use-package avy
  ;; https://github.com/abo-abo/avy
  ;; Jump to things in Emacs tree-style
  :ensure t
  :bind (("M-s" . danylo/avy-goto)
         ("M-g f" . avy-goto-line)
         ("C-'" . avy-isearch)))

(defun danylo/avy-goto ()
  "Smart avy command which senses C-u to pop the mark."
  (interactive)
  (if current-prefix-arg
      (avy-pop-mark)
    (avy-goto-word-1 (read-char "char: " t))))

;;; Ivy completion framework

(use-package ivy
  ;; https://github.com/abo-abo/swiper
  ;; Ivy is a generic completion mechanism for Emacs
  )

(use-package fzf
  ;; https://github.com/bling/fzf.el
  ;; A front-end for fzf.
  :bind (("C-c f f" . danylo/fzf-find-file)
         ("C-c f g" . fzf-grep-with-narrowing)
         ("C-c f b" . fzf-switch-buffer)
         )
  :config
  (setq fzf/args "-x --color bw --print-query --margin=1,0 --no-hscroll"
        fzf/executable "fzf"
        fzf/git-grep-args "-i --line-number %s"
        ;; command used for `fzf-grep-*` functions
        ;; example usage for ripgrep:
        ;; fzf/grep-command "rg --no-heading -nH"
        fzf/grep-command "grep -nrH"
        ;; If nil, the fzf buffer will appear at the top of the window
        fzf/position-bottom t
        fzf/window-height 15))

(require 'fzf)
(defun danylo/fzf-find-file ()
  "Find file in current directory, including hidden files."
  (interactive)
  (fzf-with-command
   "ag --hidden --ignore .git -l -g \"\""
   (lambda (selected)
     (when selected
       (find-file selected)))
   (fzf--resolve-directory)))

;;; ..:: Theming and code aesthetics ::..

;; Set frame title
(defvar frame-title '("GNU Emacs " emacs-version)
  "Frame (i.e., window) title.")

(defun danylo/make-frame-title ()
  "Sets the frame title."
  (if (boundp 'server-name)
      (setq frame-title-format (append frame-title '(" [" server-name "]")))
    (setq frame-title-format frame-title)))
(danylo/make-frame-title)
(add-hook 'after-make-frame-functions
          (lambda (new-frame)
            (danylo/make-frame-title)))

;;;; Font
(defun danylo/set-frame-font (&rest _)
  (set-frame-font "CaskaydiaCove NF" nil t))
(danylo/set-frame-font)
(add-hook 'after-make-frame-functions #'danylo/set-frame-font)

(use-package rainbow-delimiters
  ;; https://github.com/Fanael/rainbow-delimiters
  ;; Highlights delimiters such as parentheses, brackets or braces
  ;; according to their depth.
  :hook ((prog-mode . rainbow-delimiters-mode)))

;;;;  Highlight matching parentheses

(setq show-paren-delay `,danylo/fontify-delay)
(show-paren-mode 1)

(defun danylo/solaire-mode-inactive-buffer ()
  "Return t if the current buffer should not have solaire mode
active. Basically, any non-file-visiting buffer."
  (not (buffer-file-name (buffer-base-buffer))))

(use-package solaire-mode
  ;; https://github.com/hlissner/emacs-solaire-mode
  ;; Distinguish file-visiting buffers with slightly different background.
  :ensure t
  :after (doom-themes)
  :quelpa ((solaire-mode :fetcher github
                         :repo "hlissner/emacs-solaire-mode"))
  :init (setq solaire-mode-themes-to-face-swap '("doom-one")
              solaire-mode-real-buffer-fn #'danylo/solaire-mode-inactive-buffer)
  :config
  (solaire-global-mode +1))

;;; Highlight the current line.
(setq hl-line-sticky-flag nil
      global-hl-line-sticky-flag nil)
;; All programming major modes.
(add-hook 'prog-mode-hook #'hl-line-mode)
;; All modes derived from text-mode.
(add-hook 'text-mode-hook #'hl-line-mode)

;;;; (start patch) Throttle hl-line-highlight when the user is jamming
;;;;               `keyboard-quit'.
(defun danylo/hl-line-highlight (orig-fun &rest args)
  "Runs current line highlight update if certain conditions are met."
  (unless (eq this-command 'keyboard-quit)
    (apply orig-fun args)))
(advice-add 'hl-line-highlight :around #'danylo/hl-line-highlight)
;;;; (end patch)

;;;; (start patch) Make sure that line highlight is visible.
(defun my/fix-hl-line-for-solaire-mode ()
  "Ensure hl-line has a visible background in solaire-mode buffers."
  (when (and (bound-and-true-p solaire-mode) buffer-file-name)
    (set-face-background
     'solaire-hl-line-face `,danylo/dark-gray (selected-frame))))
(add-hook 'solaire-mode-hook #'my/fix-hl-line-for-solaire-mode)
(add-hook 'doom-load-theme-hook #'my/fix-hl-line-for-solaire-mode)
;;;; (end patch)

;;;; (start patch) Turn off solaire-mode in the minibuffer
(defun danylo/solaire-mode-fix-minibuffer (orig-fun &rest args)
  "No minibuffer fix, I want solaire OFF in the minibuffer.")
(defun danylo/minibuffer-no-solaire (orig-fun &rest args)
  "Do not turn on solaire in the minibuffer"
  (unless (minibufferp) (apply orig-fun args)))
(advice-add 'solaire-mode-fix-minibuffer
            :around #'danylo/solaire-mode-fix-minibuffer)
(advice-add 'turn-on-solaire-mode :around #'danylo/minibuffer-no-solaire)
;;;; (end patch)

;;;; Region pulsing

;; This setting makes the animation smoother
(setq pulse-iterations 1
      pulse-delay 0.2)

;;;; Line numbering

;; Show line numbers on the left
(general-define-key
 "C-x n l" 'danylo/toggle-display-line-numbers)

(defvar-local danylo/truncate-lines-state nil
  "The state of truncate lines when display line numbers is
toggled.")

(defun danylo/toggle-display-line-numbers ()
  "Toggle the display of line numbers on the side. Line truncation
is automatically turned on while the line numbers are displayed."
  (interactive)
  (let ((inhibit-message t))
    (if (bound-and-true-p display-line-numbers-mode)
        ;; Turn off line number display
        (progn
          (unless danylo/truncate-lines-state
            (toggle-truncate-lines 0))
          (display-line-numbers-mode 0))
      ;; Turn on line number display
      (progn
        (setq-local danylo/truncate-lines-state truncate-lines)
        (let ((inhibit-message t))
          (toggle-truncate-lines 1))
        (setq danylo/tmp nil)
        (display-line-numbers-mode 1)))))

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

;;;; General Emacs theme

(use-package doom-themes
  ;; https://github.com/hlissner/emacs-doom-themes
  ;; An opinionated pack of modern color-themes
  :config
  (load-theme 'doom-one t)
  (set-face-attribute 'region nil
                      :extend nil
                      :foreground `,danylo/black
                      :background `,danylo/yellow)
  (set-face-attribute 'font-lock-string-face nil
                      :foreground `,danylo/green))

(add-hook 'after-init-hook
          (lambda ()
            ;; Default Emacs font
            (set-face-attribute 'default nil
                                :height `,danylo/font-default-height)))

;;;; Font lock debug tools

(use-package highlight-refontification
  ;; https://github.com/Lindydancer/highlight-refontification
  ;; Visualize how font-lock refontifies a buffer
  )

(use-package font-lock-profiler
  ;; https://github.com/Lindydancer/font-lock-profiler
  ;; Coverage and timing tool for font-lock keywords
  )

(setq display-time-format "%H:%M"
      display-time-24hr-format t
      display-time-default-load-average nil
      display-time-string-forms
      '((format-time-string (or display-time-format "%H:%M") now)))
(display-time-mode)

(use-package danylo-common-font-lock
  ;; Common code for custom fontification
  :ensure nil
  :load-path danylo/emacs-custom-lisp-dir)

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

;;; ..:: Modeline ::..

(use-package mood-line
  ;; https://git.tty.dog/hpet_dog/mood-line
  ;; Minimal mode line configuration for Emacs, inspired by doom-modeline.
  :custom
  (mood-line-glyph-alist mood-line-glyphs-fira-code)
  :config
  (cl-loop for (update-fn . hooks) in mood-line--hooks-alist
           do (dolist (hook hooks)
                (add-hook hook update-fn)))
  (cl-loop for (update-fn . advised-fns) in mood-line--advice-alist
           do (dolist (advised-fn advised-fns)
                (advice-add advised-fn :after update-fn))))

;;;; (apply patch) Truncate the modeline version control string.
(defun danylo/truncate-mood-line-segment-vc (orig-fun &rest args)
  (let ((str (apply orig-fun args))
        (maxlen 10))
    (if (length> str maxlen)
        (concat (substring str 0 (1- maxlen)) "…")
      str)))
(advice-add 'mood-line-segment-vc--rev
            :around #'danylo/truncate-mood-line-segment-vc)
;;;; (end patch)

;;;; (start patch) Remove modeline boundary outlines while doing a Helm search.
(defun danylo/modeline-add-outline ()
  (when (display-graphic-p)
    (set-face-attribute 'solaire-mode-line-face nil
                        :overline `,danylo/faded-blue)
    (set-face-attribute 'solaire-mode-line-face nil
                        :underline `(:color ,danylo/faded-blue :position t))
    (set-face-attribute 'mode-line-active nil :overline `,danylo/faded-blue)
    (set-face-attribute 'mode-line-active nil
                        :underline `(:color ,danylo/faded-blue :position t))))
(defun danylo/modeline-remove-outline ()
  (when (display-graphic-p)
    (copy-face 'solaire-mode-line-face-original 'solaire-mode-line-face)
    (copy-face 'mode-line-active-original 'mode-line-active)))
(defun danylo/modeline-add-outline-on-helm-quit (orig-func &rest args)
  (danylo/modeline-add-outline)
  (apply orig-func args))
(add-hook 'helm-after-initialize-hook #'danylo/modeline-remove-outline)
(add-hook 'helm-exit-minibuffer-hook #'danylo/modeline-add-outline)
(advice-add 'helm-keyboard-quit :around #'danylo/modeline-add-outline-on-helm-quit)
;;;; (end patch)

(defface mode-line-active-original '((t))
  "Copy of the original `mode-line-active' face.")
(defface solaire-mode-line-face-original '((t))
  "Copy of the original `solaire-mode-line-face' face.")
(use-package moody
  ;; https://github.com/tarsius/moody
  ;; Tabs and ribbons for the mode-line.
  :custom
  (moody-mode-line-height #'moody-default-mode-line-height)
  (moody-ribbon-background '(base :background))
  :config
  (moody-replace-mode-line-front-space)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode)
  ;; Outline for the modeline border.
  (danylo/run-gui-conditional-code
   (lambda ()
     (unless (display-graphic-p)
       (setq moody-slant-placeholder " "))
     (set-face-attribute 'solaire-mode-line-face nil :box 'unspecified)
     (set-face-attribute 'solaire-mode-line-inactive-face nil :box 'unspecified)
     (copy-face 'mode-line-active 'mode-line-active-original)
     (copy-face 'solaire-mode-line-face 'solaire-mode-line-face-original)
     (danylo/modeline-add-outline))))
(require 'moody)

;;;; (apply patch) Fix moody modeline slant colors for solaire-mode.
(defun danylo/moody-wrap-color-fix
    (orig-fun string &optional width direction type face-active face-inactive)
  (let ((face-active (or face-active 'solaire-mode-line-face))
        (face-inactive (or face-inactive 'solaire-mode-line-inactive-face)))
    (funcall orig-fun string width direction type face-active face-inactive)))
(advice-add 'moody-wrap :around #'danylo/moody-wrap-color-fix)
;;;; (end patch)

(use-package window-numbering
  ;; https://github.com/nikolas/window-number
  ;; Allows you to select windows by numbers.
  :config
  (window-numbering-mode 1))

(use-package mlscroll
  ;; https://github.com/jdtsmith/mlscroll
  ;; Lightweight scrollbar for the Emacs mode line.
  :custom
  (mlscroll-shortfun-min-width 11) ; truncate which-func
  (mlscroll-alter-percent-position nil)
  (mlscroll-right-align nil))

;; Mode line format.
(defvar-local mode-line/current-project-cache nil
  "The project associated with the file. Accept new value when nil,
otherwise use the current value.")
(setq-default
 mode-line-format
 '((:eval
    (progn
      (let ((ml-space " ")
            (ml-current-window
             (if (moody-window-active-p)
                 (propertize "▶" 'face `(:foreground ,danylo/blue))
               " "))
            (ml-window-number
             (let ((winum (condition-case nil
                              (window-numbering-get-number-string)
                            (error "?"))))
               (concat
                " "
                (propertize winum 'face (when (moody-window-active-p)
                                          `(:foreground ,danylo/green)))
                " ")))
            (ml-buffer-mod-status
             (when (buffer-file-name (buffer-base-buffer))
               (cond
                ((buffer-modified-p)
                 (propertize (mood-line--get-glyph :buffer-modified)
                             'face 'mood-line-buffer-status-modified))
                (buffer-read-only
                 (propertize (mood-line--get-glyph :buffer-read-only)
                             'face 'mood-line-buffer-status-read-only))
                (t " "))))
            (ml-buffer-narrow-status
             (if (danylo/is-narrowed)
                 (danylo/nerd-md-icon
                  "nf-md-unfold_less_horizontal"
                  (face-attribute 'mode-line-emphasis :foreground))
               " "))
            (ml-project-name
             (let ((project (progn
                              (when (null mode-line/current-project-cache)
                                (setq mode-line/current-project-cache
                                      (mood-line-segment-project)))
                              mode-line/current-project-cache)))
               (when (and project (not (string-equal project "-")))
                 (moody-tab
                  (propertize
                   (format "(%s)" project)
                   'face (when (moody-window-active-p)
                           `(:foreground ,danylo/blue)))
                  nil 'up))))
            (ml-remote-host-name
             (when default-directory
               (when-let* ((host (file-remote-p default-directory 'host)))
                 (moody-ribbon
                  (propertize
                   (concat "@" host)
                   'face (when (moody-window-active-p)
                           `(:foreground ,danylo/green)))
                  nil 'down))))
            (ml-buffer-file-name
             (moody-tab
              (car (propertized-buffer-identification
                    (concat
                     (nerd-icons-icon-for-buffer)
                     "  "
                     (format-mode-line "%b"))))
              nil 'down))
            (ml-line-column " %l:%c ")
            (ml-scrollbar
             (progn
               (unless mlscroll-mode
                 (mlscroll-mode 1))
               (mlscroll-mode-line)))
            (ml-percent-position mode-line-percent-position)
            (ml-region-selection (when (use-region-p)
                                   (let ((num-selected
                                          (abs (- (region-end)
                                                  (region-beginning)))))
                                     (when num-selected
                                       (propertize
                                        (format "%d chars " num-selected)
                                        'face `(:foreground ,danylo/yellow))))))
            (ml-multiple-cursors (mood-line-segment-multiple-cursors))
            (ml-process-indicator (or (mood-line-segment-process) " "))
            (ml-auto-format (if apheleia-mode "𝛼" " "))
            (ml-major-mode (moody-tab (mood-line-segment-major-mode) nil 'down))
            (ml-vc (let ((vc-seg (mood-line-segment-vc)))
                     (if vc-seg
                         (moody-tab vc-seg nil 'up)
                       "")))
            (ml-clock (propertize
                       (concat (danylo/nerd-fa-icon "nf-fa-clock")
                               " "
                               display-time-string)
                       'face (if (moody-window-active-p)
                                 'solaire-mode-line-active-face
                               'solaire-mode-line-inactive-face))))
        (let* ((ml-left `(,ml-current-window
                          ,ml-window-number
                          ,ml-space
                          ,ml-buffer-mod-status
                          ,ml-buffer-narrow-status
                          ,ml-space
                          ,ml-project-name
                          ,ml-remote-host-name
                          ,ml-buffer-file-name
                          ,ml-line-column
                          ,ml-scrollbar
                          ,ml-space
                          ,ml-percent-position
                          ,ml-space
                          ,ml-region-selection
                          ,ml-multiple-cursors))
               (ml-right `(,ml-process-indicator
                           ,ml-space
                           ,ml-auto-format
                           ,ml-space
                           ,ml-major-mode
                           ,ml-vc
                           ,ml-space
                           ,ml-clock
                           ,ml-space
                           ,ml-space
                           ,ml-space))
               (ml-right-length 0)
               (ml-center-space ""))
          (dolist (seg ml-right)
            (setq ml-right-length (+ ml-right-length
                                     (string-width (format-mode-line seg)))))
          (setq ml-center-space
                (propertize
                 " "
                 'display
                 `((space :align-to
                          (- right (- 0 right-margin) ,ml-right-length)))))
          (append ml-left `(,ml-center-space) ml-right)))))))

;;;; (start patch) Patch so that modeline maintains highlight focus on active
;;;;               buffer
(defun danylo/internal--after-save-selected-window (orig-fun &rest args)
  "Patch to remove select-window modification in ansi-term redisplay."
  (advice-remove 'select-window #'danylo/select-window)
  (apply orig-fun args))

(defun danylo/select-window (orig-fun &rest args)
  "Patch to modify select-window so that modeline is not
activated on output into a buffer, e.g. ansi-term."
  (setq args `(,(nth 0 args) t))
  (apply orig-fun args))

(defun danylo/term-emulate-terminal (orig-fun &rest args)
  "Patch so that doom-modeline does not lose focus of active buffer
when there is another buffer printing out information."
  (advice-add 'select-window :around #'danylo/select-window)
  (advice-add 'internal--after-save-selected-window :around
              #'danylo/internal--after-save-selected-window)
  (apply orig-fun args)
  (advice-remove 'internal--after-save-selected-window
                 #'danylo/internal--after-save-selected-window))
(advice-add 'term-emulate-terminal :around #'danylo/term-emulate-terminal)
;;;; (end patch)

;;; ..:: Scrollbar ::..

(use-package yascroll
  ;; https://github.com/emacsorphanage/yascroll
  ;; Yet Another Scroll Bar Mode
  :hook ((prog-mode . yascroll-bar-mode)
         (text-mode . yascroll-bar-mode))
  :custom
  (yascroll:priority 1000)
  (yascroll:delay-to-hide nil)
  :config
  (let ((region-bg `,danylo/light-gray))
    (set-face-attribute 'yascroll:thumb-text-area
                        nil :background region-bg)
    (set-face-attribute 'yascroll:thumb-fringe
                        nil :background region-bg :foreground region-bg))
  )

;;;; (start patch) Improve performance of yascroll.
(defconst danylo/scrollbar-max-repeat-interval 0.025
  "Allowed interval between subsequent scrollbar refreshes, before it gets
hidden temporarily.")
(defconst danylo/scrollbar-initial-hide-interval 0.25
  "Time interval to not show the scrollbar for after it first gets hidden.")
(defvar danylo/scrollbar-show-always t
  "Always update and show the scrollbar, which effectively disables the state
machine below.")
(defvar-local danylo/scrollbar-last-show-time 0.0
  "Last time that the scrollbar show function was called.")
(defvar-local danylo/scrollbar-show-timer nil
  "Timer for transitioning scrollbar state machine states.")
(defvar-local danylo/scrollbar-state 'show
  "Scollbar state machine current state.")
(defun danylo/get-line-number (pos)
  (save-excursion
    (goto-char pos)
    (string-to-number (format-mode-line "%l"))))
(defun danylo/yascroll:show-scroll-bar-internal (&rest _)
  "Show scroll bar in buffer."
  (let* ((now (float-time))
         (dt (- now danylo/scrollbar-last-show-time)))
    (setq danylo/scrollbar-last-show-time now)
    ;; Scrollbar display state machine.
    (when (eq danylo/scrollbar-state 'show)
      (if (or danylo/scrollbar-show-always
              (> dt danylo/scrollbar-max-repeat-interval))
          (progn
            ;; This is the main code to show the scrollbar.
            (when-let ((scroll-bar (yascroll:choose-scroll-bar)))
              (let ((window-lines (yascroll:window-height))
                    (buffer-lines (danylo/get-line-number (point-max))))
                (when (< window-lines buffer-lines)
                  (let* ((scroll-top (danylo/get-line-number (window-start)))
                         (thumb-window-line (yascroll:compute-thumb-window-line
                                             window-lines buffer-lines scroll-top))
                         (thumb-buffer-line (+ scroll-top thumb-window-line))
                         (thumb-size (yascroll:compute-thumb-size
                                      window-lines buffer-lines))
                         (make-thumb-overlay
                          (cl-ecase scroll-bar
                            (right-fringe 'yascroll:make-thumb-overlay-right-fringe)
                            (text-area 'yascroll:make-thumb-overlay-text-area))))
                    (when (<= thumb-buffer-line buffer-lines)
                      (yascroll:make-thumb-overlays make-thumb-overlay
                                                    thumb-window-line
                                                    thumb-size)
                      (yascroll:schedule-hide-scroll-bar)))))))
        (setq danylo/scrollbar-state 'long-hide)))
    (when (eq danylo/scrollbar-state 'long-hide)
      (setq danylo/scrollbar-state 'idle)
      (run-with-timer
       (danylo/run-after-idle-interval danylo/scrollbar-initial-hide-interval)
       nil
       (lambda (buffer)
         (with-current-buffer buffer
           (setq danylo/scrollbar-state 'short-hide)
           (danylo/yascroll:show-scroll-bar-internal)))
       (current-buffer)))
    (when (eq danylo/scrollbar-state 'short-hide)
      (setq danylo/scrollbar-state 'idle)
      (setq
       danylo/scrollbar-show-timer
       (run-with-idle-timer
        (danylo/run-after-idle-interval danylo/scrollbar-max-repeat-interval)
        nil
        (lambda (buffer)
          (with-current-buffer buffer
            (setq danylo/scrollbar-state 'show)
            (danylo/yascroll:show-scroll-bar-internal)))
        (current-buffer))))
    ))
(advice-add 'yascroll:show-scroll-bar-internal
            :around #'danylo/yascroll:show-scroll-bar-internal)
(defun danylo/yascroll:debounced-scroll (orig-fun &rest args)
  (let ((danylo/scrollbar-show-always nil))
    (apply orig-fun args)))
(advice-add 'yascroll:after-window-scroll
            :around #'danylo/yascroll:debounced-scroll)
(advice-add 'yascroll:after-window-configuration-change
            :around #'danylo/yascroll:debounced-scroll)

(defun danylo/yascroll:after-mouse-scroll (&rest _)
  (yascroll:after-window-scroll (selected-window) nil))
(advice-add 'pixel-scroll-precision :after
            #'danylo/yascroll:after-mouse-scroll)

(defun danylo/yascroll:make-thumb-overlays-fast
    (orig-fun make-thumb-overlay window-line size)
  (save-excursion
    ;; Jump to the line.
    (goto-char (window-start))
    ;; `forward-line' is faster, but moves by logical lines instead of visual
    ;; lines. This will result in scrollbar gaps for wrapped
    ;; lines. `vertical-line' moves by visual lines, but is computationally
    ;; slower.
    ;; (forward-line window-line)
    (vertical-motion window-line)
    ;; Make thumb overlays.
    (condition-case nil
        (cl-loop repeat size
                 do
                 (progn
                   (push (funcall make-thumb-overlay) yascroll:thumb-overlays)
                   ;; (forward-line 1)
                   (vertical-motion 1))
                 until (eobp))
      (end-of-buffer nil))))
(advice-add 'yascroll:make-thumb-overlays
            :around #'danylo/yascroll:make-thumb-overlays-fast)
;;;; (end patch)

;;; ..:: Code editing ::..

;; Use spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; Follow symlinks automatically.
(setq vc-follow-symlinks t)

;; Disable secondary selection
;; (https://www.reddit.com/r/emacs/comments/3c61zl/abolish_the_secondary_selection_quick_and_easy/)
;; Also see how to unhighlight a secondary selection:
;;  https://stackoverflow.com/a/54665784
;;  Basically, do (delete-overlay mouse-secondary-overlay)
(global-unset-key (kbd "<M-drag-mouse-1>")) ;; was mouse-set-secondary
(global-unset-key (kbd "<M-down-mouse-1>")) ;; was mouse-drag-secondary
(global-unset-key (kbd "<M-mouse-1>"))	    ;; was mouse-start-secondary
(global-unset-key (kbd "<M-mouse-2>"))	    ;; was mouse-yank-secondary
(global-unset-key (kbd "<M-mouse-3>"))	    ;; was mouse-secondary-save-then-kill

;; Commenting keybindings
(general-define-key
 "C-x ;" 'comment-dwim
 "M-;" 'comment-line)

(use-package multiple-cursors
  ;; https://github.com/magnars/multiple-cursors.el
  ;; Multiple cursors for emacs.
  :custom
  (mc/always-run-for-all t)
  (mc/always-repeat-command t)
  )

(require 'multiple-cursors)

(defun mc/mark-all-previous-like-this (arg)
  "Mark all previous occurrences of word at point."
  (interactive "p")
  (condition-case nil
      (while t (mc/mark-previous-like-this arg))
    (user-error (throw 'break t))))

(defun mc/mark-all-next-like-this (arg)
  "Mark all next occurrences of word at point."
  (interactive "p")
  (condition-case nil
      (while t (mc/mark-next-like-this arg))
    (user-error (throw 'break t)))
  )

(defvar-local danylo/mc/unmark-direction nil
  "Direction in which to unmark cursor, stores the most recent mark
direction. Either 'previous or 'next.")
(defun mc/unmark-last ()
  "Unmark last mark."
  (interactive)
  (cond ((eq last-command 'mc/mark-previous-like-this)
         (mc/unmark-previous-like-this)
         (setq-local danylo/mc/unmark-direction 'previous))
        ((eq last-command 'mc/mark-next-like-this)
         (mc/unmark-next-like-this)
         (setq-local danylo/mc/unmark-direction 'next))
        ((eq last-command 'mc/unmark-last)
         (cond ((eq danylo/mc/unmark-direction 'previous)
                (mc/unmark-previous-like-this))
               ((eq danylo/mc/unmark-direction 'next)
                (mc/unmark-next-like-this))
               (t nil)))
        (t (setq-local danylo/mc/unmark-direction nil))))

(defhydra hydra-multiple-cursors (global-map "C-c m")
  "Multiple cursors"
  ("p" mc/mark-previous-like-this "↑")
  ("P" mc/mark-all-previous-like-this "↑*" :exit t)
  ("n" mc/mark-next-like-this "↓")
  ("N" mc/mark-all-next-like-this "↓*" :exit t)
  ("*" mc/mark-all-like-this "all" :exit t)
  ("r" mc/mark-all-in-region "region" :exit t)
  ("u" mc/unmark-last "undo")
  ("l" mc/edit-lines "lines" :exit t)
  )

(add-hook
 'after-init-hook
 (lambda ()
   ;; The following commands are run once rather than for each cursor (when
   ;; there's more than one via multiple-cursors).
   (dolist (item '(hydra-multiple-cursors/mc/mark-previous-like-this
                   hydra-multiple-cursors/mc/mark-all-previous-like-this-and-exit
                   hydra-multiple-cursors/mc/mark-next-like-this
                   hydra-multiple-cursors/mc/mark-all-next-like-this-and-exit
                   hydra-multiple-cursors/mc/mark-all-like-this-and-exit
                   hydra-multiple-cursors/mc/mark-all-in-region-and-exit
                   hydra-multiple-cursors/mc/unmark-last
                   hydra-multiple-cursors/mc/edit-lines-and-exit
                   hydra-keyboard-quit))
     (add-to-list 'mc/cmds-to-run-once item))))

(general-define-key
 :keymaps 'mc/keymap
 "<return>" nil
 "C-<return>" 'multiple-cursors-mode)

;;;; (start patch) Temporarily disable hl-line-mode when using multiple
;;;;               cursors. It seems to interact badly with keyboar-quit and
;;;;               proper tracking to the cursor state.
(defvar danylo/do-enable-hl-line-mode nil)
(make-variable-buffer-local 'danylo/do-enable-hl-line-mode)
(defun danylo/do-on-mc-start ()
  "This code runs when the user creates 2 or more cusors in the buffer."
  (let ((inhibit-quit t))
    (when hl-line-mode
      (hl-line-mode 0)
      (setq danylo/do-enable-hl-line-mode t))))
(defun danylo/do-on-mc-end ()
  "This code runs when the user goes back to a single cusor."
  (let ((inhibit-quit t))
    (when danylo/do-enable-hl-line-mode
      (hl-line-mode)
      (setq danylo/do-enable-hl-line-mode nil))))
(add-hook 'multiple-cursors-mode-enabled-hook #'danylo/do-on-mc-start)
(add-hook 'multiple-cursors-mode-disabled-hook #'danylo/do-on-mc-end)
;;;; (end patch)

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

(use-package drag-stuff
  ;; https://github.com/rejeep/drag-stuff.el
  ;; Drag stuff around in Emacs.
  :init
  (drag-stuff-global-mode 1)
  (defhydra hydra-drag-stuff (global-map "C-c C-d")
    "Resize window"
    ("p" drag-stuff-up "↑")
    ("n" drag-stuff-down "↓")
    ("b" drag-stuff-left "←")
    ("f" drag-stuff-right "→"))
  )

(use-package move-text
  ;; https://github.com/emacsfodder/move-text
  ;; Move current line or region up or down.
  ;; (Deprecated in favor of drag-stuff above)
  :disabled
  :config
  (move-text-default-bindings)
  (defun indent-region-advice (&rest ignored)
    (let ((deactivate deactivate-mark))
      (if (region-active-p)
          (indent-region (region-beginning) (region-end))
        (indent-region (line-beginning-position) (line-end-position)))
      (setq deactivate-mark deactivate)))
  (advice-add 'move-text-up :after 'indent-region-advice)
  (advice-add 'move-text-down :after 'indent-region-advice))

(use-package undo-fu
  ;; Undo helper with redo
  ;; Simple, stable linear undo with redo for Emacs.
  :bind (("C-z" . danylo/undo-redo))
  :config
  ;; Bind C-S-z to a key that does nothing, effectively disabling it so that we
  ;; don't accidentally undo when we mean to redo (use M-z for that).
  (define-key key-translation-map (kbd "C-S-z") (kbd "<f20>"))
  )

(defvar danylo/undo-command '(undo-fu-only-undo 1)
  "Memory for the undo direction (undo or redo). This makes C-u sticky to
keep redoing once it is pressed, so user can do C-u C-z C-z
C-z... instead of C-u C-z C-u C-z C-u C-z...")

(defun danylo/undo-redo (&optional arg)
  "Nominally undo, and if command is preceded with C-u then redo."
  (interactive "P")
  (if (or arg (not (memq last-command '(undo-fu-only-undo
                                        undo-fu-only-redo
                                        danylo/undo-redo))))
      (if (not arg)
          (setq danylo/undo-command '(undo-fu-only-undo 1))
        (if (listp arg)
            (setq danylo/undo-command '(undo-fu-only-redo 1))
          (setq undo-steps arg)
          (if (< undo-steps 0)
              (setq danylo/undo-command `(undo-fu-only-redo ,(- arg)))
            (setq danylo/undo-command `(undo-fu-only-undo ,arg))))))
  (let ((undo-command (car danylo/undo-command))
        (undo-steps (nth 1 danylo/undo-command)))
    (let ((inhibit-message t)
          (message-log-max nil))
      (funcall undo-command undo-steps))
    (danylo/print-in-minibuffer
     (format "%s %s %d steps"
             (danylo/nerd-fa-icon
              `,(if (eq undo-command 'undo-fu-only-undo)
                    "nf-fa-step_backward"
                  "nf-fa-step_forward"))
             (if (eq undo-command 'undo-fu-only-undo) "Undo" "Redo")
             undo-steps))))

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
  :custom
  (hl-todo-color-background nil)
  :config
  (global-hl-todo-mode)
  (add-to-list 'hl-todo-keyword-faces `("TODO" . ,danylo/black))
  (add-to-list 'hl-todo-keyword-faces `("NOTE" . ,danylo/black))
  (add-to-list 'hl-todo-keyword-faces `("DONE" . ,danylo/black))
  (add-to-list 'hl-todo-keyword-faces `("FIXME" . ,danylo/black)))

;;;; (start patch) Make hl-todo highlights into overlays that are not hidden by
;;;; hl-line.
(defvar danylo/hl-todo-overlay-idle-delay 0.25)
(defvar danylo/hl-todo-overlay-timer nil)
(defun danylo/hl-todo-overlay (&rest _)
  "Ensure hl-todo overlays are above hl-line overlays."
  (let ((inhibit-quit t))
    (when (bound-and-true-p hl-todo-mode)
      (when danylo/hl-todo-overlay-timer
        (cancel-timer danylo/hl-todo-overlay-timer))
      (setq
       danylo/hl-todo-overlay-timer
       (run-with-idle-timer
        (danylo/run-after-idle-interval
         danylo/hl-todo-overlay-idle-delay) nil
        (lambda (&rest _)
          (remove-overlays nil nil 'hl-todo-overlay t)
          (save-excursion
            (let ((case-fold-search nil)
                  (start (window-start))
                  (end (window-end nil t)))
              (goto-char start)
              (while (re-search-forward (hl-todo--regexp) end t)
                (let ((ov (make-overlay (match-beginning 0) (match-end 0))))
                  (overlay-put ov 'face (hl-todo--get-face))
                  (overlay-put ov 'priority 100) ;; Higher than hl-line-overlay-priority
                  (overlay-put ov 'hl-todo-overlay t)
                  ))))))))))
(add-hook 'after-change-functions #'danylo/hl-todo-overlay)
(add-hook 'find-file-hook #'danylo/hl-todo-overlay)
;; (add-hook 'post-command-hook #'danylo/hl-todo-overlay)
(add-hook 'window-size-change-functions #'danylo/hl-todo-overlay)
(add-hook 'window-scroll-functions #'danylo/hl-todo-overlay)
;;;; (end patch)

(use-package highlight-symbol
  ;; https://github.com/nschum/highlight-symbol.el
  ;; automatic and manual symbol highlighting
  :bind ((:map prog-mode-map
               ;; ("C-." . highlight-symbol-at-point)
               ("C-c C-." . highlight-symbol-remove-all)
               ))
  ;; :hook ((prog-mode . highlight-symbol-mode)
  ;;        (text-mode . highlight-symbol-mode))
  :custom
  (highlight-symbol-idle-delay 1.0)
  (highlight-symbol-highlight-single-occurrence nil)
  ;; (highlight-symbol-colors '(danylo/highlight-symbol-face))
  )

(defhydra hydra-highlight-symbol-at-point (global-map "C-s")
  "Highlight symbol"
  ("." highlight-symbol-at-point "toggle")
  ("n" highlight-symbol-next "next")
  ("p" highlight-symbol-prev "prev")
  ("k" highlight-symbol-remove-all "remove all" :exit t))

(require 'multiple-cursors)

(use-package danylo-highlight-symbol
  ;; Improvements to highlight-symbol.
  :after (highlight-symbol multiple-cursors)
  :load-path danylo/emacs-custom-lisp-dir)

(use-package rainbow-mode
  ;; https://github.com/dmalyuta/rainbow-mode
  ;; Colorize color names in buffers.
  ;; (Superceded by colorful-mode)
  :disabled
  :quelpa ((rainbow-mode :fetcher url
                         :url "https://raw.githubusercontent.com/dmalyuta/rainbow-mode/refs/heads/master/rainbow-mode.el"))
  :hook ((LaTeX-mode . (lambda () (rainbow-mode 1)))
         (emacs-lisp-mode . (lambda () (rainbow-mode 1))))
  )

(use-package colorful-mode
  ;; https://github.com/DevelopmentCool2449/colorful-mode
  ;; Preview any color in your buffer.
  :custom
  (colorful-use-prefix t)
  (colorful-prefix-string "● ")
  (colorful-allow-mouse-clicks t)
  (css-fontify-colors nil)
  :config
  (global-colorful-mode t)
  (add-to-list 'global-colorful-modes 'helpful-mode))

(use-package filladapt
  ;; https://elpa.gnu.org/packages/filladapt.html
  ;; Enhance the behavior of Emacs' Auto Fill mode
  :hook ((prog-mode . filladapt-mode)
         (org-mode . filladapt-mode)
         (text-mode . filladapt-mode)))

(use-package diff-hl
  ;; https://github.com/dgutov/diff-hl
  ;; Emacs package for highlighting uncommitted changes.
  ;; :hook ((prog-mode . diff-hl-flydiff-mode)
  ;;        (text-mode . diff-hl-flydiff-mode))
  :bind (:map diff-hl-mode-map
              ("C-c v =" . 'diff-hl-diff-goto-hunk)
              ("C-c v *" . 'diff-hl-show-hunk)
              ("C-c v ^" . 'diff-hl-set-reference-rev))
  :custom
  (diff-hl-command-prefix (kbd "C-c v"))
  (diff-hl-flydiff-delay 2.0)
  (diff-hl-side 'right)
  (diff-hl-update-async t)
  :init
  (let* ((width 2)
         (bitmap (vector (1- (expt 2 width)))))
    (define-fringe-bitmap 'my:diff-hl-bitmap bitmap 1 width '(top t)))
  (setq diff-hl-fringe-bmp-function (lambda (type pos) 'my:diff-hl-bitmap))
  :config
  ;; Match fringe background with buffer background.
  (dolist (item '(diff-hl-change diff-hl-insert diff-hl-delete))
    (set-face-attribute
     item nil :background (face-background 'solaire-default-face nil t)))
  (diff-hl-set-reference-rev "HEAD")
  (global-diff-hl-mode)
  (defhydra hydra-diff-hl (diff-hl-mode-map "C-c v")
    "Diff"
    ("[" diff-hl-previous-hunk "↑")
    ("]" diff-hl-next-hunk "↑")
    ("{" diff-hl-show-hunk-previous "↑*")
    ("}" diff-hl-show-hunk-next "↑*")
    ("*" diff-hl-show-hunk "show" :exit t)))

(defun danylo/diff-hl-set-reference ()
  "Set the reference revision for showing diff-hl changes. Do so buffer-locally."
  (interactive)
  (setq-local
   diff-hl-reference-revision
   (read-string
    (format "Set reference revision (buffer %s): "
            (buffer-name)) "master"))
  (diff-hl-update))

(global-set-key (kbd "C-c C-g") 'danylo/diff-hl-set-reference)

;; Remove a significant contributor to line scan slowness
(setq-default bidi-display-reordering nil
              bidi-paragraph-direction 'left-to-right
              bidi-inhibit-bpa t)

;; Delete trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;;; Copy whole file to clipboard.
(defun danylo/copy-whole-file (&optional arg)
  "Copy whole file to clipboard, without moving the cursor."
  (interactive)
  (kill-new (buffer-string)))

(general-define-key
 "C-c C-w" 'danylo/copy-whole-file)

;;;; Section delimiters

(defun danylo/section-msg (left msg right &optional nospace)
  "Section delimiters for comment.
LEFT and RIGHT are the section delimineters.
MSG is the section name.
NOSPACE, if t, means that there is no spacing added between delimiters."
  (if (region-active-p)
      (let* ((start (region-beginning))
             (end (region-end))
             (regionp (buffer-substring start end)))
        (delete-region start end)
        (if nospace
            (insert (format "%s%s%s" left regionp right))
          (insert (format "%s %s %s" left regionp right))))
    (if nospace
        (insert (format "%s%s%s" left msg right))
      (insert (format "%s %s %s" left msg right))))
  (goto-char (- (point) (+ (length right) (if nospace 0 1)))))

(defun danylo/code-section ()
  "Section delimiters for comment."
  (interactive)
  (danylo/section-msg "..::" "SECTION" "::.."))

(defun danylo/code-subsection ()
  "Subsection delimiters for comment."
  (interactive)
  (danylo/section-msg ">>" "SUBSECTION" "<<"))

(defun danylo/code-subsubsection ()
  "Subsubsection delimiters for comment."
  (interactive)
  (danylo/section-msg "@" "SUBSUBSECTION" "@"))

;;;; Duplicate line

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

(defun danylo/backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (backward-word arg) (point))))

(general-define-key
 "M-d" 'danylo/forward-delete-word
 "M-DEL" 'danylo/backward-delete-word)

;;;; Fill column (line width)

(setq-default fill-column danylo/fill-column)

;;;; Filling special structures

(defun danylo/search-region-delims (start-re &optional end-re)
  "Return the start and end points of a region using regular
expressions. If not inside the region, returns nil."
  (unless end-re (setq end-re start-re))
  (setq danylo/begin nil danylo/end nil)
  (save-excursion
    (let ((danylo/pre-first-begin (save-excursion (re-search-backward start-re nil t 1)))
          (danylo/pre-first-end (save-excursion (re-search-backward end-re nil t 1)))
          (danylo/post-first-begin (save-excursion (re-search-forward start-re nil t 1)))
          (danylo/post-first-end (save-excursion (re-search-forward end-re nil t 1))))
      (when (and danylo/pre-first-begin danylo/post-first-end
                 (or (not danylo/pre-first-end)
                     (>= danylo/pre-first-begin danylo/pre-first-end))
                 (or (not danylo/post-first-begin)
                     (>= danylo/post-first-begin danylo/post-first-end)))
        (setq danylo/begin danylo/pre-first-begin
              danylo/end danylo/post-first-end)
        ))))

(defun danylo/org-inside-block ()
  "Check if point is inside org-mode structure."
  (danylo/search-region-delims "#\\+begin_" "#\\+end_")
  (if danylo/begin t nil))

(defun danylo/julia-inside-docstring ()
  "Check if point is inside julia-mode function docstring."
  (let* ((ppss (save-excursion (syntax-ppss)))
         (in-string (nth 3 ppss))
         (in-triple-quote (and in-string (not (eq in-string ?\")))))
    (if in-triple-quote
        (progn
          (danylo/search-region-delims "\"\"\"" "\"\"\"")
          (if danylo/begin t nil))
      nil)))

(defun danylo/fill-region (delim-fun)
  "Fill an Org-mode block structure. The DELIM-FUN specifies which
function to use to get the region delimiters."
  (interactive)
  (save-excursion
    (when (funcall delim-fun)
      (let* ((region-start (progn (goto-char danylo/begin)
                                  (forward-line)
                                  (point)))
             (region-end (progn (goto-char danylo/end)
                                (forward-line 0)
                                (1- (point)))))
        (pulse-momentary-highlight-region region-start region-end)
        (fill-region region-start region-end)
        ;; Clear the fill-prefix so that does not mess with future indenting
        (if filladapt-mode (setq fill-prefix nil))))))

(defun danylo/fill ()
  "Custom filling depending on mode and point location in special
regions."
  (cond ((and (not (and transient-mark-mode mark-active))
              (derived-mode-p 'org-mode)
              (danylo/org-inside-block))
         (danylo/fill-region 'danylo/org-inside-block)
         t)
        ((and (not (and transient-mark-mode mark-active))
              (derived-mode-p 'julia-mode)
              (danylo/julia-inside-docstring))
         (danylo/fill-region 'danylo/julia-inside-docstring)
         t)
        (t nil)))

(defun danylo/smart-fill ()
  "Smart select between regular filling and my own filling."
  (interactive)
  (if (and transient-mark-mode mark-active)
      (let ((selection-start (region-beginning))
            (selection-end (region-end)))
        (deactivate-mark)
        (cl-letf
            (;; Let fill-paragrpah do the work, because I find that the
             ;; treesitter C++ comment filling function does not work very well
             ;; for filling regions.
             ((symbol-function 'c-ts-common--fill-paragraph)
              (lambda (&rest _) nil)))
          (fill-region selection-start selection-end nil t nil)))
    (unless (danylo/fill)
      (fill-paragraph nil t))))

;;;; (start patch) Unset fill-prefix when evaluating indent-region.
(defun danylo/indent-region (orig-fun &rest args)
  (let ((fill-prefix nil))
    (apply orig-fun args)))
(advice-add 'indent-region :around #'danylo/indent-region)
;;;; (end patch)

(defun danylo/julia-docstring-fill-skip ()
  "Return T if this block of text should not be filled. This occurs
in the following cases:
- It is a source code block;
- It is the initial list of function call signatures at the top of the
  docstring."
  (let* ((current-line (buffer-substring
                        (line-beginning-position) (line-end-position)))
         (has-backticks (string-match-p "^\s*```" current-line))
         (indent-this-line (current-indentation))
         is-heading
         (indent-last-line
          ;; Undo narrowing by filladapt temporarily. See
          ;; https://www.gnu.org/software/emacs/manual/html_node/eintr/save_002drestriction.html#:~:text=In%20Emacs%20Lisp%2C%20you%20can,narrowing%20that%20the%20code%20caused.
          (save-restriction
            (widen)
            (save-excursion
              (forward-line -1)
              (current-indentation)
              (setq is-heading
                    (string-match-p
                     "^\s*\"\"\""
                     (buffer-substring
                      (line-beginning-position)
                      (line-end-position))))))))
    (or has-backticks
        (and is-heading
             (eq indent-this-line
                 (+ indent-last-line julia-indent-offset))))
    ))

(defun danylo/filladapt--fill-paragraph (orig-fun &rest args)
  "Patch fill-adapt according to mode."
  (cond ((derived-mode-p 'julia-mode)
         (unless (danylo/julia-docstring-fill-skip)
           (apply orig-fun args)))
        (t (apply orig-fun args))))
(advice-add 'filladapt--fill-paragraph :around
            #'danylo/filladapt--fill-paragraph)

(use-package indent-bars
  ;; https://github.com/jdtsmith/indent-bars
  ;; Fast, configurable indentation guide-bars for Emacs.
  ;;
  ;; This package is also very useful because it builds correct stipple bitmaps
  ;; via `invent-bars--stipple'.
  :hook ((python-ts-mode . indent-bars-mode)
         (c-mode-common . indent-bars-mode))
  :custom
  (indent-bars-treesit-support t)
  (indent-bars-no-descend-string nil)
  (indent-bars-no-descend-lists t)
  (indent-bars-treesit-wrap '((cpp argument_list parameter_list init_declarator parenthesized_expression)))
  ;; Minimal dashed pattern.
  (indent-bars-color '(highlight :face-bg t :blend 0.2))
  (indent-bars-pattern "...   ")
  (indent-bars-no-stipple-char ?┆)
  (indent-bars-width-frac 0.1)
  (indent-bars-pad-frac 0.1)
  (indent-bars-zigzag nil)
  (indent-bars-color-by-depth nil)
  (indent-bars-treesit-scope-min-lines 5)
  (indent-bars-highlight-current-depth nil)
  ;; indent-bars on blank lines interact badly with lsp-ui-sideline:
  ;; https://github.com/jdtsmith/indent-bars/issues/14#issuecomment-1685103354.
  ;; I implemented a patch in the form of `danylo/indent-bars-watcher' below in
  ;; order to turn off display of the indent guides on blank lines whenever
  ;; lsp-ui-sideline information is displayed.
  (indent-bars-display-on-blank-lines t)
  )

(require 'indent-bars)

;; Define a column marker that will serve to indicate the fill column on lines
;; that go beyond it. Otherwise, the fill column ruler would be absent on those
;; lines.
(defvar-local danylo/fill-column-marker nil
  "Variable storing the column marker.")
;; (defface danylo/fill-column-whitespace-face
;;   `((t (:background "red")))
;;   "Face used for the danylo/turbo segment in the mode-line.")
(defun danylo/fill-column-marker-create (show)
  "Marker that indicates the fill column on lines with text that exceeds
it. If SHOW is nil, remove the marker."
  ;; Remove the marker.
  (when danylo/fill-column-marker
    (font-lock-remove-keywords nil danylo/fill-column-marker)
    (setq danylo/fill-column-marker nil))
  ;; Show the marker, if requested.
  (when (and show fill-column)
    ;; The matcher finds the next position where the line exceeds fill-column
    ;; and the character at the threshold is a blank character.
    (setq
     matcher
     (lambda (end)
       (let ((found nil)
             (col fill-column))
         (while (and (not found) (< (point) end))
           (let ((bol (point))
                 (eol (line-end-position)))
             (when (> (- eol bol) col)
               (let ((char (char-after (+ bol col))))
                 ;; Match only blank characters.
                 (when (and char (memq char '(?\s ?\t)))
                   (goto-char (+ bol col))
                   (set-match-data (list (point) (1+ (point))))
                   (font-lock-ensure bol eol)
                   (setq found t))))
             (forward-line 1)))
         found)
       ))
    ;; See `font-lock-keywords' for documentation of how below is structured.
    (setq danylo/fill-column-marker
          (if (display-graphic-p)
              `((,matcher (0 'fill-column-indicator prepend t)))
            `((,matcher
               (0 (progn
                    (put-text-property
                     (match-beginning 0)
                     (match-end 0)
                     'display (format "%c" display-fill-column-indicator-character))
                    (put-text-property
                     (match-beginning 0)
                     (match-end 0)
                     'face 'fill-column-indicator)) nil)))))
    (font-lock-add-keywords nil danylo/fill-column-marker)
    ;; Make sure that the whole buffer is fontified, so that no lines are
    ;; lazily "skipped".
    (font-lock-flush)
    ))

;; Display the fill indicator.
(require 'display-fill-column-indicator)
(defun danylo/update-fill-column-indicator (&rest _)
  "Adjust the stipple bitmap for the current font size, such that it
displays as a single thin vertical line. Inspired by
https://emacs.stackexchange.com/a/81307 but makes sure that the vertical
line is not repeated horizontally at certain text zoom levels."
  (if display-fill-column-indicator-mode
      (progn
        (add-to-list 'font-lock-extra-managed-props 'display)
        (let* ((char-width-pixels (frame-char-width))
               (rot (indent-bars--stipple-rot (selected-window) char-width-pixels)))
          (set-face-attribute 'fill-column-indicator nil
                              :background 'unspecified
                              :foreground `,danylo/light-gray
                              :stipple (indent-bars--stipple
                                        char-width-pixels 1 rot nil 0.1 0 "." 0)))
        (danylo/fill-column-marker-create t))
    (danylo/fill-column-marker-create nil))
  )
(defun danylo/init-fill-indicator-update ()
  (when (display-graphic-p)
    (setq-default display-fill-column-indicator-character ?\ ))
  (add-hook 'after-init-hook 'danylo/update-fill-column-indicator)
  (advice-add 'set-fill-column
              :after #'danylo/update-fill-column-indicator)
  (advice-add 'default-text-scale-decrease
              :after #'danylo/update-fill-column-indicator)
  (advice-add 'default-text-scale-increase
              :after #'danylo/update-fill-column-indicator)
  (advice-add 'danylo/reset-font-size
              :after #'danylo/update-fill-column-indicator))
(danylo/run-gui-conditional-code #'danylo/init-fill-indicator-update)

;; Activate fill indicator in programming and text major modes.
(add-hook 'find-file-hook
          (lambda ()
            (when (cl-loop
                   for parent-mode in '(prog-mode text-mode)
                   thereis (derived-mode-p parent-mode))
              (display-fill-column-indicator-mode 1))))
(add-hook 'display-fill-column-indicator-mode-hook
          (lambda ()
            (danylo/update-fill-column-indicator)))

;;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph
;;; https://www.emacswiki.org/emacs/UnfillParagraph
(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

(general-define-key
 "M-q" 'danylo/smart-fill
 "M-Q" 'unfill-paragraph)

(use-package minimap
  ;; https://github.com/dengste/minimap
  ;; Sidebar showing a "mini-map" of a buffer
  :init (setq minimap-width-fraction 0.1
              minimap-minimum-width 15
              minimap-hide-fringes t
              minimap-always-recenter nil
              minimap-recenter-type 'relative
              minimap-display-semantic-overlays nil
              minimap-window-location 'right))

;;;; Turbo mode for minimal-latency editing

(defvar danylo/turbo-on nil
  "State variable indicating if turbo editing mode is on.")

(defun danylo/turbo-toggle ()
  "Toggle turning off all heavy minor modes, for fast typing."
  (interactive)
  (if danylo/turbo-on
      (progn
        (danylo/enable-my-minor-modes)
        (danylo/print-in-minibuffer
         (format "%s Turbo OFF" (danylo/fa-icon "rocket"))))
    (progn
      (danylo/disable-my-minor-modes)
      (danylo/print-in-minibuffer
       (format "%s Turbo ON" (danylo/fa-icon "rocket")))))
  (force-mode-line-update t)
  (setq danylo/turbo-on (not danylo/turbo-on)))

(general-define-key
 "C-c e t" 'danylo/turbo-toggle)

;;;; Server mode customization

(setq server-client-instructions nil)

;; Allow narrow-to-region.
(put 'narrow-to-region 'disabled nil)

;;;; Code formatting

(use-package apheleia
  ;; https://github.com/radian-software/apheleia
  ;; Run code formatter on buffer contents without moving point, using RCS
  ;; patches and dynamic programming.
  :custom
  (apheleia-remote-algorithm 'remote)
  :config
  (apheleia-global-mode +1)
  (setf (alist-get 'black apheleia-formatters)
        (append (alist-get 'black apheleia-formatters) '("--required-version" "24.10.0")))
  )

;;;; (start patch) Colorize/fontify the visible window after apheleia runs
;;;;               formatting.
(defun danylo/fontify-after-format ()
  "Fontify visible part of the current window."
  (run-with-idle-timer
   (danylo/run-after-idle-interval 0.1) nil
   (lambda ()
     (save-excursion
       (font-lock-fontify-region (window-start) (window-end))))))
(add-hook 'apheleia-post-format-hook #'danylo/fontify-after-format)
;;;; (end patch)

;;;; Gray out false #if preprocessor fences in C-like languages.
(setq hide-ifdef-shadow t
      hide-ifdef-initially t
      hide-ifdef-verbose nil)
(add-hook
 'c-mode-common-hook
 (lambda ()
   (add-hook 'hide-ifdef-mode-hook
             (lambda ()
	       (unless hide-ifdef-define-alist
	         (setq hide-ifdef-define-alist
		       '((c-default-defines
                          (false . 0)
                          (true . 1)))))
	       (hide-ifdef-use-define-alist 'c-default-defines)))
   (hide-ifdef-mode t)))
(defvar danylo/hif-idle-delay 0.25)
(defvar danylo/hif-update-timer nil)
(defun danylo/hide-ifdefs-debounced (&rest _)
  (if danylo/hif-update-timer
      (cancel-timer danylo/hif-update-timer))
  (setq
   danylo/hif-update-timer
   (run-with-idle-timer
    (danylo/run-after-idle-interval danylo/hif-idle-delay) nil #'hide-ifdefs)))
(defun danylo/hide-ifdef-extra-hooks (&rest _)
  "Add extra functionality when hide-ifdef-mode is active, and remove it
otherwise."
  (if hide-ifdef-mode
      (progn
        (add-hook 'after-change-functions #'danylo/hide-ifdefs-debounced nil t)
        (add-hook 'after-save-hook #'danylo/hide-ifdefs-debounced nil t))
    (remove-hook 'after-change-functions #'danylo/hide-ifdefs-debounced)
    (remove-hook 'after-save-hook #'danylo/hide-ifdefs-debounced)))
(advice-add 'hide-ifdef-mode :after #'danylo/hide-ifdef-extra-hooks)

(use-package dtrt-indent
  ;; https://github.com/jscheid/dtrt-indent
  ;; Guess the indentation offset and transparently adjust the corresponding
  ;; settings in Emacs.
  :custom
  (dtrt-indent-verbosity 0)
  :config
  ;; (dtrt-indent-global-mode)
  )

(defun danylo/set-c-indent ()
  "Set the indent offset fopr C/C++ in treesitter."
  (interactive)
  (setq c-ts-mode-indent-offset
        (string-to-number (read-string "Set C-language indent to: "))))

(use-package fancy-narrow
  ;; https://github.com/Malabarba/fancy-narrow
  ;; Emacs package to immitate narrow-to-region with more eye-candy.
  :bind (("C-x n D" . fancy-narrow-to-defun)
         ("C-x n N" . fancy-narrow-to-region)
         ("C-x n W" . fancy-widen)))

;;; ..:: Window management ::..

;;;; >> Movement across windows <<

(defun danylo/switch-to-last-window ()
  "Return to the last active window."
  (interactive)
  (let ((win (get-mru-window t t t)))
    (unless win (error "Last window not found"))
    (let ((frame (window-frame win)))
      (select-frame-set-input-focus frame)
      (select-window win))))

(general-define-key
 "<left>" 'windmove-left
 "<right>" 'windmove-right
 "<up>" 'windmove-up
 "<down>" 'windmove-down
 "C-x l" 'danylo/switch-to-last-window
 "C-x C-l" 'danylo/switch-to-last-window)
(global-unset-key (kbd "C-<left>"))
(global-unset-key (kbd "C-<right>"))
(global-unset-key (kbd "C-<up>"))
(global-unset-key (kbd "C-<down>"))

;;;; >> Splitting windows <<

(general-define-key
 "C-x -" 'split-window-below
 "C-x |" 'split-window-right)

(setq split-window-preferred-function 'split-window-sensibly
      split-height-threshold 80
      split-width-threshold 160)

(defun danylo/ask-for-integer (prompt default)
  "Prompt user to enter an integer number, with a DEFAULT value.
Show PROMPT string in the minibuffer."
  (let ((user-input (read-string
                     (format "%s [default %d]: " prompt default))))
    (cond ((string= user-input "")
           ;; Default column width
           default)
          ((not (string-match "\\`[1-9][0-9]*\\'" user-input))
           ;; Bad input - ignore
           nil)
          (t
           ;; Good input - record as an integer
           (string-to-number user-input)))))

(defun danylo/split-main-window ()
  "Split the main window left/right/below/above the entire current
frame. Based on https://emacs.stackexchange.com/a/681/13661. Will
ask user for split direction (which user selects using the arrow
keys), and for the size of the resulting new window."
  (interactive)
  (let ((direction (progn
                     (message "direction up/down/left/right")
                     (read-event))))
    (when (member direction '(up down left right))
      (let* ((horizontal (member direction '(right left)))
             (default-size (if horizontal
                               (/ (window-total-width) 2)
                             (/ (window-total-height) 2)))
             (size (danylo/ask-for-integer
                    (if horizontal "width" "height") default-size)))
        (let ((new-window (split-window (frame-root-window) nil direction)))
          (save-excursion
            (select-window new-window)
            (enlarge-window (- size (if horizontal
                                        (window-width)
                                      (window-height)))
                            horizontal))
          new-window)
        ))))

;;;; >> Zoom split window <<

(use-package zoom-window
  ;; https://github.com/emacsorphanage/zoom-window
  ;; Zoom and Unzoom window.
  :bind (("C-x C-z" . zoom-window-zoom))
  )

;;;; >> Resizing windows <<

(use-package windsize
  ;; https://github.com/grammati/windsize
  ;; Easy resizing of emacs windows
  :init (setq windsize-cols 1
              windsize-rows 1))

(defvar danylo/windsize-current-step 1
  "The step amount by which to resize windows.")

(defvar danylo/windsize-timer nil
  "Timer object for windsize increment resetting.")

(defun danylo/windsize-smart-step (dir &optional arg)
  (interactive "P")
  (when arg
    ;; Cancel existing timer
    (when danylo/windsize-timer
      (cancel-timer danylo/windsize-timer))
    ;; Change step increment and create new timer
    (setq danylo/windsize-current-step danylo/windsize-big-step
          danylo/windsize-timer (run-with-idle-timer
                                 (danylo/run-after-idle-interval 2.0)
                                 nil
                                 (lambda ()
                                   (setq danylo/windsize-current-step 1)))))
  (windsize-resize dir danylo/windsize-current-step))

(defun danylo/windsize-up-smart (&optional arg)
  (interactive "P")
  (danylo/windsize-smart-step 'up arg))

(defun danylo/windsize-down-smart (&optional arg)
  (interactive "P")
  (danylo/windsize-smart-step 'down arg))

(defun danylo/windsize-left-smart (&optional arg)
  (interactive "P")
  (danylo/windsize-smart-step 'left arg))

(defun danylo/windsize-right-smart (&optional arg)
  (interactive "P")
  (danylo/windsize-smart-step 'right arg))

(defhydra hydra-resize-window (global-map "M-r")
  "Resize window"
  ("<up>" danylo/windsize-up-smart "↑")
  ("<down>" danylo/windsize-down-smart "↓")
  ("<left>" danylo/windsize-left-smart "←")
  ("<right>" danylo/windsize-right-smart "→"))

(defun danylo/set-window-width (width)
  "Set the selected window's width."
  (let ((delta (- width (window-width))))
    (if (>= delta 0)
        (enlarge-window-horizontally delta)
      (shrink-window-horizontally (* -1 delta)))))

(defun danylo/set-window-columns-precise ()
  "Set the selected window to user-specified number of columns.
Default is 80"
  (interactive)
  (let ((desired-width (danylo/ask-for-integer
                        "width" danylo/fill-column)))
    (when desired-width
      (danylo/set-window-width desired-width))))

(general-define-key
 "C-x ~" 'danylo/set-window-columns-precise)

;;;; >> Swapping windows between buffers <<

(use-package buffer-move
  ;; https://github.com/lukhas/buffer-move
  ;; Swap buffers between windows
  :custom
  (buffer-move-behavior 'move)
  )

(defmacro danylo/move-with-swap (fun-name move-fun)
  "Swap the current buffer with a buffer in another window, the direction
being set by MOVE-FUN."
  `(defun ,fun-name ()
     (interactive)
     (let ((buffer-move-behavior 'swap))
       (funcall ,move-fun))))

(defmacro danylo/move-window (fun-name move-fun)
  "Move current window to another window, the direction being set by MOVE-FUN.
Unlike `buf-move-*', the original window is removed."
  `(defun ,fun-name ()
     (interactive)
     (let ((win (funcall ,move-fun)))
       (danylo/switch-to-last-window)
       (delete-window)
       (select-window win))))

(danylo/move-with-swap buf-swap-up #'buf-move-up)
(danylo/move-with-swap buf-swap-down #'buf-move-down)
(danylo/move-with-swap buf-swap-left #'buf-move-left)
(danylo/move-with-swap buf-swap-right #'buf-move-right)

(danylo/move-window win-move-up #'buf-move-up)
(danylo/move-window win-move-down #'buf-move-down)
(danylo/move-window win-move-left #'buf-move-left)
(danylo/move-window win-move-right #'buf-move-right)

(defhydra hydra-swap-buffer (global-map "C-c s")
  "Swap buffer with window"
  ("<up>" buf-swap-up "↑")
  ("<down>" buf-swap-down "↓")
  ("<left>" buf-swap-left "←")
  ("<right>" buf-swap-right "→"))

(defhydra hydra-move-buffer (global-map "C-c m")
  "Move buffer to window"
  ("<up>" buf-move-up "↑")
  ("<down>" buf-move-down "↓")
  ("<left>" buf-move-left "←")
  ("<right>" buf-move-right "→"))

(defhydra hydra-move-window (global-map "C-c w")
  "Move buffer to window"
  ("<up>" win-move-up "↑")
  ("<down>" win-move-down "↓")
  ("<left>" win-move-left "←")
  ("<right>" win-move-right "→"))

;; winner-mode, which lets you go back (C-c <left>) and forward (C-c <right>) in
;; window layout history
(when (fboundp 'winner-mode) (winner-mode 1))
(defhydra hydra-window-history (global-map "C-c l")
  "Window layout history"
  ("<left>" winner-undo "previous")
  ("<right>" winner-redo "next"))

;;;; >> Transposing the window layout <<

(use-package transpose-frame
  ;; https://melpa.org/#/transpose-frame
  ;; Transpose windows arrangement in a frame
  :bind (("C-x t t" . 'transpose-frame))
  )

;;;; >> Buffer menu <<

(setq-default ibuffer-default-sorting-mode 'major-mode)

(use-package bufler
  ;; https://github.com/alphapapa/bufler.el
  ;; Group buffers into workspaces with programmable rules
  :quelpa (bufler :fetcher github
                  :repo "alphapapa/bufler.el"
                  :files ("*.el" (:exclude "helm-bufler.el")))
  :config
  ;; Filter out imenu buffers, but keep fundamental-mode and special-mode
  ;; buffers.
  (setq bufler-filter-buffer-modes
        (append
         (cl-set-difference bufler-filter-buffer-modes '(fundamental-mode special-mode))
         '(imenu-list-major-mode)))

  ;; Filter out buffers by regexp.
  (setq bufler-filter-buffer-name-regexps
        (append bufler-filter-buffer-name-regexps
                `(,(rx "*Help*")
                  ,(rx "*Buffer List*"))))

  ;; Customize the "Name" column to should file icons.
  (bufler-define-column "Name" (:max-width nil)
    (ignore depth)
    (let ((indentation (make-string (* 2 bufler-indent-per-level) ? ))
          (mode-annotation (when (cl-loop for fn in bufler-buffer-mode-annotate-preds
                                          thereis (funcall fn buffer))
                             (propertize (concat (replace-regexp-in-string
                                                  (rx "-mode" eos) ""
                                                  (symbol-name (buffer-local-value 'major-mode buffer))
                                                  t t)
                                                 " ")
                                         'face 'bufler-mode)))
          (buffer-name (buffer-name buffer))
          (buffer-icon (with-current-buffer buffer
                         (nerd-icons-icon-for-buffer)))
          (modified (when (and (buffer-file-name buffer)
                               (buffer-modified-p buffer))
                      (propertize bufler-column-name-modified-buffer-sigil
                                  'face 'font-lock-warning-face))))
      (concat indentation ; mode-annotation
              buffer-icon " " buffer-name modified)))
  (setq bufler-columns '("Name" "Size" "VC" "Path")))

(use-package all-the-icons-ibuffer
  ;; https://github.com/seagle0128/all-the-icons-ibuffer
  ;; Display icons for all buffers in ibuffer.
  :hook (ibuffer-mode . all-the-icons-ibuffer-mode)
  )

(use-package ibuffer-vc
  ;; https://github.com/purcell/ibuffer-vc
  ;; Let Emacs' ibuffer-mode group files by git project etc., and show file state.
  :init
  :hook ((ibuffer . (lambda ()
                      (ibuffer-vc-set-filter-groups-by-vc-root)
                      (unless (eq ibuffer-sorting-mode 'alphabetic)
                        (ibuffer-do-sort-by-alphabetic))))))

(defun danylo/buffer-list ()
  "Run bufler nominally, or ibuffer if prefixed with C-u."
  (interactive)
  (if current-prefix-arg
      (ibuffer)
    (bufler)))

(general-define-key
 "C-x C-b" 'danylo/buffer-list)

;; Move cursor to minibuffer (useful if lost focus, due to mouse click)

(defun danylo/switch-to-minibuffer-window ()
  "switch to minibuffer window (if active)"
  (interactive)
  (when (active-minibuffer-window)
    (select-frame-set-input-focus (window-frame (active-minibuffer-window)))
    (select-window (active-minibuffer-window))))

(general-define-key
 "C-x c b" 'danylo/switch-to-minibuffer-window)

;;;; Improve quit-window behavior (automatically delete-window sometimes)

(defun danylo/quit-and-kill-window (orig-fun &rest args)
  "Optionally also kill the window after quitting it."
  (setq delete-this-window nil)
  ;; Quit or delete window as appropriate
  (if delete-this-window
      (progn
        (apply orig-fun args)
        (delete-window))
    (apply orig-fun args)))
(advice-add 'quit-window :around #'danylo/quit-and-kill-window)

(use-package popper
  ;; https://github.com/karthink/popper
  ;; Emacs minor-mode to summon and dismiss buffers easily.
  :after (lsp-mode)
  :bind (("C-`"   . popper-toggle)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :custom
  (popper-display-function #'popper-select-popup-at-bottom)
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          "\\*Compile-Log\\*"
          help-mode
          lsp-help-mode
          compilation-mode
          emacs-lisp-compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1)
  )

(use-package hide-mode-line
  ;; https://github.com/hlissner/emacs-hide-mode-line
  ;; An Emacs plugin that hides (or masks) the current buffer's mode-line
  )

;;; ..:: Terminal emulator ::..

(defun danylo/run-terminal-here ()
  "Run terminal from current buffer"
  (interactive "@")
  (shell-command (concat "kitty > /dev/null 2>&1 & disown") nil nil))

(defconst danylo/emacs-libvterm-dir `,(danylo/make-path "libvterm/")
  "Location of emacs-libvterm.")

(use-package vterm
  ;; https://github.com/akermu/emacs-libvterm
  ;; Emacs libvterm integration
  :load-path danylo/emacs-libvterm-dir
  :bind (:map vterm-mode-map
              ("C-<up>" . nil)
              ("C-<down>" . nil)
              ("C-<left>" . nil)
              ("C-<right>" . nil)
              ("<up>" . nil)
              ("<down>" . nil)
              ("<left>" . nil)
              ("<right>" . nil)
              ("M-r" . nil)
              ("C-x [" . vterm-copy-mode)
              ("C-c r" . rename-buffer)
              ("S-<return>" . vterm-send-tab)
              :map vterm-copy-mode-map
              ("q" . vterm-copy-mode) ; Exit from vterm-copy-mode
              )
  :hook ((vterm-mode . (lambda ()
                         (goto-address-mode 1)
                         (set (make-local-variable 'buffer-face-mode-face) '(:family "CaskaydiaCove Nerd Font"))
                         (buffer-face-mode t))))
  :init
  (require 'vterm)
  (setq vterm-always-compile-module t
        vterm-kill-buffer-on-exit t
        vterm-max-scrollback 100000
        vterm--prompt-tracking-enabled-p t
        vterm-buffer-name "Terminal")
  ;; Add update-pwd to the list of commands that Emacs is allowed to execute from vterm.
  (add-to-list 'vterm-eval-cmds '("update-pwd" (lambda (path) (setq default-directory path)))))

(use-package vterm-toggle
  ;; https://github.com/jixiuf/vterm-toggle
  ;; Toggles between the vterm buffer and whatever buffer you are editing.
  :disabled
  :ensure nil
  :bind (:map vterm-mode-map
              ("s-n" . vterm-toggle-forward)
              ("s-p" . vterm-toggle-backward))
  :config
  (global-set-key [f2] 'vterm-toggle)
  (global-set-key [C-f2] 'vterm-toggle-cd)
  )

(defun danylo/vterm (orig-fun &rest args)
  "Create a new vterm buffer, or open an existing one. This
function is a patch around the original one, such that a call
to (vterm) with no argument will create a **new** vterm buffer."
  (setq danylo/vterm~buf-num (nth 0 args))
  (if danylo/vterm~buf-num
      (apply orig-fun args)
    (progn
      ;; Create a new vterm buffer
      (setq danylo/vterm~buf-num 0)
      (mapc (lambda (buf)
              (let* ((this-buffer (buffer-name buf))
                     (existing-num
                      (string-match
                       (concat (regexp-quote vterm-buffer-name) "<\\([0-9]+\\)>")
                       this-buffer)))
                (when existing-num
                  (setq existing-num (string-to-number
                                      (match-string 1 this-buffer)))
                  (when (>= existing-num danylo/vterm~buf-num)
                    (setq danylo/vterm~buf-num (1+ existing-num)))
                  )))
            (buffer-list))
      (vterm danylo/vterm~buf-num)
      )))
(advice-add 'vterm :around #'danylo/vterm)

(general-define-key
 "C-c t e" 'vterm
 "C-c t r" 'danylo/run-terminal-here)

;; Find file at point in vterm buffer and open it in a buffer below.
(with-eval-after-load 'vterm
  (push (list "find-file-below"
              (lambda (path)
                (if-let* ((buf (find-file-noselect path))
                          (window (display-buffer-below-selected buf nil)))
                    (select-window window)
                  (message "Failed to open file: %s" path))))
        vterm-eval-cmds))

(use-package with-editor
  ;; Use the current Emacs instance as the $EDITOR of child processes.
  ;; https://github.com/magit/with-editor
  :disabled
  :config
  (add-hook 'vterm-mode-hook  'with-editor-export-editor)
  )

;;; ..:: Completion ::..

(use-package company
  ;; https://github.com/company-mode/company-mode
  ;;  Modular in-buffer completion framework for Emacs
  :hook ((emacs-lisp-mode . company-mode)
         (LaTeX-mode . company-mode)
         (sh-mode . company-mode)
         (prog-mode . company-mode)
         (text-mode . company-mode))
  :bind (("S-<return>" . company-complete))
  :init
  (setq company-dabbrev-downcase 0
        company-async-timeout 10
        company-require-match 'never
        company-minimum-prefix-length 0
        company-auto-complete nil
        company-idle-delay nil
        company-auto-select-first-candidate nil)
  )

(use-package cape
  ;; https://github.com/minad/cape
  ;; cape.el - Completion At Point Extensions
  :bind ("C-c p" . cape-prefix-map)
  :config
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
  )

(defun danylo/company--continue (orig-fun &rest args)
  "A patch so that company does not auto-trigger after completing
a candidate. This means user has to hit the completion key again
for continuing completion. This gets around a bug where
company-point is nil, which causes the error `Wrong type
argument: number-or-marker-p, nil'."
  (when (company-call-backend 'no-cache company-prefix)
    ;; Don't complete existing candidates, fetch new ones.
    (setq company-candidates-cache nil))
  (company-cancel 'unique))
(advice-add 'company--continue :around #'danylo/company--continue)

(use-package citre
  ;; https://github.com/universal-ctags/citre
  ;; A superior code reading & auto-completion tool with pluggable backends.  :defer t
  :disabled
  :init
  ;; This is needed in `:init' block for lazy load to work.
  (require 'citre-config)
  ;; Bind your frequently used commands.  Alternatively, you can define them
  ;; in `citre-mode-map' so you can only use them when `citre-mode' is enabled.
  (global-set-key (kbd "C-x c j") 'citre-jump)
  (global-set-key (kbd "C-x c J") 'citre-jump-back)
  (global-set-key (kbd "C-x c p") 'citre-ace-peek)
  (global-set-key (kbd "C-x c u") 'citre-update-this-tags-file)
  :config
  (setq
   ;; Set this if you use project management plugin like projectile.  It's
   ;; used for things like displaying paths relatively, see its docstring.
   citre-project-root-function #'projectile-project-root
   ;; Set this if you'd like to use ctags options generated by Citre
   ;; directly, rather than further editing them.
   citre-edit-ctags-options-manually nil
   ;; If you only want the auto enabling citre-mode behavior to work for
   ;; certain modes (like `prog-mode'), set it like this.
   citre-auto-enable-citre-mode-modes '(prog-mode)))

(use-package dape
  ;; https://github.com/svaante/dape
  ;; Debug Adapter Protocol for Emacs.
  )

(use-package yaml)

(use-package lsp-mode
  :ensure t
  :hook ((c-mode-common . lsp)
         (python-ts-mode . lsp)
         (astro-mode . lsp)
         (lsp-mode . lsp-diagnostics-mode)
         (lsp-mode . lsp-completion-mode)
         (lsp-completion-mode
          . (lambda ()
              ;; Ensure LSP backend retains the snippet completion.
              (setq company-backends
                    (mapcar #'company-mode/backend-with-yas
                            company-backends)))))
  :custom
  (lsp-keymap-prefix "C-c l")
  (lsp-diagnostics-provider :auto)
  (lsp-completion-provider :capf)
  (lsp-session-file (expand-file-name ".lsp-session" user-emacs-directory))
  ;; I thought the following would speed up LSP by not triggering a call to
  ;; server on every key stroke, but ultimately it seems to just break clangd
  ;; from tracking file changes properly...
  ;; (lsp-document-sync-method 'lsp--sync-incremental)
  ;; ... so instead just use the default option of letting the server decide
  ;; how it wants to receive updates:
  (lsp-document-sync-method nil)
  (lsp-debounce-full-sync-notifications t)
  (lsp-keep-workspace-alive nil)
  (lsp-idle-delay 0.5)
  ;; Verbose logging for debugging
  (lsp-log-io nil)
  (lsp-log-max 1000)
  ;; Progress
  (lsp-progress-via-spinner t)
  (lsp-progress-spinner-type 'vertical-breathing)
  (lsp-progress-function #'lsp-on-progress-legacy)
  ;; Core
  (lsp-enable-xref t)
  (lsp-auto-configure t)
  (lsp-eldoc-enable-hover nil)
  (lsp-enable-dap-auto-configure nil)
  (lsp-enable-file-watchers nil)
  (lsp-enable-folding nil)
  (lsp-enable-imenu t)
  (lsp-enable-indentation nil)
  (lsp-enable-links nil)
  (lsp-enable-on-type-formatting nil)
  (lsp-enable-suggest-server-download t)
  (lsp-enable-symbol-highlighting nil)
  (lsp-enable-text-document-color nil)
  ;; Completion
  (lsp-completion-enable t)
  (lsp-completion-enable-additional-text-edit nil)
  (lsp-enable-snippet nil)
  (lsp-completion-show-kind nil)
  ;; Headerline
  (lsp-headerline-breadcrumb-enable t)
  (lsp-headerline-breadcrumb-enable-diagnostics nil)
  (lsp-headerline-breadcrumb-enable-symbol-numbers nil)
  (lsp-headerline-breadcrumb-icons-enable nil)
  ;; Modeline
  (lsp-modeline-code-actions-enable nil)
  (lsp-modeline-diagnostics-enable nil)
  (lsp-modeline-workspace-status-enable nil)
  ;; Minibuffer
  (lsp-signature-auto-activate nil)
  (lsp-signature-doc-lines nil)
  (lsp-signature-render-documentation nil)
  ;; Lens
  (lsp-lens-enable nil)
  ;; C++
  (lsp-clients-clangd-args '("--header-insertion=never"))
  ;; semantic
  (lsp-semantic-tokens-enable nil)
  :init
  (setq lsp-use-plists t)
  :config
  ;; WORKAROUND Hide mode-line of the lsp-ui-imenu buffer
  ;; See https://github.com/emacs-lsp/lsp-ui/issues/243
  (advice-add 'lsp-ui-imenu :after #'hide-mode-line-mode)
  ;; Make sure that temporary symbol highlight does not stick around when
  ;; renaming variables.
  (advice-add 'lsp-rename :before (lambda (&rest args)
                                    (highlight-symbol-mode-remove-temp))))

(use-package lsp-ui
  ;; https://github.com/emacs-lsp/lsp-ui
  ;; UI integrations for lsp-mode.
  :after (lsp-mode)
  :bind (:map lsp-ui-imenu-mode-map
              ("<left>" . nil)
              ("<right>" . nil)
              ("C-c t i" . danylo/imenu-list-jump)
              ("DEL" . danylo/imenu-list-jump)
              ("SPC" . lsp-ui-imenu--view)
              ("<return>" . lsp-ui-imenu--visit))
  :custom
  (lsp-ui-sideline-enable t)
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-delay 0.2)
  (lsp-ui-sideline-show-diagnostics t)
  (lsp-ui-sideline-diagnostic-max-line-length 80)
  (lsp-ui-imenu-auto-refresh nil)
  (lsp-ui-imenu-buffer-position 'left)
  (lsp-ui-imenu-window-width 30)
  (lsp-ui-imenu-window-fix-width t)
  )

(defun toggle-lsp ()
  "Toggle LSP activation."
  (interactive)
  (if lsp-managed-mode
      (lsp-managed-mode -1)
    (lsp)))

;;;; (start patch)
(defvar-local danylo/indent-bars-display-on-blank-lines-prev nil
  "Value of indent-bars-display-on-blank-lines the last time that
`danylo/indent-bars-watcher' ran.")
(defun danylo/indent-bars-watcher (symbol newval operation where)
  (when (and indent-bars-mode
             (memq 'lsp-ui-mode minor-mode-list))
    (setq indent-bars-display-on-blank-lines (equal newval nil))
    (unless (equal indent-bars-display-on-blank-lines
                   danylo/indent-bars-display-on-blank-lines-prev)
      (setq danylo/indent-bars-display-on-blank-lines-prev
            indent-bars-display-on-blank-lines)
      (save-excursion
        ;; Add back or remove indent-bars guides on blank lines.
        (indent-bars--setup-font-lock)
        (indent-bars--window-change)
        (font-lock-fontify-region (window-start) (window-end))))))
(add-variable-watcher 'lsp-ui-sideline--ovs #'danylo/indent-bars-watcher)
;;;; (end patch)

;;;;;;;;;; lsp-booster for more performant LSP mode.
;; Emacs LSP performance booster
;; https://github.com/blahgeek/emacs-lsp-booster
;;
;; Place the executable emacs-lsp-booster into one of the folders returned by
;; (exec-path) (Print these out with M-:).
(defun lsp-booster--advice-json-parse (old-fn &rest args)
  "Try to parse bytecode instead of json."
  (or
   (when (equal (following-char) ?#)
     (let ((bytecode (read (current-buffer))))
       (when (byte-code-function-p bytecode)
         (funcall bytecode))))
   (apply old-fn args)))
(advice-add (if (progn (require 'json)
                       (fboundp 'json-parse-buffer))
                'json-parse-buffer
              'json-read)
            :around
            #'lsp-booster--advice-json-parse)

(defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
  "Prepend emacs-lsp-booster command to lsp CMD."
  (let ((orig-result (funcall old-fn cmd test?)))
    (if (and (not test?)                             ;; for check lsp-server-present?
             (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
             lsp-use-plists
             (not (functionp 'json-rpc-connection))  ;; native json-rpc
             (executable-find "emacs-lsp-booster"))
        (progn
          (when-let ((command-from-exec-path (executable-find (car orig-result))))  ;; resolve command from exec-path (in case not found in $PATH)
            (setcar orig-result command-from-exec-path))
          (message "Using emacs-lsp-booster for %s!" orig-result)
          (cons "emacs-lsp-booster" orig-result))
      orig-result)))
(advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)
;;;;;;;;;; (end of lsp-booster)

(use-package posframe)

(use-package lsp-treemacs
  ;; https://github.com/emacs-lsp/lsp-treemacs
  ;; Integration between lsp-mode and treemacs and implementation of treeview controls using treemacs as a tree renderer.
  :after (posframe lsp-mode)
  )

(use-package helm-lsp
  ;; https://github.com/mrjosh/helm-ls
  ;; Helm-ls is a helm language server protocol LSP implementation.
  :after (lsp-mode)
  )

;;; ..:: Code linting ::..

(use-package flycheck
  ;; https://github.com/flycheck/flycheck
  ;; On the fly syntax checking for GNU Emacs
  :config
  (defhydra hydra-flycheck (global-map "C-!")
    "Flycheck commands"
    ("e" flycheck-explain-error-at-point "explain")
    ("d" flycheck-display-error-at-point "display")
    ("n" flycheck-next-error "next")
    ("p" flycheck-previous-error "previous")
    ("l" flycheck-list-errors "list")))

;;; ..:: File management ::..

(defun danylo/maintain-xref-history (orig-fun &rest args)
  "Push marker to stack to that M-, works to jump back."
  (xref-push-marker-stack)
  (apply orig-fun args))
(advice-add 'helm-projectile-ag :around #'danylo/maintain-xref-history)

(use-package projectile
  ;; https://github.com/bbatsov/projectile
  ;; Project interaction library offering tools to operate on a project level
  :init (setq projectile-enable-caching 'persistent
              projectile-indexing-method 'hybrid
              projectile-file-exists-remote-cache-expire nil
              projectile-auto-update-cache nil
              projectile-dynamic-mode-line nil)
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map)
              ("C-c p s g" . danylo/helm-projectile-ag-grep)
              ("M-o" . projectile-find-other-file))
  :config
  (setq projectile-globally-ignored-directories
        (append '(".idea"
                  ".eunit"
                  ".git"
                  ".hg"
                  ".svn"
                  ".fslckout"
                  ".bzr"
                  "_darcs"
                  ".tox"
                  "build"
                  "target")
                projectile-globally-ignored-directories)
        projectile-globally-ignored-files
        (append '(".DS_Store" ".gitignore")
                projectile-globally-ignored-files))
  (projectile-mode))

(defun danylo/helm-projectile-ag-grep ()
  "Normally use Ag, and if C-u provided use Grep, to search a
project."
  (interactive)
  (if current-prefix-arg
      (helm-projectile-grep)
    (helm-projectile-ag)))

;;; ..:: Common actions ::..

(defhydra hydra-byte-compile (:hint none)
  "
Compile actions
---------------
_i_: Compile init directory, except the init.el file
_I_: Compile the whole init directory

Essential commands
------------------
_q_: Quit"
  ("i" danylo/byte-compile-init-dir-no-init :exit t)
  ("I" danylo/byte-compile-init-dir :exit t)
  ("q" nil "cancel"))

(defhydra hydra-common-actions (:hint none)
  "
^^File operations                ^^Session config              ^^Coding                     ^^Remote
^^---------------------------    ^^------------------------    ^^-----------------------    ^^--------------
_A_: Copy absolute path          _b_: Byte-compile files       _L_: toggle LSP              _$_: clear cache
_a_: Show absolute path          _l_: Load last saved state    _TAB_: detect indentation
_c_: Copy buffer to clipboard    _s_: Save state               _p_: toggle Projectile
_d_: Diff with saved file
_n_: Copy file name
_R_: Refresh changed buffers

Essential commands
------------------
_q_: Quit"
  ("A" danylo/copy-file-absolute-path :exit t)
  ("a" danylo/show-file-absolute-path :exit t)
  ("c" danylo/copy-whole-file :exit t)
  ("d" (lambda ()
         (interactive)
         (select-window
          (diff-buffer-with-file (current-buffer)))) :exit t)
  ("n" (lambda ()
         (interactive)
         (kill-new (buffer-name))) :exit t)
  ("R" danylo/revert-all-buffers :exit t)
  ("b" hydra-byte-compile/body :exit t)
  ("l" desktop-read :exit t)
  ("s" danylo/desktop-save :exit t)
  ("L" toggle-lsp :exit t)
  ("TAB" dtrt-indent-mode :exit t)
  ("p" projectile-mode :exit t)
  ("$" danylo/clear-tramp-caches :exit t)
  ("q" nil "cancel"))

(general-define-key
 "C-c a" 'hydra-common-actions/body)

;;; ..:: Org ::..

(defun danylo/org-mode-setup ()
  "Configure org mode after loading it"
  (setq org-adapt-indentation nil
        org-hide-leading-stars nil)
  (visual-line-mode t)
  (company-mode t)
  (make-variable-buffer-local 'electric-pair-pairs)
  (add-to-list 'electric-pair-pairs '(?~ . ?~))
  (add-to-list 'electric-pair-pairs '(?$ . ?$))
  ;; Do not fill source code blocks inside docstring
  (make-variable-buffer-local 'filladapt-not-token-table)
  (add-to-list 'filladapt-not-token-table "^$")
  (add-to-list 'filladapt-not-token-table ".*\"\"\"$"))

(use-package org
  ;; https://github.com/bzg/org-mode
  ;; Your life in plain text
  :hook ((org-mode . danylo/org-mode-setup)
         (org-mode . subword-mode))
  :bind (:map org-mode-map
              ("M-." . org-open-at-point)
              ("M-," . org-mark-ring-goto)
              ("C-c C-d" . nil))
  :custom
  (org-image-actual-width nil)
  (org-image-max-width 'fill-column)
  (org-startup-with-inline-images nil)
  :init
  (setq org-startup-folded nil
        ;;org-ellipsis "..." ;; " ▾"
        org-src-tab-acts-natively t
        org-fontify-quote-and-verse-blocks t
        org-src-preserve-indentation t
        ;; LaTeX options
        org-startup-with-latex-preview nil
        org-preview-latex-default-process 'dvisvgm
        ;; minuted setup: https://emacs.stackexchange.com/a/27984/13661
        org-latex-listings 'minted
        org-latex-packages-alist '(("" "minted"))
        org-latex-pdf-process
        '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f")
        ;; Jumping around headings
        org-goto-interface 'outline-path-completionp
        org-outline-path-complete-in-steps nil
        org-imenu-depth 6
        ))

(use-package org-modern
  ;; https://github.com/minad/org-modern
  ;; Modern Org Style.
  :config
  (with-eval-after-load 'org (global-org-modern-mode))
  )

(use-package org-modern-indent
  ;; https://github.com/jdtsmith/org-modern-indent
  ;; Modern block styling with org-indent.
  :quelpa (org-modern-indent :fetcher github
                             :repo "jdtsmith/org-modern-indent")
  :config
  (add-hook 'org-mode-hook #'org-modern-indent-mode 90))

(use-package org-auctex
  ;; https://github.com/karthink/org-auctex
  ;; Better latex previews for org-mode
  :disabled
  :quelpa (org-auctex :fetcher github
                      :repo "karthink/org-auctex"))

(use-package org-download
  ;; https://github.com/abo-abo/org-download
  ;; Drag and drop images to Emacs org-mode.
  :hook ((dired-mode . org-download-enable)))

(require 'org-download)
(defhydra hydra-org-download (:hint none)
  "
Org image download
_v_: paste from clipboard
_s_: take screenshot
_k_: delete

Essential commands
_q_: Quit"
  ("v" org-download-clipboard :exit t)
  ("s" org-download-screenshot :exit t)
  ("k" org-download-delete :exit t)
  ("q" nil "cancel"))
(general-define-key
 :keymaps 'org-mode-map
 "C-c o i" 'hydra-org-download/body)

(defun danylo/math-preview-init ()
  "Startup actions for math preview."
  (when (display-graphic-p)
    ;; (math-preview-all)
    ))

(use-package math-preview
  ;; https://gitlab.com/matsievskiysv/math-preview/
  ;; Emacs preview math inline
  :hook ((org-mode . danylo/math-preview-init)
         (markdown-mode . danylo/math-preview-init))
  :config
  (set-face-attribute 'math-preview-face nil
                      :foreground `,danylo/yellow))

(general-define-key
 :keymaps '(org-mode-map markdown-mode-map)
 "C-c m a" 'math-preview-all
 "C-c m c" 'math-preview-clear-all
 "C-c m ." 'math-preview-at-point
 "C-c m r" 'math-preview-region)

(with-eval-after-load "org"
  (define-key org-mode-map [remap fill-paragraph] nil)
  ;; Make sure buffer motion across windows keys are unaffected
  (define-key org-mode-map (kbd "S-M-<up>") nil)
  (define-key org-mode-map (kbd "S-M-<down>") nil)
  (define-key org-mode-map (kbd "S-M-<left>") nil)
  (define-key org-mode-map (kbd "S-M-<right>") nil)
  ;; LaTeX equations preview style
  (require 'danylo-custom-variables)
  (setq org-format-latex-options (plist-put org-format-latex-options
                                            :scale `,danylo/latex-preview-scale)
        org-format-latex-options (plist-put org-format-latex-options
                                            :foreground `,danylo/yellow)
        org-format-latex-options (plist-put org-format-latex-options
                                            :background "#282c34"))
  ;; Open PDF with Evince
  ;; see: https://stackoverflow.com/a/9116029/4605946
  (setcdr (assoc "\\.pdf\\'" org-file-apps) "evince %s")
  (add-to-list 'org-file-apps
               '("\\.pdf::\\([0-9]+\\)\\'" . "evince -p %1 %s") t)
  ;; Make sure window movement keys are not over-written
  (define-key org-mode-map (kbd "C-S-<up>") 'nil)
  (define-key org-mode-map (kbd "C-S-<down>") 'nil)
  (define-key org-mode-map (kbd "C-S-<left>") 'nil)
  (define-key org-mode-map (kbd "C-S-<right>") 'nil)
  ;; Text emphasis
  (define-key org-mode-map (kbd "C-c f b")
              (lambda () (interactive) (danylo/org-emphasize "*")))
  (define-key org-mode-map (kbd "C-c f i")
              (lambda () (interactive) (danylo/org-emphasize "/")))
  (define-key org-mode-map (kbd "C-c f u")
              (lambda () (interactive) (danylo/org-emphasize "_")))
  (define-key org-mode-map (kbd "C-c f c")
              (lambda () (interactive) (danylo/org-emphasize "~")))
  (define-key org-mode-map (kbd "C-c f e") 'danylo/org-emphasize-equation))

(defun danylo/org-emphasize (char)
  "Emphasize text in org-mode. CHAR is the character to wrap the
  text with."
  (interactive)
  (org-emphasize (string-to-char char)))

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

(defun danylo/org-insert-link (&optional noinsert fill)
  "Prompt user for link name and description, then insert a
link. If NOINSERT is t then only store the link as a string and
return it, but do not print to buffer. If FILL is t then perform
fill after inserting the link."
  (interactive)
  (let ((link (read-string "Reference: "))
        (desc (if (and transient-mark-mode mark-active)
                  (delete-and-extract-region (region-beginning) (region-end))
                (read-string "Description: ")))
        (orig-pos (point))
        new-pos link-text)
    (when (string= "" desc)
      (setq desc link))
    (setq link-text (format "[[%s][%s]]" link desc))
    (if noinsert
        link-text
      (progn
        (insert link-text)
        (when fill
          (setq new-pos (point))
          (fill-region orig-pos new-pos))))))

(defun danylo/org-eqref ()
  "Prompt user for link to an equation reference, then insert link."
  (interactive)
  (insert (format "(%s)" (danylo/org-insert-link t))))

;;; ..:: Git ::..

(use-package magit
  ;; https://github.com/magit/magit
  ;; An interface to the version control system Git
  ;;
  ;; Useful commands:
  ;;   j : in diff view, jump to diff for file at cursor and back up.
  :bind (("C-c v s" . magit-status)
         ("C-c v g" . magit-dispatch)
         ("C-c v f" . magit-file-dispatch)
         ("C-c v r" . magit-toggle-verbose-refresh)
         ("C-c v D" . danylo/magit-diff-range)
         ("C-c v l" . magit-log)
         ("C-c v t" . magit-log-current)
         ("C-c v d" . magit-diff-buffer-file)
         :map magit-file-section-map
         ("RET" . magit-diff-visit-file-other-window)
         ("C-RET" . magit-diff-visit-file)
         :map magit-hunk-section-map
         ("RET" . magit-diff-visit-file-other-window)
         ("C-RET" . magit-diff-visit-file))
  :init (setq magit-display-buffer-function
              #'magit-display-buffer-same-window-except-diff-v1
              magit-refresh-status-buffer nil
              magit-refresh-verbose nil
              )
  :config
  ;; Turn off features that I won't miss when running magit-status.
  (remove-hook 'magit-status-headers-hook 'magit-insert-tags-header)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-pushremote)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-pushremote)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-upstream)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-upstream-or-recent)
  (remove-hook 'magit-status-sections-hook 'magit-insert-untracked-files)

  (defun danylo/magit-diff-range ()
    "Custom function for diffing a commit range."
    (interactive)
    (let ((baseline-commit (read-string "Baseline: " "HEAD^"))
          (compare-commit (read-string "Compare against: " "")))
      (if (string-equal compare-commit "")
          (magit-diff-working-tree baseline-commit)
        (magit-diff-range (concat baseline-commit ".." compare-commit)))))
  )

(with-eval-after-load "magit"
  (set-face-attribute 'magit-branch-current nil
                      :foreground `,danylo/blue
                      :box t
                      :weight 'bold))

;; Make sure commit message open immediately in the Git commit mode.
(require 'git-commit)
(global-git-commit-mode)

;;;; (start patch) Fontify magit buffers. These seem to have trouble fontifying
;;;;               due to jit-lock-deter-time non-nil.
(defconst danylo/magit-fontify-time-interval 0.1
  "Number of seconds between fontification calls.")
(defvar danylo/magit-fontify-current-window nil
  "The window to check repeatedly for fontification.")
(defconst danylo/magit-fontify-max-call-count 15
  "Number of calls for which to fontify matching magit buffers. This
value times `danylo/magit-fontify-time-interval' gives the total
duration of time for which to repeat fontification when the list of
visible buffers changes.")
(defconst danylo/magit-buffer-regexp
  "COMMIT_EDITMSG\\|git-.*"
  "Regexp to match magit buffers that we want to fontify.")
(defvar danylo/magit-fontify-call-count 0
  "Counter from 0 to `danylo/magit-fontify-max-call-count'.")
(defvar danylo/magit-fontify-timer nil
  "Periodic timer to fontify desired magit buffers")
(defun danylo/magit-fontify ()
  "Find magit buffers that we want to fontify, and fontify them."
  (if (and danylo/magit-fontify-current-window
           (< danylo/magit-fontify-call-count
              danylo/magit-fontify-max-call-count))
      (progn
        (setq danylo/magit-fontify-call-count
              (1+ danylo/magit-fontify-call-count))
        (with-selected-window danylo/magit-fontify-current-window
          (let ((buffer (current-buffer)))
            (when (string-match danylo/magit-buffer-regexp (buffer-name buffer))
              (save-excursion
                (condition-case nil
                    (font-lock-fontify-region (window-start) (window-end))
                  (error nil)))))))
    ;; Cancel the timer.
    (cancel-timer danylo/magit-fontify-timer)))
(defun danylo/magit-fontify-reset (frame)
  (unless (minibuffer-window-active-p (selected-window))
    (setq danylo/magit-fontify-call-count 0
          danylo/magit-fontify-current-window (selected-window))
    ;; Start the repeating timer that fontifies the window.
    (when danylo/magit-fontify-timer
      (cancel-timer danylo/magit-fontify-timer))
    (setq danylo/magit-fontify-timer
          (run-with-timer
           danylo/magit-fontify-time-interval
           danylo/magit-fontify-time-interval
           #'danylo/magit-fontify))))
(add-hook 'window-buffer-change-functions #'danylo/magit-fontify-reset)
;;;; (end patch)

(use-package why-this
  ;; https://codeberg.org/akib/emacs-why-this
  ;; Why the current line contains this?
  :bind (("C-c w /" . why-this)
         ("C-c w ?" . why-this-mode)
         ("C-c w a" . why-this-annotate))
  :custom
  (why-this-idle-delay 0.05)
  :config
  (set-face-attribute 'why-this-face nil :foreground `,danylo/light-gray))

;;; ..:: Shell interaction ::..

(defun danylo/shell~check-open (shell-buffer-name)
  "Check if a shell is running."
  (if (get-buffer shell-buffer-name) t nil))

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
          (let ((vterm-shell shell-type))
            (vterm shell-buffer-name)))
      ;; Create shell in new window
      (let ((this-window (selected-window))
            (new-window (split-window-vertically)))
        (select-window new-window)
        (let ((vterm-shell shell-type))
          (vterm shell-buffer-name))
        (select-window this-window)))))

(defun danylo/shell-exec (shell-buffer-name command)
  "Run command in the shell.
If there is no shell open, prints a message to inform."
  (if (danylo/shell~check-open shell-buffer-name)
      ;; Send a run command for the current file
      (with-current-buffer shell-buffer-name
        (vterm-copy-mode -1)
        (vterm-send-string (format "%s\n" command)))
    (message "No shell open.")))

(defun danylo/shell-get-point (shell-buffer-name)
  "Get the current location of point in the shell."
  (let ((current-point nil))
    (if (danylo/shell~check-open shell-buffer-name)
        (with-current-buffer shell-buffer-name
          (vterm-copy-mode 1)
          (setq current-point (point))
          (vterm-copy-mode -1))
      (message "No shell open."))
    current-point))

(defun danylo/shell-get-content (shell-buffer-name start end)
  "Get the shell text between START and END point positions."
  (setq shell-content "")
  (if (danylo/shell~check-open shell-buffer-name)
      (with-current-buffer shell-buffer-name
        (vterm-copy-mode 1)
        (let ((start (max start (point-min)))
              (end (min end (point-max))))
          (setq shell-content (buffer-substring start end)))
        (vterm-copy-mode -1))
    (message "No shell open."))
  shell-content)

;;;; Open files manually, rather than through xref

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

(defun danylo/xref-find-definition-with-ag-fallback (orig-fun &rest args)
  "Use AG as a fallback in case xref standard command fails to
find a definion."
  (condition-case nil
      (apply orig-fun args)
    (user-error (helm-projectile-ag))))
(advice-add 'xref-find-definitions :around
            #'danylo/xref-find-definition-with-ag-fallback)

;;; ..:: Calculator ::..

(use-package literate-calc-mode
  ;; https://github.com/sulami/literate-calc-mode.el
  ;; Literate programming for M-x calc.
  :custom
  (literate-calc-usimplify-results t)
  :config
  (add-to-list 'auto-mode-alist '("\\.calc" . literate-calc-mode)))

(defhydra hydra-literate-calc (:hint none)
  "
Literate calc:
_i_: insert results
_d_: delete results
_c_: copy result to clipboard

Essential commands
_q_: Quit"
  ("i" literate-calc-insert-results :exit t)
  ("d" literate-calc-remove-results :exit t)
  ("c" literate-calc-kill-result :exit t)
  ("q" nil "cancel"))
(general-define-key
 :keymaps 'literate-calc-mode-map
 "C-c l c" 'hydra-literate-calc/body)

;;; ..:: C/C++ ::..

(add-hook 'c-mode-common-hook
          (lambda ()
            (setq c-auto-newline nil)
            (c-set-offset 'arglist-cont 'c-lineup-arglist)
            (c-set-offset 'arglist-cont-nonempty 'c-lineup-arglist)
            (eldoc-mode -1)))

(use-package modern-cpp-font-lock
  ;; https://github.com/ludwigpacifici/modern-cpp-font-lock
  ;; C++ font-lock for Emacs
  :disabled
  :hook ((c++-mode . modern-c++-font-lock-mode)))

(use-package highlight-doxygen
  ;; https://github.com/Lindydancer/highlight-doxygen
  ;; Highlight Doxygen comments in Emacs, including code blocks
  :config
  (set-face-attribute 'highlight-doxygen-comment nil :background 'unspecified)
  (set-face-attribute 'highlight-doxygen-code-block nil
                      :background 'unspecified)
  (highlight-doxygen-global-mode 1)
  )

(use-package yasnippet
  ;; https://github.com/joaotavora/yasnippet
  ;; A template system for Emacs
  :init
  (setq yas-snippet-dirs `(,(danylo/make-path "lisp/snippets")))
  :hook ((prog-mode . yas-minor-mode)
         (text-mode . yas-minor-mode))
  )

(use-package helm-c-yasnippet
  ;; https://github.com/emacs-jp/helm-c-yasnippet
  ;; Helm source for yasnippet
  :bind (("C-M-y" . helm-yas-complete))
  :custom
  (helm-yas-space-match-any-greedy t)
  (helm-yas-display-key-on-candidate t)
  )

(use-package yasnippet-capf
  ;; https://github.com/elken/yasnippet-capf
  ;; Completion-At-Point Extension for YASnippet.
  :after cape
  :hook ((lsp-mode . danylo/yasnippet-capf-h)
         (prog-mode . danylo/yasnippet-capf-h)
         (text-mode . danylo/yasnippet-capf-h))
  :init
  (defun danylo/yasnippet-capf-h ()
    (add-to-list 'completion-at-point-functions #'yasnippet-capf))
  )

(require 'yasnippet)
(yas-reload-all)
(add-to-list 'auto-mode-alist '("\\.yasnippet" . snippet-mode))

(defun company-mode/backend-with-yas (backend)
  (if (and (listp backend) (member 'company-yasnippet backend))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))

;; helm-company choose from company completions with C-:
(with-eval-after-load 'company
  (define-key company-mode-map (kbd "C-:") 'helm-company)
  (define-key company-active-map (kbd "C-:") 'helm-company))

;; Set the global company backends.
(setq company-backends
      (mapcar #'company-mode/backend-with-yas company-backends))

;;;; Indentation style.

(use-package danylo-code-styles
  ;; Coding style definitions.
  :ensure nil
  :load-path danylo/emacs-custom-lisp-dir)

(c-add-style "danylo-cpp-style" danylo/cpp-style)

(use-package cc-mode
  ;; Emacs C-style language mode
  :ensure t
  :after (danylo-code-styles)
  :bind (:map c-mode-base-map
              ("C-c f o" . ff-find-other-file)
              :map c++-ts-mode-map
              ("M-q" . nil))
  :hook ((c-mode-common . yas-minor-mode)
         (c-mode-common . subword-mode)
         (c-mode-common
          . (lambda ()
              (c-ts-mode-set-style 'danylo/cpp-ts-style)
              ;; Take the following from `c-comment-prefix-regexp', otherwise
              ;; the filladapt-token-table will have a (nil c-comment) entry
              ;; that results in an error when trying to fill paragraphs.
              (setq-local c-current-comment-prefix "//+\\|\\**")
              (c-setup-filladapt)
              )))
  :init
  ;; (add-to-list 'c-ts-mode-indent-style)
  (setq
   ;; Legacy modes.
   c-default-style '((other . "danylo-cpp-style")))
  ;; Not needed as we are using LSP.
  (remove-hook 'c-mode-common-hook 'semantic-mode)
  )

(use-package clang-format
  ;; https://github.com/emacsmirror/clang-format
  ;; Format code using clang-format
  ;; DEPRECATED: replaced with apheleia package.
  :disabled
  )

(use-package clang-format+
  ;; https://github.com/SavchenkoValeriy/emacs-clang-format-plus
  ;; Emacs minor mode for automatic clang-format application
  ;; DEPRECATED: replaced with apheleia package.
  :disabled
  :init
  (add-hook 'c-mode-common-hook #'clang-format+-mode)
  (setq clang-format+-always-enable t)
  )

;;; ..:: Python ::..

;;;; Code editing

(use-package python
  ;; Emacs built-in
  ;; Python editing major mode
  :ensure nil
  :hook ((python-mode . yas-minor-mode)
         (python-mode . subword-mode))
  :bind (:map python-ts-mode-map
              ;; Disabled keybindings.
              ("C-c C-d" . nil)
              ;; Make indentation automatic, especially ensures correct
              ;; indentation in docstrings (see
              ;; https://emacs.stackexchange.com/a/28445/13661)
              ("RET" . 'newline-and-indent)
              ;; The python-indent-dedent-line-backspace function causes
              ;; unintuitive deletion of code via backspace. It's unnecessary
              ;; headache since I de-indent with Shift+Tab.
              ("DEL" . 'python-indent-dedent-line-backspace)
              ;; ("TAB" . 'python-indent-shift-right)
              ;; ("<backtab>" . 'python-indent-shift-left)
              )
  :init (setq python-indent-guess-indent-offset t
              python-indent-guess-indent-offset-verbose nil
              python-fill-docstring-style 'pep-257-nn)
  (remove-hook 'python-mode-hook 'semantic-mode)
  (remove-hook 'python-mode-hook 'semantic))

(use-package python-black
  ;; https://github.com/wbolster/emacs-python-black
  ;; Emacs package to reformat Python using black-macchiato.
  ;; DEPRECATED: replaced with apheleia package.
  :disabled
  :demand t
  :after python
  :bind (:map python-ts-mode-map
              ("C-M-f" . 'python-black-buffer)
              ("C-M-r" . 'python-black-region))
  :init (setq python-black-extra-args '("--required-version" "24.10.0")))

;;;; LSP setup

(use-package lsp-pyright
  ;; https://github.com/emacs-lsp/lsp-pyright
  ;; Microsoft's python language server.
  :after (lsp-mode)
  :ensure t
  :init (setq lsp-pyright-multi-root nil
              ;; lsp-enable-file-watchers nil
              )
  :custom (lsp-pyright-langserver-command "pyright") ;; or basedpyright
  :hook (python-ts-mode
         . (lambda ()
             (require 'lsp-pyright)
             (setq-local lsp-signature-auto-activate nil
                         lsp-signature-render-documentation nil))))

;;;; Imenu setup

(defun danylo/python-imenu ()
  (let ((python-imenu (imenu--generic-function
                       imenu-generic-expression)))
    (append python-imenu)))

(defun danylo/python-imenu-hooks ()
  (setq imenu-generic-expression
        '(("f(x) " "^[[:blank:]]*def \\(.*\\).*(.*$" 1)
          ("class" "^class \\(.*\\).*:$" 1)
          ("§    " "^\s*[#]+\s+\\.\\.::\s+\\(.*\\)\s+::\\.\\.\s*$" 1)))
  ;; (setq imenu-create-index-function 'danylo/python-imenu)
  (setq imenu-create-index-function 'python-imenu-create-index)
  ;; Rescan the buffer as contents are added
  (setq imenu-auto-rescan t)
  )

(add-hook 'python-ts-mode-hook 'danylo/python-imenu-hooks)

;;;; Python shell interaction
(defvar-local danylo/custom-python-shell nil
  "Set locally to a custom path of ipython that you'd like to execute.")

(defun danylo/python-shell-open (arg)
  "Open a Python shell."
  (interactive "P")
  (let ((python-shell-type (or danylo/custom-python-shell
                               danylo/python-shell-type)))
    (if arg
        ;; Open in current window
        (danylo/shell-open danylo/python-buffer-name python-shell-type t)
      ;; Open in new window
      (danylo/shell-open danylo/python-buffer-name python-shell-type))))

(defun danylo/python-shell-run-file ()
  "Run current Python file."
  (interactive)
  (let* ((file-name (file-name-nondirectory (buffer-file-name)))
         (command (format "%%run -i %s" file-name)))
    (danylo/shell-exec danylo/python-buffer-name command)
    ))

(defun danylo/smart-select-region (start end)
  "Select region in file, removing possible indent of all
lines according to the first line."
  (interactive "r")
  (if (use-region-p)
      (let* ((selection (buffer-substring start end))
             (starting-spaces (progn (string-match "^\s*" selection)
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

(defconst python-paste-refresh-rate 0.1
  "How often to check IPython REPL if %paste has finished.")

(defun danylo/python-shell-run-region (start end)
  "Run highlighted selection in file."
  (interactive "r")
  (let ((command (danylo/smart-select-region start end)))
    (deactivate-mark)
    (when command
      (let ((kill-ring-top  (car kill-ring))
            (start-point (danylo/shell-get-point
                          danylo/python-buffer-name)))
        (kill-new command t)
        (danylo/shell-exec danylo/python-buffer-name "%paste")
        ;; Wait until Ipython has copied over the text
        (run-with-timer
         python-paste-refresh-rate nil
         'danylo/python-restore-kill-ring-after-paste
         kill-ring-top start-point)
        )
      )))

(defun danylo/python-restore-kill-ring-after-paste (kill-ring-top start-point)
  "Restore the kill ring top item after %paste in IPython."
  (let* ((end-point (danylo/shell-get-point
                     danylo/python-buffer-name))
         (ipython-pasted-text
          (danylo/shell-get-content danylo/python-buffer-name
                                    start-point end-point))
         (ipython-paste-end
          (string-match "^## -- End pasted text --$"
                        ipython-pasted-text)))
    (if ipython-paste-end
        (kill-new kill-ring-top t)
      (run-with-timer
       python-paste-refresh-rate nil
       'danylo/python-restore-kill-ring-after-paste
       kill-ring-top start-point)
      )))

(defun danylo/python-config ()
  ;; Key bindings
  (define-key python-ts-mode-map (kbd "C-c C-s") 'danylo/python-shell-open)
  (define-key python-ts-mode-map (kbd "C-c C-f") 'danylo/python-shell-run-file)
  (define-key python-ts-mode-map (kbd "C-c C-r") 'danylo/python-shell-run-region)
  (define-key python-ts-mode-map (kbd "C-c C-p") nil))
(add-hook 'python-ts-mode-hook (lambda () (danylo/python-config)))

;;; ..:: Julia ::..

(use-package julia-mode
  ;; https://github.com/JuliaEditorSupport/julia-emacs
  ;; Major mode for the julia programming language
  :hook ((julia-mode . yas-minor-mode)
         (julia-mode . (lambda ()
                         (make-variable-buffer-local 'electric-pair-text-pairs)
                         (add-to-list 'electric-pair-text-pairs '(?` . ?`))
                         ;; Do not fill source code blocks inside docstring
                         (make-variable-buffer-local 'filladapt-not-token-table)
                         (add-to-list 'filladapt-not-token-table "^$")
                         (add-to-list 'filladapt-not-token-table ".*\"\"\"$")
                         )))
  :bind (:map julia-mode-map
              ("C-h ." . danylo/julia-help-at-point)
              ("C-c f b" . danylo/docstring-insert-bold)
              ("C-c f i" . danylo/docstring-insert-italic)
              ("C-c f c" . danylo/docstring-insert-literal)
              ("C-c f e" . danylo/docstring-insert-equation)))

(defun danylo/ignore-triple-backticks (orig-fun &rest args)
  "Patch the Julia syntax coloring so that triple back ticks are
ignored and not treated as strings. I use them as srouce code
blocks."
  (let* ((ppss (save-excursion (syntax-ppss (match-beginning 0))))
         (string-open (and (not (nth 4 ppss)) (nth 8 ppss))))
    (unless (or (eq (char-before) ?`) (eq (char-after string-open) ?`))
      (apply orig-fun args))))
(advice-add 'julia-syntax-stringify :around #'danylo/ignore-triple-backticks)

;;;; Julia shell interaction

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

(defun danylo/julia-shell-run-file ()
  "Run current Julia file."
  (interactive)
  (let* ((file-name (file-name-nondirectory (buffer-file-name)))
         (command (format "include(\"%s\");" file-name)))
    (danylo/shell-exec danylo/julia-buffer-name command)
    ))

(defun danylo/julia-shell-run-region (start end)
  "Run highlighted selection in file."
  (interactive "r")
  (let ((command (danylo/smart-select-region start end)))
    (when command
      (setq command (format "%s\n" command))
      (danylo/shell-exec danylo/julia-buffer-name command))))

(defun danylo/julia-config ()
  ;; Key bindings
  (define-key julia-mode-map (kbd "C-c C-s") 'danylo/julia-shell-open)
  (define-key julia-mode-map (kbd "C-c C-f") 'danylo/julia-shell-run-file)
  (define-key julia-mode-map (kbd "C-c C-r") 'danylo/julia-shell-run-region)
  (define-key julia-mode-map (kbd "C-c C-l") 'julia-latexsub-or-indent))
(add-hook 'julia-mode-hook (lambda () (danylo/julia-config)))

;;;; Get help at point

(defconst julia-docstring-delay 0.5
  "After what time to check Julia REPL if docstring has finished printing.")

(defconst julia-docstring-refresh-rate 0.1
  "How often to check Julia REPL if docstring has finished printing.")

(defun danylo/julia-help-at-point (&optional start end)
  "Query Julia REPL for help about thing at point."
  (interactive (if (use-region-p) (list (region-beginning) (region-end))))
  (let ((thing (if start (buffer-substring start end)
                 (thing-at-point 'symbol)))
        (start-point (danylo/shell-get-point danylo/julia-buffer-name)))
    (when start-point
      ;; Record the help string
      (danylo/shell-exec danylo/julia-buffer-name (format "?%s" thing))
      (run-with-timer julia-docstring-delay nil
                      'danylo/julia-show-help-docstring start-point)
      )
    ))

(defun danylo/julia-show-help-docstring (start-point)
  "Record the docstring output by the Julia REPL and put it into a help buffer.
Calls itself until the docstring has completed printing."
  (let* (;; Get end point, removing the final "julia> " prompt
         (end-point (danylo/shell-get-point danylo/julia-buffer-name))
         ;; Get docstring printed thus far
         (julia-help-docstring
          (danylo/shell-get-content danylo/julia-buffer-name
                                    start-point end-point))
         ;; Check if the document printout finished
         (julia-help-docstring-end
          (string-match "^julia>\s$" julia-help-docstring)))
    (if julia-help-docstring-end
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
            (insert (substring julia-help-docstring 0 julia-help-docstring-end))
            (special-mode)
            (danylo-julia-help-mode)
            (goto-char (point-min))
            ;; Go back to original window
            (select-window this-window)
            ;; Return to help window. Quitting will return cursor to original
            ;; window
            (select-window new-window)))
      ;; Run again in a little while
      (run-with-timer julia-docstring-refresh-rate nil
                      'danylo/julia-show-help-docstring start-point))))

(define-minor-mode danylo-julia-help-mode
  "Minor mode for Julia help buffer."
  :init-value nil
  :lighter " danylo-julia-help"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "q") 'kill-buffer-and-window)
            map)
  (read-only-mode +1))

;;;; Imenu setup

(defun danylo/julia-imenu-hooks ()
  (setq imenu-generic-expression
        '(("f(x)   " "^[[:blank:]]*function \\(.*\\).*(.*$" 1)
          ("f(x)   " "^\\([a-zA-Z0-9_\\.!]+?\\)\s*(.*?\\(?:).*=.*\\|,\\|{\\)$" 1)
          ("@      " "^[[:blank:]]*macro \\(.*\\).*(.*$" 1)
          ("struct " "^[^#]*\s+struct\s+\\(.*?\\)$" 1)
          ("struct " "^struct\s+\\(.*?\\)$" 1)
          ("import " "^\s*import\s+\\([a-zA-Z\\.,:\s]*\\)$" 1)
          ("include" "^\s*include\s*(\"\\([a-zA-Z/\\.,:\s]*\\)\")$" 1)
          ("using  " "^\s*using\s+\\([a-zA-Z\\.,:\s]*\\)$" 1)
          ("export " "^\s*export\s+\\([a-zA-Z\\.,:!@\s]*\\)$" 1)
          ("const  " "^\s*const\s+\\(.*?\\)\s*=.*$" 1)
          ("§      " "^\s*[#]+\s+\\.\\.::\s+\\(.*\\)\s+::\\.\\.\s*$" 1)
          ))
  (setq imenu-create-index-function
        (lambda () (imenu--generic-function imenu-generic-expression)))
  ;; Rescan the buffer as contents are added
  (setq imenu-auto-rescan t))

(add-hook 'julia-mode-hook 'danylo/julia-imenu-hooks)

;;;; Commenting and documentation

(defun danylo/julia-block-comment ()
  "Julia block comment."
  (interactive)
  (danylo/section-msg "#= " "" " =#" t))

(defun danylo/julia-function-docstring-short ()
  "A short Julia function docstring."
  (interactive)
  (if current-prefix-arg
      ;; Inline version
      (danylo/section-msg "\"\"\" " "" " \"\"\"" t)
    ;; Multiline version
    (danylo/section-msg "\"\"\"\n" "" "\n\"\"\"" t)))

(defun danylo/julia-function-docstring ()
  "Julia block comment."
  (interactive)
  (let ((docstring '("\"\"\""
                     "    Signature"
                     ""
                     "Description."
                     ""
                     "# Arguments"
                     "- `foo`: description."
                     ""
                     "# Returns"
                     "- `bar`: description."
                     "\"\"\""))
        (indent-string-prefix (make-string (current-indentation) ?\s))
        (first t)
        prefix-newline)
    (save-excursion
      (mapc (lambda (line-string)
              (setq prefix-newline (if first "" "\n"))
              (setq prefix-indent (if first "" indent-string-prefix))
              (insert (format "%s%s%s"
                              prefix-newline
                              prefix-indent
                              line-string))
              (setq first nil))
            docstring))
    (re-search-forward "Signature" nil nil 1)
    ))

(defun danylo/docstring-insert-style (left right &optional display)
  "Insert specified (e.gg, markdown) delimiters into a docstring."
  (let (orig-pos indent-string-prefix)
    (if (and transient-mark-mode mark-active)
        (let ((text (delete-and-extract-region
                     (region-beginning) (region-end))))
          (if display
              (progn
                (setq indent-string-prefix
                      (make-string (current-indentation) ?\s))
                (insert (format "%s\n%s" left indent-string-prefix))
                (insert text)
                (setq orig-pos (point))
                (insert (format "\n%s%s" indent-string-prefix right))
                (goto-char orig-pos))
            (progn
              (insert (format "%s%s" left text))
              (setq orig-pos (point))
              (insert right)
              (goto-char orig-pos))))
      (if display
          (progn
            (setq indent-string-prefix
                  (make-string (current-indentation) ?\s))
            (insert (format "%s\n%s" left indent-string-prefix))
            (setq orig-pos (point))
            (insert (format "\n%s%s" indent-string-prefix right))
            (goto-char orig-pos))
        (progn
          (insert (format "%s%s" left right))
          (backward-char (length right)))))))

(defalias 'danylo/docstring-insert-bold
  (lambda () (interactive) (danylo/docstring-insert-style "**" "**"))
  "Insert bold text delimiters.")

(defalias 'danylo/docstring-insert-italic
  (lambda () (interactive) (danylo/docstring-insert-style "*" "*"))
  "Insert italic text delimiters.")

(defalias 'danylo/docstring-insert-literal
  (lambda ()
    (interactive)
    (if current-prefix-arg
        (danylo/docstring-insert-style "```julia" "```" t)
      (danylo/docstring-insert-style "`" "`")))
  "Insert inline or display source code delimiters.")

(defalias 'danylo/docstring-insert-equation
  (lambda ()
    (interactive)
    (if current-prefix-arg
        (danylo/docstring-insert-style "```math" "```" t)
      (danylo/docstring-insert-style "``" "``")))
  "Insert inline or display math delimiters.")

;;; ..:: Lisp ::..

(add-to-list 'auto-mode-alist '("\\.el" . emacs-lisp-mode))
(add-to-list 'auto-mode-alist '("\\.el.gz" . emacs-lisp-mode))

;; Turn off (by default) function signatures popping up in minibuffer
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (eldoc-mode -1)))

;;;; Imenu setup
(defun danylo/lisp-imenu-hooks ()
  (setq imenu-generic-expression
        '(("var   " "^\s*(\\(def\\(?:c\\(?:onst\\(?:ant\\)?\\|ustom\\)\\|ine-symbol-macro\\|parameter\\)\\)\s+\\(\\(?:\\sw\\|\\s_\\|\\\\.\\)+\\)" 2)
          ("var   " "^\s*(defvar\\(?:-local\\)?\s+\\(\\(?:\\sw\\|\\s_\\|\\.\\)+\\)" 1)
          ("f(x)  " "^(defun\s+\\([a-zA-Z0-9/\\:\\-]*?\\)\s+(.*$" 1)
          ("pkg   " "^\s*(use-package\s*\\([a-zA-Z0-9\\-]*\\)$" 1)
          ("§     " "^[;]+\s+\\.\\.::\s+\\(.*\\)\s+::\\.\\." 1)
          ("patch " "^;;;; (start patch)\s\\(.*\\)" 1)))
  (setq imenu-create-index-function
        (lambda () (imenu--generic-function imenu-generic-expression)))
  ;; Rescan the buffer as contents are added
  (setq imenu-auto-rescan t))

(add-hook 'emacs-lisp-mode-hook 'danylo/lisp-imenu-hooks)

;;; ..:: LaTeX ::..

(defun danylo/symbol-at-point-with-special ()
  "Get the symbol at current point, with special characters that I
tend to use in LaTeX (_, @, :, \\). Useful for swooping inside
LaTeX document."
  (interactive)
  (let ((table (copy-syntax-table (syntax-table))))
    (modify-syntax-entry ?_ "w" table)
    (modify-syntax-entry ?@ "w" table)
    (modify-syntax-entry ?: "w" table)
    (modify-syntax-entry ?\\ "w" table)
    (with-syntax-table table
      (thing-at-point 'symbol))))

(use-package tex
  ;; https://elpa.gnu.org/packages/auctex.html
  ;; Package for writing and formatting TeX files
  :ensure auctex
  :hook ((LaTeX-mode . LaTeX-math-mode)
         (LaTeX-mode . turn-on-reftex)
         (LaTeX-mode . TeX-source-correlate-mode)
         (LaTeX-mode . TeX-PDF-mode)
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
                         (LaTeX-add-environments "subequations")
                         (LaTeX-add-environments "align")
                         ;; Line-breaking math
                         (add-to-list 'fill-nobreak-predicate 'texmathp)
                         ;; Helm for candidate completion
                         (require 'helm-mode)
                         (add-to-list 'helm-completing-read-handlers-alist
                                      '(LaTeX-environment
                                        . helm-completing-read-default-handler))
                         ;; Helm swoop include underscores
                         (setq-local helm-swoop-pre-input-function
                                     'danylo/symbol-at-point-with-special)
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
              TeX-insert-macro-default-style 'mandatory-args-only
              LaTeX-beamer-item-overlay-flag nil
              TeX-electric-math '("$" . "$")
              texmathp-tex-commands '(("optimization" env-on))
              reftex-plug-into-AUCTeX nil)
  (setq-default TeX-master nil)
  :bind (:map LaTeX-mode-map
              ("C-c i w" . ispell-word)
              ("C-x C-DEL" . electric-pair-delete-pair)
              ("C-c f e" . danylo/org-emphasize-equation)
              ("C-c x c" . reftex-citep)
              ("C-c s" . nil)
              ("C-c x b" . (lambda ()
                             (interactive)
                             (TeX-command "LaTeX" 'TeX-master-file)))
              ("C-c x v" . TeX-view))
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
  (add-to-list 'LaTeX-verbatim-environments "minted")
  (add-to-list 'LaTeX-verbatim-environments "pycode")
  (add-to-list 'LaTeX-verbatim-environments "pykzmathinline")
  (add-to-list 'LaTeX-verbatim-environments "pykzmathblock")
  (add-to-list 'LaTeX-verbatim-environments "@pie@shell")
  (add-to-list 'LaTeX-indent-environment-list '("pycode" current-indentation))
  (add-to-list 'LaTeX-indent-environment-list '("pykzmathblock" current-indentation))
  (add-to-list 'LaTeX-indent-environment-list '("@pie@shell" current-indentation)))

(use-package bibtex
  ;; http://www.jonathanleroux.org/bibtex-mode.html
  ;; Package for writing and formatting BibTeX files
  :ensure bibtex
  :bind ((:map bibtex-mode-map
               ("M-q" . 'bibtex-fill-entry)))
  :init (setq bibtex-align-at-equal-sign t))

(defun danylo/TeX-dwim-master (orig-fun &rest args)
  "Find a likely `TeX-master'.
Patched so that any new file by default is guessed as being its own master."
  nil)
(advice-add 'TeX-dwim-master :around #'danylo/TeX-dwim-master)

(use-package company-auctex
  ;; https://github.com/alexeyr/company-auctex
  ;; company-mode autocompletion for auctex
  :after (company latex)
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

(use-package company-reftex
  ;; https://github.com/TheBB/company-reftex
  ;; RefTeX backends for company-mode
  :after (company tex)
  :hook ((LaTeX-mode
          . (lambda ()
              (setq company-backends
                    (append
                     '(company-reftex-labels company-reftex-citations)
                     company-backends))))))

;;;; Imenu setup
(defun danylo/tex-imenu-hooks ()
  (setq imenu-generic-expression
        '(("::  " "^\s*[%]+\s+\\.\\.::\s+\\(.*\\)\s+::\\.\\." 1)
          ("§§§ " "^\s*\\\\subsubsection{\\(.*\\)}" 1)
          ("§§  " "^\s*\\\\subsection{\\(.*\\)}" 1)
          ("§   " "^\s*\\\\section{\\(.*\\)}" 1)
          ))
  (setq imenu-create-index-function
        (lambda () (imenu--generic-function imenu-generic-expression)))
  ;; Rescan the buffer as contents are added
  (setq imenu-auto-rescan t))

(add-hook 'LaTeX-mode-hook 'danylo/tex-imenu-hooks)

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

;;; ..:: YAML, JSON, etc. ::..

(use-package yaml-mode
  ;; https://github.com/yoshiki/yaml-mode
  ;; Major mode for editing files in the YAML data serialization format.
  :bind (:map yaml-ts-mode-map
              ("C-c t i" . danylo/imenu-list-jump))
  :mode (("\\.yml$" . yaml-mode)
         ("\\.yaml$" . yaml-mode)))

(use-package yaml-imenu
  ;; https://github.com/emacsmirror/yaml-imenu
  ;; Enhancement of the imenu support in yaml-mode.
  :after yaml-mode
  :config
  (yaml-imenu-enable))

(defun which-function-from-imenu-index ()
  "Call the imenu-index part in `which-function'.

It is a fallback for when which-func-functions and `add-log-current-defun' return nil."
  (let (which-func-functions)
    (letf (((symbol-function 'add-log-current-defun)
            (lambda () nil)))
          (which-function))))

;; `add-log-current-defun' returns a not so meaningful result in some
;; major modes when the default `add-log-current-defun-function'
;; happens to match a random line that is not really a function
;; definition.  It is often much more desirable to find a function
;; name from an imenu index in those modes.  Results are also used by
;; `which-function-mode'.
(defun enable-add-log-current-defun-using-which-function ()
  (setq-local add-log-current-defun-function 'which-function-from-imenu-index))

(add-hook 'yaml-mode-hook
          'enable-add-log-current-defun-using-which-function)

(use-package jsonnet-mode
  ;; https://github.com/tminor/jsonnet-mode
  ;; Emacs major mode for editing Jsonnet files.
  )

;;; ..:: Markdown ::..

(defun danylo/insert-liquid (&optional tag is-block markup)
  "Insert a Liquid tag or block."
  (interactive)
  ;; Get Liquid tag or block definition
  (let* ((liquid-type (if current-prefix-arg "Block" "Tag"))
         (liquid-tag (if tag
                         tag
                       (read-string (format "%s: " liquid-type))))
         liquid-markup)
    (when (or is-block current-prefix-arg)
      (setq liquid-markup (if is-block markup (read-string "Markup: "))))
    ;; Print the Liquid tag or block
    (if (or tag current-prefix-arg)
        (progn
          (insert (format "{%% %s %s %%}\n\n{%% end%s %%}"
                          liquid-tag liquid-markup liquid-tag))
          (forward-line -1))
      (progn
        (insert (format "{%% %s %%}" liquid-tag))
        (re-search-backward "{% " nil t)
        (re-search-forward (format "{%% %s" liquid-tag) nil t)))
    )
  )

(defun danylo/insert-code ()
  "Insert a Liquid code block."
  (interactive)
  (let* ((language-name (read-string "Language: "))
         (liquid-markup (format "%s linenos" language-name)))
    (danylo/insert-liquid "highlight" t liquid-markup)
    ))

(defun danylo/insert-latex-math-mode ()
  "Insert latex math mode environment."
  (interactive)
  (danylo/insert-liquid "latexmm" t ""))

(use-package markdown-mode
  ;; https://github.com/jrblevin/markdown-mode
  ;; Markdown editing major mode
  :after (latex)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :bind (:map markdown-mode-map
              ("C-c f e" . danylo/org-emphasize-equation)
              ("C-c i l" . danylo/insert-liquid)
              ("C-c i c" . danylo/insert-code)
              ("C-c i m m" . danylo/insert-latex-math-mode)
              :map LaTeX-mode-map
              ("C-c i l" . danylo/insert-liquid))
  :init (setq markdown-command "multimarkdown")
  (add-hook 'markdown-mode-hook
            (lambda ()
              (filladapt-mode -1)
              (eldoc-mode -1)
              (setq-local markdown-enable-math t))))

(use-package poly-markdown
  ;; https://github.com/polymode/polymode
  ;; Framework for Multiple Major Modes in Emacs
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.md" . poly-markdown-mode))
  (setq poly-lock-allow-background-adjustment nil)
  :config
  (define-innermode poly-markdown-latex-innermode
    :mode 'LaTeX-mode
    :head-matcher "^{% latex .* %}\n"
    :tail-matcher "^{% endlatex *%}\n"
    :head-mode 'host
    :tail-mode 'host)
  (define-innermode poly-markdown-code-innermode
    :mode 'text-mode
    :head-matcher "^{%\s*highlight.*%}\n"
    :tail-matcher "^{%\s*endhighlight\s*%}\n"
    :head-mode 'host
    :tail-mode 'host)
  (define-polymode poly-markdown-mode
    :hostmode 'poly-markdown-hostmode
    :innermodes '(poly-markdown-latex-innermode
                  poly-markdown-code-innermode)))

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

;;; ..:: Graphviz ::..

(use-package graphviz-dot-mode
  ;; https://github.com/ppareit/graphviz-dot-mode
  ;; Emacs mode for the DOT language, used by graphviz.
  :ensure t
  :config
  (setq graphviz-dot-indent-width 4))

;;; ..:: Jinja ::..

(use-package jinja2-mode
  ;; https://github.com/paradoxxxzero/jinja2-mode
  ;; Jinja2 mode for emacs.
  :bind (:map jinja2-mode-map
              ;; Disabled keybindings.
              ("C-c C-d" . nil)
              ("C-c t" . nil))
  :init
  (add-to-list 'auto-mode-alist '("\\.j2.*" . jinja2-mode)))

;;; ..:: Astro (web) ::..

(use-package web-mode
  ;; https://web-mode.org/
  ;; Major-mode for editing web templates.
  :quelpa (web-mode :fetcher github
                    :repo "fxbois/web-mode"
                    :branch "master")
  :bind (:map web-mode-map
              ("C-c C-d n" . nil))
  :custom
  (web-mode-enable-css-colorization nil))

;; Astro major mode.
(define-derived-mode astro-mode web-mode "Astro")
(add-to-list 'auto-mode-alist '("\\.astro" . astro-mode))
(add-to-list 'apheleia-mode-alist '(astro-mode . prettier-html))

(add-hook
 'astro-mode-hook
 (lambda ()
   (add-to-list
    'colorful-extra-color-keyword-functions
    '(astro-mode
      . (colorful-add-css-variables-colors
         colorful-add-rgb-colors
         colorful-add-hsl-colors
         colorful-add-oklab-oklch-colors
         colorful-add-color-names)))))

;;; ..:: CSV ::..

(use-package danylo-csv
  ;; CSV file editing.
  :load-path danylo/emacs-custom-lisp-dir
  :config
  ;; Highlight the current line, which makes the full CSV row obvious even when
  ;; the line is wrapped.
  (add-hook 'danylo-csv-mode-hook #'hl-line-mode))

;;; ..:: Bazel ::..

(use-package bazel
  ;; https://github.com/bazelbuild/emacs-bazel-mode
  ;; Emacs mode for Bazel.
  )

;;; ..:: PDF viewing ::..

(use-package pdf-tools
  ;; https://github.com/vedang/pdf-tools
  ;; Emacs support library for PDF files.
  :hook ((pdf-view-mode . pdf-view-midnight-minor-mode))
  :bind (:map pdf-view-mode-map
              ("C-s" . isearch-forward))
  :custom
  (pdf-view-resize-factor 1.1)
  (pdf-view-display-size 'fit-page)
  :init
  (pdf-tools-install)
  (pdf-loader-install)
  )

;;; ..:: Load custom non-version-controlled init code ::..

(use-package init_local
  ;; Local non-version-controlled init file.
  :load-path danylo/emacs-custom-lisp-dir
  :if (locate-library "init_local.el"))
(put 'list-timers 'disabled nil)
