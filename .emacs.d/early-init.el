;;; --- Code styles. -*- lexical-binding: t; -*-

;; Put use-package in control of when packages are loaded.
;; https://www.gnu.org/software/emacs/manual/html_node/use-package/Deferring-loading.html
(setq package-enable-at-startup nil)

;; Source directory of Emacs core C runtime code.
(setq source-directory "~/Documents/software/emacs/")

;; Lisp deprecation
;; https://github.com/kiwanami/emacs-epc/issues/35#issuecomment-773420321
(setq byte-compile-warnings '(cl-functions))

;; LSP language server performance
;; https://emacs-lsp.github.io/lsp-mode/page/performance/#use-plists-for-deserialization
(setenv "LSP_USE_PLISTS" "true")
