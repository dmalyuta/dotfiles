;;; danylo-custom-variables.el --- Emacs init file customization variables

;; Author: Danylo Malyuta

;; Keywords: variables, defcustom, defface


;; This file is not part of GNU Emacs

;;; Commentary:
;;
;;  Custom parameters (variables, faces) used by init file.
;;
;;; Code:

(defgroup danylo nil
  "My customization variables for the init.el file."
  :group 'local)

;;;; Variables

(defcustom danylo/gc-cons-threshold `,(* 1024 1024 100)
  "Limit before garbage collection can happen automatically."
  :type 'integer
  :group 'danylo)

(defcustom danylo/gc-collect-print t
  "Print message in minibuffer on garbage collection."
  :type 'boolean
  :group 'danylo)

(defcustom danylo/font-default-height 100
  "Default font height."
  :type 'float
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

(defcustom danylo/ivy-window-name "*ivy-candidate-list*"
  "Name of ivy candidate list buffer."
  :type 'string
  :group 'danylo)

(defcustom danylo/latex-preview-scale 1.3
  "Size of the latex preview in Org mode."
  :type 'float
  :group 'danylo)

(defconst danylo/fontify-delay~hz 3.0)
(defcustom danylo/fontify-delay `,(/ 1.0 danylo/fontify-delay~hz)
  "Delay before fontifying text. Helps to improve performance
since fontification is a slow-ish process."
  :type 'float
  :group 'danylo)

(defcustom danylo/delay-long 30.0
  "A standard 'long' delay for Emacs to do something every once
in a while."
  :type 'float
  :group 'danylo)

(defcustom danylo/linum-height 0.7
  "Height of line number font in the margin."
  :type 'float
  :group 'danylo)

;;;; Faces

;; Colors taken from doom-one theme
(defconst danylo/white      "white")
(defconst danylo/black      "black")
(defconst danylo/yellow     "#ECBE7B")
(defconst danylo/faded      "#464c5d")
(defconst danylo/red        "#ff6c6b")
(defconst danylo/orange     "#da8548")
(defconst danylo/blue       "#51afef")
(defconst danylo/dark-blue  "#375c79")
(defconst danylo/faded-blue "#31495d")
(defconst danylo/green      "#98be65")

(provide 'danylo-custom-variables)
;;; danylo-custom-variables.el ends here
