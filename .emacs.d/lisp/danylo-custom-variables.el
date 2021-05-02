;;; danylo-custom-variables.el --- Emacs init file customization variables
;;
;; Author: Danylo Malyuta
;;
;; Keywords: variables, defcustom, defface
;;
;; This file is not part of GNU Emacs
;;
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

(defcustom danylo/font-default-height 105
  "Default font height."
  :type 'float
  :group 'danylo)

(defcustom danylo/fill-column 79
  "Line width beyond which line wrapping should happen."
  :type 'integer
  :group 'danylo)

(defcustom danylo/side-window-width 30
  "Column width for side windows (Neotree, Imenu-list, etc.)."
  :type 'integer
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

(defcustom danylo/julia-help-buffer-name "*JuliaHelp*"
  "Name of the Julia help buffer."
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

(defconst danylo/fontify-delay~hz 30.0)
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

(defcustom danylo/linum-height 0.85
  "Height of line number font in the margin."
  :type 'float
  :group 'danylo)

(defcustom danylo/num-completion-candidates 15
  "How many completion candidates to display, tops."
  :type 'integer
  :group 'danylo)

(defcustom danylo/flycheck-error-list-size
  danylo/num-completion-candidates
  "How many Flycheck errors to display, tops."
  :type 'integer
  :group 'danylo)

(defcustom danylo/email-refresh-period `,(* 3 60)
  "How often (in seconds) to get new mail."
  :type 'integer
  :group 'danylo)

(defcustom danylo/get-mail-min-interval 5
  "How often (in seconds) the inbox refresh command can be
called (manually or automatically)."
  :type 'integer
  :group 'danylo)

(defcustom danylo/windsize-big-step 3
  "Step amount for the large-step mode of window resizing."
  :type 'integer
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

(defface danylo/imenu-section-face
  `((t (:foreground ,danylo/green :weight bold :inherit default)))
  "Face for sections in Imenu."
  :group 'danylo)

(defface danylo/imenu-function-face
  `((t (:foreground ,danylo/blue :weight bold :inherit default)))
  "Face for functions in Imenu."
  :group 'danylo)

(defface danylo/imenu-var-face
  `((t (:foreground ,danylo/orange :weight bold :inherit default)))
  "Face for functions in Imenu."
  :group 'danylo)

(defface danylo/imenu-const-face
  `((t (:foreground ,danylo/white :weight bold :inherit default)))
  "Face for constant variables in Imenu."
  :group 'danylo)

(defface danylo/imenu-macro-face
  `((t (:foreground ,danylo/red :weight bold :inherit default)))
  "Face for macros in Imenu."
  :group 'danylo)

(defface danylo/imenu-class-face
  `((t (:foreground ,danylo/green :weight bold :inherit default)))
  "Face for classes and structs in Imenu."
  :group 'danylo)

(defface danylo/imenu-import-face
  `((t (:foreground ,danylo/yellow :weight bold :inherit default)))
  "Face for import statements in Imenu."
  :group 'danylo)

(defface danylo/imenu-export-face
  `((t (:foreground ,danylo/orange :weight bold :inherit default)))
  "Face for export statements in Imenu."
  :group 'danylo)

;;;; Fringe objects

;; Flycheck fringe indicator, from Spacemacs
;; (https://github.com/syl20bnr/spacemacs)
(define-fringe-bitmap 'danylo/flycheck-fringe-indicator
  (vector #b00000000 #b00000000 #b00000000 #b00000000 #b00000000
          #b00000000 #b00000000 #b00011100 #b00111110 #b00111110
          #b00111110 #b00011100 #b00000000 #b00000000 #b00000000
          #b00000000 #b00000000))

(provide 'danylo-custom-variables)
;;; danylo-custom-variables.el ends here
