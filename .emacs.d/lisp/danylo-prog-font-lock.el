;;; danylo-prog-font-lock.el --- Font locking for programming code files
;;
;; Author: Danylo Malyuta
;;
;; Keywords: font lock
;;
;; This file is not part of GNU Emacs
;;
;;; Commentary:
;;
;;  Font locking for programming code files (Python and so on).
;;
;;; Code:

(require 'danylo-custom-variables)

(defcustom danylo/class-height 1.2
  "Class name font size."
  :type 'float
  :group 'danylo)

(defcustom danylo/module-height 1.3
  "Module name font size."
  :type 'float
  :group 'danylo)

(defface danylo/python-docstring-heading-face
  `((t (:foreground ,danylo/yellow
		    :weight normal
		    :inherit default)))
  "Face for Python docstring headings for arguments, returns, etc."
  :group 'danylo)

(defface danylo/class-keyword-face
  `((t (:foreground ,danylo/white
		    :background ,danylo/blue
		    :height ,danylo/class-height
		    :weight bold
		    :inherit default)))
  "Face for class keyword."
  :group 'danylo)

(defface danylo/class-other-face
  `((t (:height ,danylo/class-height
		:inherit font-lock-keyword-face)))
  "Face for other keywords in the class line."
  :group 'danylo)

(defface danylo/class-name-face
  `((t (:foreground ,danylo/yellow
		:height ,danylo/class-height
	        :inherit default)))
  "Face for class name."
  :group 'danylo)

(defface danylo/module-keyword-face
  `((t (:foreground ,danylo/white
		    :background ,danylo/red
		    :height ,danylo/module-height
		    :weight bold
		    :inherit default)))
  "Face for module keyword."
  :group 'danylo)

(defface danylo/module-name-face
  `((t (:foreground ,danylo/red
		:height ,danylo/module-height
	        :inherit default)))
  "Face for class name."
  :group 'danylo)

(defface danylo/class-name-colon
  `((t (:height ,danylo/class-height
	        :inherit default)))
  "Face for class name."
  :group 'danylo)

(defface danylo/function-face
  `((t (:foreground ,danylo/white
		    :background ,danylo/dark-blue
		    :weight bold
		    :inherit default)))
  "Face for function keyword."
  :group 'danylo)

(defface danylo/macro-face
  `((t (:foreground ,danylo/yellow
		    :background ,danylo/dark-blue
		    :slant italic
		    :weight bold
		    :inherit default)))
  "Face for function keyword."
  :group 'danylo)

(defface danylo/return-face
  `((t (:foreground ,danylo/red
		    :weight bold
		    :inherit default)))
  "Face for return-type keyword."
  :group 'danylo)

(defface danylo/continue-face
  `((t (:foreground ,danylo/blue
		    :weight bold
		    :inherit default)))
  "Face for continue-type keyword."
  :group 'danylo)

(defface danylo/self-face
  `((t (:foreground ,danylo/green
		    :weight normal
		    :inherit default)))
  "Face for class self."
  :group 'danylo)

(defface danylo/property-face
  `((t (:foreground ,danylo/faded-blue
		    :inherit default)))
  "Face for class self."
  :group 'danylo)

(defface danylo/import-face
  `((t (:foreground ,danylo/green
		    :weight bold
		    :inherit default)))
  "Face for class self."
  :group 'danylo)

(defface danylo/bool-face
  `((t (:foreground ,danylo/orange
		    :weight bold
		    :inherit default)))
  "Face for class self."
  :group 'danylo)

(defface danylo/control-face
  `((t (:foreground ,danylo/blue
		    :inherit default)))
  "Face for class self."
  :group 'danylo)

;;;###autoload
(defun danylo/make-prog-highlight-keywords ()
  "Make the font-lock keywords for prog-font-lock-mode fontification."
  ;; ..:: Python ::..
  (cond ((eq major-mode 'python-mode)
	 (setq danylo/prog-highlight-keywords
	       '(("^\s*\\(Args\\|Returns\\|Raises\\):$"
		  (0 '(face danylo/python-docstring-heading-face) t))
		 ("^\\(class\\)\s+[^:]*\\(:\\)$"
		  (0 '(face danylo/class-name-face) t)
		  (1 '(face danylo/class-keyword-face) t)
		  (2 '(face danylo/class-name-colon) t))
		 ("self" (0 '(face danylo/self-face) t))
		 ("^\s*\\(return\\|break\\|raise\\)" (1 '(face danylo/return-face) t))
		 ("^\s*\\(continue\\|pass\\)" (1 '(face danylo/continue-face) t))
		 ("^\s*\\(def\\)\s+" (1 '(face danylo/function-face) t))
		 ("^\s*\\(@property\\)" (1 '(face danylo/property-face) t))
		 ("^\s*\\(import\\|from\\)" (1 '(face danylo/import-face) t))
		 ("^\s*\\(?:from\\).*\\(import\\)" (1 '(face danylo/import-face) t))
		 ("\\(?:import\\).*\\(as\\)" (1 '(face danylo/import-face) t))
		 ("\\(?:True\\|False\\)" (0 '(face danylo/bool-face) t))
		 ("^\s*\\(?:if\\|for\\|while\\|elif\\|else\\)\s+"
		  (0 '(face danylo/control-face) t))
		 )))
	((eq major-mode 'julia-mode)
	 (setq danylo/prog-highlight-keywords
	       '(("^\\(using\\|include\\)" (1 '(face danylo/import-face) t))
		 ("^\s*#*\s*\\(Args\\|Returns\\|Raises\\):$"
		  (1 '(face danylo/python-docstring-heading-face) t))
		 ("^[^#\n]*\s+\\(struct\\)\s+\\(.*?\\)$"
		  (0 '(face danylo/class-other-face) t)
		  (1 '(face danylo/class-keyword-face) t)
		  (2 '(face danylo/class-name-face) t))
		 ("^\\(struct\\)\s+\\(.*?\\)$"
		  (0 '(face danylo/class-other-face) t)
		  (1 '(face danylo/class-keyword-face) t)
		  (2 '(face danylo/class-name-face) t))
		 ("^\\(module\\)\s+\\(.*?\\)$"
		  (1 '(face danylo/module-keyword-face) t)
		  (2 '(face danylo/module-name-face) t))
		 ("^\s*\\b\\(return\\|break\\|throw\\)\\b"
		  (1 '(face danylo/return-face) t))
		 ("^\s*\\(continue\\|pass\\)" (1 '(face danylo/continue-face) t))
		 ("^\s*\\(function\\)\s+" (1 '(face danylo/function-face) t))
		 ("^\s*\\(macro\\)\s+" (1 '(face danylo/macro-face) t))
		 ("\\(?:true\\|false\\)" (0 '(face danylo/bool-face) t))
		 ("^\s*\\(?:if\\|for\\|while\\|elseif\\|else\\)\s+"
		  (0 '(face danylo/control-face) t))
		 )))
	)
  danylo/prog-highlight-keywords)

;;;###autoload
(defun danylo/prog-font-lock-mode-on ()
  "Activate minor mode."
  (make-variable-buffer-local 'font-lock-extra-managed-props)
  (add-to-list 'font-lock-extra-managed-props 'invisible)
  (add-to-list 'font-lock-extra-managed-props 'display)
  ;; Add keywords
  (font-lock-add-keywords nil (danylo/make-prog-highlight-keywords))
  (font-lock-flush)
  (message "Custom code highlighting on"))

;;;###autoload
(defun danylo/prog-font-lock-mode-off ()
  "Shutdown minor mode."
  ;; Remove keywords
  (font-lock-remove-keywords nil (danylo/make-prog-highlight-keywords))
  (font-lock-flush)
  (message "Custom code highlighting off"))

;;;###autoload
(define-minor-mode danylo-prog-font-lock-mode
  "Program code font locking."
  :lighter " danylo-prog-highlight"
  (if danylo-prog-font-lock-mode
      (danylo/prog-font-lock-mode-on)
    (danylo/prog-font-lock-mode-off)))

(provide 'danylo-prog-font-lock)
;;; danylo-prog-font-lock.el ends here
