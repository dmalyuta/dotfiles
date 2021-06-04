;;; danylo-text-font-lock.el --- Font locking for Org and LaTeX files
;;
;; Author: Danylo Malyuta
;;
;; Keywords: font lock
;;
;; This file is not part of GNU Emacs
;;
;;; Commentary:
;;
;;  Font locking for text files like Org and LaTeX files.
;;
;;; Code:

(require 'danylo-custom-variables)
(require 'danylo-common-font-lock)

(defcustom danylo/ref-prefix-height 0.8
  "Height of the font to use for LaTeX \...ref{...} fontification."
  :type 'float
  :group 'danylo)

(defcustom danylo/section-height 1.4
  "Section heading font size."
  :type 'float
  :group 'danylo)

(defcustom danylo/subsection-height 1.2
  "Subsection heading font size."
  :type 'float
  :group 'danylo)

(defcustom danylo/subsubsection-height 1.1
  "Subsubsection heading font size."
  :type 'float
  :group 'danylo)

(defface danylo/latex-face-equation-main
  `((t (:foreground ,danylo/yellow
                    :weight normal
                    :inherit default)))
  "Face for LaTeX math content."
  :group 'danylo)

(defface danylo/latex-face-equation-delim
  `((t (:foreground ,danylo/faded
                    :weight bold
                    :inherit default)))
  "Face for LaTeX math delimiters."
  :group 'danylo)

(defface danylo/latex-face-item
  `((t (:foreground ,danylo/red
                    :inherit default)))
  "Face for LaTeX list item."
  :group 'danylo)

(defface danylo/latex-face-ref
  `((t (:foreground ,danylo/blue
                    :inherit default)))
  "Face for LaTeX references."
  :group 'danylo)

(defface danylo/latex-face-ref-prefix
  `((t (:foreground ,danylo/faded-blue
                    :height ,danylo/ref-prefix-height
                    :inherit default)))
  "Face for LaTeX reference's kind."
  :group 'danylo)

(defface danylo/face-section
  `((t (:foreground ,danylo/white
                    :background ,danylo/red
                    :height ,danylo/section-height
                    :inherit default)))
  "Face for text section."
  :group 'danylo)

(defface danylo/face-subsection
  `((t (:foreground ,danylo/white
                    :background ,danylo/green
                    :height ,danylo/subsection-height
                    :inherit default)))
  "Face for text subsection."
  :group 'danylo)

(defface danylo/face-subsubsection
  `((t (:foreground ,danylo/white
                    :background ,danylo/blue
                    :height ,danylo/subsubsection-height
                    :inherit default)))
  "Face for text subsubsection."
  :group 'danylo)

(defface danylo/face-deepsection
  `((t (:foreground ,danylo/white
                    :background ,danylo/faded-blue
                    :inherit default)))
  "Face for text sections at lower levels than subsubsection."
  :group 'danylo)

(defface danylo/latex-boolean
  `((t (:foreground ,danylo/white
                    :background ,danylo/red
                    :inherit default)))
  "Face for boolean variables like \iffalse and \fi."
  :group 'danylo)

(defun danylo/make-text-highlight-keywords ()
  "Make the font-lock keywords for text-font-lock-mode fontification."
  (setq danylo/ref-prefix-raise (- 1 0.8))
  (setq danylo/text-highlight-keywords nil)
  ;; ..:: References ::..
  (when (eq major-mode 'latex-mode)
    ;; >> Citations <<
    (add-to-list
     'danylo/text-highlight-keywords
     `("\\(\\\\\\)\\(cite.?\\)\\({\\)\\(?:.\\|\n\\)*?\\(}\\)"
       (0 '(face danylo/latex-face-ref invisible nil) nil t)
       (1 '(face danylo/latex-face-ref invisible t) nil t)
       (2 '(face danylo/latex-face-ref-prefix display
                 '(raise ,danylo/ref-prefix-raise)) t)
       (3 '(face danylo/latex-face-ref display "[") nil t)
       (4 '(face danylo/latex-face-ref display "]") nil t)))
    (add-to-list
     'danylo/text-highlight-keywords
     `("\\(\\\\\\)\\(cite.?\\)\\(\\[\\)\\(?:.\\|\n\\)*?\\(\\]\\)\\({\\)\\(?:.\\|\n\\)*?\\(}\\)"
       (0 '(face danylo/latex-face-ref invisible nil) nil t)
       (1 '(face danylo/latex-face-ref invisible t) nil t)
       (2 '(face danylo/latex-face-ref-prefix display
                 '(raise ,danylo/ref-prefix-raise)) t)
       (3 '(face danylo/latex-face-ref display "(") nil t)
       (4 '(face danylo/latex-face-ref display ")") nil t)
       (5 '(face danylo/latex-face-ref display "[") nil t)
       (6 '(face danylo/latex-face-ref display "]") nil t)))
    ;; >> Labels <<
    (add-to-list
     'danylo/text-highlight-keywords
     `("\\(\\\\[a-zA-Z^\n]*label{\\)\\(?:[^}\n]*\\)\\(}\\)"
       (0 '(face danylo/latex-face-ref invisible nil) t t)
       (1 '(face danylo/latex-face-ref display "<") t t)
       (2 '(face danylo/latex-face-ref display ">") t t)))
    ;; >> \[...]ref{[...]} <<
    (add-to-list
     'danylo/text-highlight-keywords
     `("\\(\\\\\\)\\([^{}\\\t\r\n\s]*?\\)\\(ref\\)\\({\\)\\(?:.\\|\n\\)*?\\(}\\)"
       (0 '(face danylo/latex-face-ref invisible nil) nil t)
       (1 '(face danylo/latex-face-ref-prefix invisible t) t)
       (2 '(face danylo/latex-face-ref-prefix display
                 '(raise ,danylo/ref-prefix-raise)) t)
       (3 '(face danylo/latex-face-ref-prefix invisible t) t)
       (4 '(face danylo/latex-face-ref display "(") nil t)
       (5 '(face danylo/latex-face-ref display ")") nil t))))
  ;; ..:: Math ::..
  ;; >> Delimiters <<
  (mapcar
   (lambda (arg)
     (add-to-list
      'danylo/text-highlight-keywords
      `(,(format "\\(?:%s\\)" arg)
        (0 '(face danylo/latex-face-equation-delim invisible nil) nil t))))
   `("\\$\\(?:\\$\\)?"
     ,(concat "\\\\\\(?:begin\\|end\\){\\(?:equation\\|align\\|alignat"
              "\\|gather\\|multline\\|subequations\\|optimization\\)[\\*]?}")))
  ;; >> Body <<
  (mapcar
   (lambda (arg)
     (add-to-list
      'danylo/text-highlight-keywords
      `(,(format "\\(?:%s\\)\\(\\(?:.\\|\n\\)*?\\)\\(?:%s\\)"
                 (car arg) (cdr arg))
        (1 '(face danylo/latex-face-equation-main invisible nil) nil t))))
   '(("\\$" . "\\$")
     ("\\$\\$" . "\\$\\$")
     ("\\\\begin{equation[\\*]?}" . "\\\\end{equation[\\*]?}")
     ("\\\\begin{align[\\*]?}" . "\\\\end{align[\\*]?}")
     ("\\\\begin{alignat[\\*]?}" . "\\\\end{alignat[\\*]?}")
     ("\\\\begin{gather[\\*]?}" . "\\\\end{gather[\\*]?}")
     ("\\\\begin{multline[\\*]?}" . "\\\\end{multline[\\*]?}")
     ("\\\\begin{subequations[\\*]?}" . "\\\\end{subequations[\\*]?}")
     ("\\\\begin{optimization[\\*]?}" . "\\\\end{optimization[\\*]?}")))
  ;; ..:: Lists ::..
  (when (eq major-mode 'latex-mode)
    (add-to-list
     'danylo/text-highlight-keywords
     `("\\\\\\(?:begin\\|end\\){\\(?:itemize\\|enumerate\\)}"
       (0 '(face danylo/latex-face-item invisible nil) nil t)))
    (add-to-list
     'danylo/text-highlight-keywords
     '("\\(\\\\item\\) " 1 '(face danylo/latex-face-item display "●"))))
  ;; ..:: Sections ::..
  (cond ((eq major-mode 'latex-mode)
         (add-to-list
          'danylo/text-highlight-keywords
          '("^\s*\\(\\\\section[\\*]?{\\)\\(?:.\\|\n\\)*?\\(}\\)$"
            (0 '(face danylo/face-section) t)
            (1 '(face danylo/face-section display "§ ") t)
            (2 '(face danylo/face-section invisible t) t)))
         (add-to-list
          'danylo/text-highlight-keywords
          '("^\s*\\(\\\\subsection[\\*]?{\\)\\(?:.\\|\n\\)*?\\(}\\)$"
            (0 '(face danylo/face-subsection) t)
            (1 '(face danylo/face-subsection display "§§ ") t)
            (2 '(face danylo/face-subsection invisible t) t)))
         (add-to-list
          'danylo/text-highlight-keywords
          '("^\s*\\(\\\\subsubsection[\\*]?{\\)\\(?:.\\|\n\\)*?\\(}\\)$"
            (0 '(face danylo/face-subsubsection) t)
            (1 '(face danylo/face-subsubsection display "§§§ ") t)
            (2 '(face danylo/face-subsubsection invisible t) t))))
        ((eq major-mode 'org-mode)
         (add-to-list
          'danylo/text-highlight-keywords
          '("^\\(\\*\\{1\\}\s\\)\\(?:.*\\)$"
            (0 '(face danylo/face-section) t)))
         (add-to-list
          'danylo/text-highlight-keywords
          '("^\\(\\*\\{2\\}\s\\)\\(?:.*\\)$"
            (0 '(face danylo/face-subsection) t)))
         (add-to-list
          'danylo/text-highlight-keywords
          '("^\\(\\*\\{3\\}\s\\)\\(?:.*\\)$"
            (0 '(face danylo/face-subsubsection) t)))
         (mapcar
          (lambda (arg)
            (add-to-list
             'danylo/text-highlight-keywords
             `(,(format "^\\(\\*\\{%d\\}\s\\)\\(?:.*\\)$" arg)
               (0 '(face danylo/face-deepsection) t))))
          '(4 5 6 7 8))))
  ;; ..:: Miscellaneous ::..
  (when (eq major-mode 'latex-mode)
    (add-to-list
     'danylo/text-highlight-keywords
     `("\\(\\\\if[a-zA-Z]*\\)\\(?:[\s\r\n]?\\)"
       (1 '(face danylo/latex-boolean) nil t)))
    (add-to-list
     'danylo/text-highlight-keywords
     `("\\(\\\\fi\\|\\\\else\\)\\(?:[\s\r\n]\\{1\\}\\)"
       (1 '(face danylo/latex-boolean) nil t))))
  danylo/text-highlight-keywords)

;;;###autoload
(defun danylo/text-font-lock-mode-on ()
  "Activate minor mode."
  (make-variable-buffer-local 'font-lock-extra-managed-props)
  (add-to-list 'font-lock-extra-managed-props 'invisible)
  (add-to-list 'font-lock-extra-managed-props 'display)
  ;; ..:: Enable multiline highlight ::..
  (set (make-local-variable 'font-lock-multiline) t)
  (add-hook 'font-lock-extend-region-functions 'danylo/font-lock-extend-region)
  ;; Add keywords
  (font-lock-add-keywords nil (danylo/make-text-highlight-keywords))
  (font-lock-flush)
  (message "Custom text highlighting on"))

;;;###autoload
(defun danylo/text-font-lock-mode-off ()
  "Shutdown minor mode."
  ;; Remove keywords
  (font-lock-remove-keywords nil (danylo/make-text-highlight-keywords))
  (font-lock-flush)
  (message "Custom text highlighting off"))

;;;###autoload
(define-minor-mode danylo-text-font-lock-mode
  "Text font locking."
  :lighter " danylo-text-highlight"
  (if danylo-text-font-lock-mode
      (danylo/text-font-lock-mode-on)
    (danylo/text-font-lock-mode-off)))

(provide 'danylo-text-font-lock)
;;; danylo-text-font-lock.el ends here
