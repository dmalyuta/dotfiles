;;; danylo-csv.el --- Danylo's mode for editing CSV files. -*- lexical-binding: t; -*-
;;
;; Author: Danylo Malyuta
;;
;; Keywords: csv, tables, data, minor-mode
;;
;; This file is not part of GNU Emacs
;;
;;; Commentary:
;;
;;  Defines a danylo-csv-mode.
;;
;;; Code:

(require 'color)

(defvar csv-column-colors-list nil
  "List to hold the colors of columns.")
(make-variable-buffer-local 'csv-column-colors-list)

(defun danylo/csv-highlight (&optional separator)
  "Gives alternating colors to columns in a CSV file, separated by
SEPARATOR (, by default)."
  (interactive (list (when current-prefix-arg (read-char "Separator: "))))
  (font-lock-mode 1)
  (let* ((separator (or separator ?\,))
         (n (count-matches (string separator) (pos-bol) (pos-eol))))
    (setq csv-column-colors-list
          (cl-loop for i from 0 to 1.0 by (/ 2.0 n)
                   collect (apply #'color-rgb-to-hex
                                  (color-hsl-to-rgb i 0.3 0.5))))
    (cl-loop for i from 2 to n by 2
             for c in csv-column-colors-list
             for r = (format "^\\([^%c\n]+%c\\)\\{%d\\}" separator separator i)
             do (font-lock-add-keywords nil `((,r (1 '(face (:foreground ,c)))))))))

(defvar csv-field-index-header-fields-list nil
  "List to hold the text of fields in csv file.")
(make-variable-buffer-local 'csv-field-index-header-fields-list)

(defcustom csv-field-display-names t
  "When true, display both name and field number instead of
just field number under `csv-field-index-mode'.")

(defun csv-populate-header-fields ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let* ((text (buffer-substring (point) (line-end-position)))
           (fields (split-string text ",")))
      (setq csv-field-index-header-fields-list fields))))
(add-hook 'csv-mode-hook #'csv-populate-header-fields)

(defvar danylo/csv-field-index-delay 0.2)

(defvar danylo/csv-field-index-timer nil)
(make-variable-buffer-local 'danylo/csv-field-index-timer)

;; (defcustom danylo/csv-field-name-color "#97bd64"
;;   "Color used for echoing the CSV entry's field name.")

(defun danylo/csv-field-index ()
  "Display column names in the echo area."
  ;; Set up colorization of columns.
  (when (eq major-mode 'danylo-csv-mode)
    (danylo/csv-highlight)
    (let ((this-buffer (current-buffer)))
      (when danylo/csv-field-index-timer
        (cancel-timer danylo/csv-field-index-timer))
      (setq
       danylo/csv-field-index-timer
       (run-with-idle-timer
        danylo/csv-field-index-delay t
        (lambda (this-buffer)
          (when (and (eq (current-buffer) this-buffer)
                     csv-field-display-names
                     (eq major-mode 'danylo-csv-mode))
            (csv-populate-header-fields)
            (save-excursion
              (let ((lbp (line-beginning-position)) (field 1))
                (while (re-search-backward "," lbp 1)
                  ;; Move as far as possible, i.e. to beginning of line.
                  (setq field (1+ field)))
                (when (looking-at paragraph-separate) (setq field nil))
                (when field
                  (let ((field-name (nth (1- field)
                                         csv-field-index-header-fields-list))
                        (fg-color
                         (if (= (mod field 2) 1)
                             (face-attribute 'default :foreground)
                           (nth (round (/ (1- field) 2)) csv-column-colors-list)))
                        ;; Print message without logging.
                        (message-log-max nil)
                        )
                    (when field-name
                      (message
                       "%s"
                       (concat
                        (propertize
                         (format "%s " (danylo/fa-icon "table"))
                         'face `(:family ,(all-the-icons-faicon-family)
                                         :foreground ,fg-color))
                        (propertize
                         (format "%s (%d)" field-name field)
                         'face `(:foreground ,fg-color)))))
                    ))))))
        this-buffer)))))

(define-derived-mode danylo-csv-mode fundamental-mode "Danylo/CSV"
  ;; Do not truncate lines
  (setq truncate-lines nil)
  ;; Print column names.
  (danylo/csv-field-index)
  ;; Turn off line wrap indicators in the fringe.
  (let ((custom-fringe-indicator-alist '()))
    (dolist (indicator fringe-indicator-alist)
      (if (eq (car indicator) 'continuation)
          (add-to-list 'custom-fringe-indicator-alist '(continuation nil nil))
        (add-to-list 'custom-fringe-indicator-alist indicator)))
    (setq fringe-indicator-alist custom-fringe-indicator-alist))
  )

;; Associate file extensions with the mode.
(add-to-list 'auto-mode-alist '("\\.csv" . danylo-csv-mode))

(provide 'danylo-csv)
;;; danylo-csv.el ends here
