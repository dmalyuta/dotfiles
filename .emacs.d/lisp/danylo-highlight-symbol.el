;;; danylo-highlight-symbol.el --- Better symbol highlighting. -*- lexical-binding: t; -*-
;;
;; Author: Danylo Malyuta
;;
;; Keywords: symbol, highlight, overlay, regex
;;
;; This file is not part of GNU Emacs
;;
;;; Commentary:
;;
;;  Better symbol highlighting via overlays and skipping comments when
;;  tree-sitter is available. This code directly redefines the following
;;  functions provided in highlight-symbol.el:
;;
;;    highlight-symbol-jump
;;    highlight-symbol-count
;;    highlight-symbol-symbol-highlighted-p
;;    highlight-symbol-add-symbol-with-face
;;    highlight-symbol-remove-symbol
;;    highlight-symbol-remove-all
;;
;;  The following functions are wrapped with advice:
;;
;;    highlight-symbol-temp-highlight
;;
;;; Code:

(require 'highlight-symbol)
(require 'multiple-cursors)

(defvar danylo/update-highlight-symbol nil)
(make-variable-buffer-local 'danylo/update-highlight-symbol)

(defvar danylo/string-to-remove nil)
(make-variable-buffer-local 'danylo/string-to-remove)

(defun danylo/delete-overlay (overlay after beg end &optional len)
  "Delete OVERLAY from buffer."
  (let ((inhibit-quit t))
    (save-excursion
      (let ((this-string (buffer-substring
                          (overlay-start overlay)
                          (overlay-end overlay))))
        (if after
            (progn
              (when danylo/string-to-remove
                (highlight-symbol-remove-symbol
                 `,(concat "\\_<" danylo/string-to-remove "\\_>"))
                (setq danylo/string-to-remove nil))
              (when danylo/update-highlight-symbol
                (setq danylo/update-highlight-symbol nil)
                (setq highlight-symbol this-string)))
          (when (overlay-buffer overlay)
            (when (equal highlight-symbol (highlight-symbol-get-symbol))
              (setq danylo/update-highlight-symbol t))
            (setq danylo/string-to-remove this-string)))))))

(defun danylo/has-treesitter ()
  "Check if major mode has treesitter parser available."
  (and (fboundp 'treesit-available-p) (treesit-available-p)))

(defun danylo/check-if-is-source (pos)
  "Check if POS position is source code and not something like a comment or
a string, using `treesit-node-at'."
  (interactive)
  (let ((inhibit-quit t))
    (if (danylo/has-treesitter)
        (let ((node (treesit-node-at pos)))
          (if (and node (member (treesit-node-type node)
                                '("comment" "string_content")))
              nil
            t))
      t)))

(defun danylo/count-occurrences-not-in-comment (regexp beg end)
  "Count number of occurrences of REGEXP between BEG and END that are not
in a comment."
  (let ((inhibit-quit t))
    (save-excursion
      (goto-char beg)
      (let ((case-fold-search nil)
            (count 0))
        (while (re-search-forward `,regexp end t)
          (when (danylo/check-if-is-source (match-beginning 0))
            (setq count (1+ count))))
        count))))

(defun danylo/search-forward-not-in-comment (regexp noerror dir)
  "Search for first occurrence of REGEXP that is not in a comment in the
direction DIR."
  (let ((search-count (if (< 0 dir) most-positive-fixnum most-negative-fixnum))
        (inhibit-quit t))
    (catch 'next-occurrence
      (save-excursion
        (while (re-search-forward `,regexp nil noerror dir)
          (when (danylo/check-if-is-source (match-beginning 0))
            (throw 'next-occurrence
                   (if (< 0 dir) (match-end 0) (match-beginning 0))))))))
  )

(defun highlight-symbol-jump (dir)
  "Jump to the next or previous occurence of the symbol at point.
DIR has to be 1 or -1."
  (let ((symbol (highlight-symbol-get-symbol))
        (inhibit-quit t))
    (if symbol
        (let* ((case-fold-search nil)
               (msg (member 'navigation highlight-symbol-occurrence-message))
               (bounds (bounds-of-thing-at-point 'symbol))
               (offset (- (point) (if (< 0 dir) (cdr bounds) (car bounds)))))
          (unless (eq last-command 'highlight-symbol-jump)
            (push-mark))
          ;; move a little, so we don't find the same instance again
          (goto-char (- (point) offset))
          (let ((target (danylo/search-forward-not-in-comment symbol t dir)))
            (unless target
              (goto-char (if (< 0 dir) (point-min) (point-max)))
              (unless msg
                (message "Continued from beginning of buffer"))
              (setq target (danylo/search-forward-not-in-comment symbol nil dir)))
            (goto-char (+ target offset)))
          (when msg
            (highlight-symbol-count symbol t))
          (setq this-command 'highlight-symbol-jump))
      (error "No symbol at point"))))

(defun highlight-symbol-count (&optional symbol message-p)
  "Print the number of occurrences of SYMBOL at point."
  (interactive '(nil t))
  (let* ((inhibit-quit t)
         (symbol (or symbol
                     (highlight-symbol-get-symbol)
                     (error "No symbol at point")))
         (case-fold-search nil)
         (count (danylo/count-occurrences-not-in-comment
                 symbol (point-min) (point-max))))
    (when message-p
      (if (= count 0)
          (message "Only occurrence in buffer")
        (message "Occurrence %d/%d in buffer"
                 (1+ (danylo/count-occurrences-not-in-comment
                      symbol (point-min) (1- (point))))
                 count)))
    count))

(defun highlight-symbol-symbol-highlighted-p (symbol)
  "Test is SYMBOL is current highlighted."
  (let ((inhibit-quit t))
    (catch 'found-overlay
      (dolist (ov (overlays-at (point)))
        (when (overlay-get ov 'highlight-symbol-overlay)
          (throw 'found-overlay t)))
      (throw 'found-overlay nil))))

(defun highlight-symbol-add-symbol-with-face (symbol face)
  "Highlight SYMBOL with face FACE."
  (let ((inhibit-quit t))
    (save-excursion
      (let ((case-fold-search nil))
        (goto-char (point-min))
        (while (re-search-forward `,symbol nil t)
          (let ((match-pos-start (match-beginning 0))
                (match-pos-end (match-end 0)))
            (when (danylo/check-if-is-source match-pos-start)
              (let ((ov (make-overlay match-pos-start match-pos-end)))
                (overlay-put ov 'face face)
                (overlay-put ov 'priority -10) ;; Higher than hl-line-overlay-priority
                (overlay-put ov 'highlight-symbol-overlay t)
                (overlay-put ov 'modification-hooks (list #'danylo/delete-overlay))
                ))))))))

(defun highlight-symbol-remove-symbol (symbol)
  "Un-highlight SYMBOL."
  (let ((inhibit-quit t))
    (unless (string= "" symbol)
      (save-excursion
        (let ((case-fold-search nil))
          (goto-char (point-min))
          (while (re-search-forward `,symbol nil t)
            (remove-overlays
             (match-beginning 0) (match-end 0)
             'highlight-symbol-overlay t)))))))

(defun highlight-symbol-remove-all ()
  "Remove symbol highlighting in buffer."
  (interactive)
  (let ((inhibit-quit t))
    (save-excursion
      (remove-overlays nil nil 'highlight-symbol-overlay t))))

(defun danylo/highlight-symbol-temp-highlight (orig-fun &rest args)
  "Temporarily highlight the symbol under certain conditions."
  (when (and (danylo/check-if-is-source (point))
             (not (use-region-p))
             (= (mc/num-cursors) 1))
    (apply orig-fun args)))

(advice-add 'highlight-symbol-temp-highlight
            :around #'danylo/highlight-symbol-temp-highlight)

(provide 'danylo-highlight-symbol)
;;; danylo-highlight-symbol.el ends here
