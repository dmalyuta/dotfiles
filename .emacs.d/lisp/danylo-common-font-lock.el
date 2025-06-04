;;; danylo-common-font-lock.el --- Font locking common code. -*- lexical-binding: t; -*-
;;
;; Author: Danylo Malyuta
;;
;; Keywords: font lock
;;
;; This file is not part of GNU Emacs
;;
;;; Commentary:
;;
;;  Font locking common functions.
;;
;;; Code:

(defun danylo/font-lock-extend-region ()
  "Extend the search region to include an entire block of text."
  (let ((changed nil))
    (save-excursion
      (goto-char font-lock-beg)
      (let ((found (or (re-search-backward "^$" nil t) (point-min))))
	(unless (eq font-lock-beg found)
	  (goto-char found)
	  (setq font-lock-beg (if (bolp) found (line-beginning-position))
		changed t)))
      (goto-char font-lock-end)
      (let ((found (or (re-search-forward "^$" nil t) (point-max))))
	(unless (eq font-lock-end found)
	  (goto-char found)
	  (setq font-lock-end (if (bolp) found (line-beginning-position 2))
		changed t))))
    changed))

(provide 'danylo-common-font-lock)
;;; danylo-common-font-lock.el ends here
