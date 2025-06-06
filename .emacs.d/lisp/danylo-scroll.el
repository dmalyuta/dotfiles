;;; danylo-scroll.el --- Better scrolling. -*- lexical-binding: t; -*-
;;
;; Author: Danylo Malyuta
;;
;; Keywords: scroll
;;
;; This file is not part of GNU Emacs
;;
;;; Commentary:
;;
;;  Better scrolling up and down than what default Emacs provides. Inspired by
;;  pager.el (https://github.com/emacsorphanage/pager/blob/master/pager.el).
;;
;;; Code:

(defcustom danylo/scroll-frac 0.15
  "Fraction of window's height to fast scroll up/down by."
  :type 'float
  :group 'danylo-scroll)

(defconst danylo/keep-column-commands
  '(danylo/scroll-down
    danylo/scroll-up
    danylo/scroll-row-down
    danylo/scroll-row-up)
  "Commands which when called without any other intervening command should
keep the `danylo/scroll-temporary-goal-column'.")

(defvar danylo/scroll-temporary-goal-column 0
  "Similar to temporary-goal-column but used by my custom scroll functions.")

(defun danylo/window-height-fraction (frac)
  "Get FRAC of the full window height, default is 0.5."
  (ceiling (* (window-height (selected-window)) frac)))

(defun danylo/scroll-screen (lines)
  "Scroll screen by LINES visual lines, but keep the cursors position on
screen."
  (unless (memq last-command danylo/keep-column-commands)
    (setq danylo/scroll-temporary-goal-column
          (- (point) (save-excursion (beginning-of-visual-line) (point)))))
  (save-excursion
    (goto-char (window-start))
    (vertical-motion lines)
    (set-window-start (selected-window) (point)))
  (vertical-motion lines)
  ;; Move to the same column as before.
  (move-to-column
   (+ (save-excursion (beginning-of-visual-line) (current-column))
      danylo/scroll-temporary-goal-column)))

(defun danylo/get-scroll-step (arg)
  "Get the scroll step."
  (if (eq arg 0)
      1
    (progn
      (when (and (not (null arg)) (>= arg 0) (<= arg 100))
        (setq danylo/scroll-frac (/ (float arg) 100.0))
        (message "New scroll fraction: %.2f" danylo/scroll-frac))
      (danylo/window-height-fraction danylo/scroll-frac))))

(defun danylo/scroll-down (&optional arg)
  "Scroll down."
  (interactive "P")
  (let ((inhibit-quit t))
    (if (not (pos-visible-in-window-p (point-max)))
        (danylo/scroll-screen (danylo/get-scroll-step arg)))))

(defun danylo/scroll-up (&optional arg)
  "Scroll up."
  (interactive "P")
  (let ((inhibit-quit t))
    (if (not (pos-visible-in-window-p (point-min)))
        (danylo/scroll-screen (- (danylo/get-scroll-step arg))))))

(defun danylo/scroll-row-up ()
  "Move point to previous line, keeping cursor in the same position on the
screen."
  (interactive)
  (danylo/scroll-up 0))

(defun danylo/scroll-row-down ()
  "Move point to next line, keeping cursor in the same position on the
screen."
  (interactive)
  (danylo/scroll-down 0))

(provide 'danylo-scroll)
;;; danylo-scroll.el ends here
