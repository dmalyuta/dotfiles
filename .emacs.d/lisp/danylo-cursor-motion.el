;;; danylo-cursor-motion.el --- Moving the point. -*- lexical-binding: t; -*-
;;
;; Author: Danylo Malyuta
;;
;; Keywords: point, cursor
;;
;; This file is not part of GNU Emacs
;;
;;; Commentary:
;;
;;  Moving the point around the buffer.
;;
;;; Code:

(defcustom danylo/cursor-big-step 5
  "Step amount for accelerated cursor motion."
  :type 'integer
  :group 'danylo-cursor-motion)

(defvar danylo/cursor-current-step 1
  "The step amount by which to move cursor.")

(defvar danylo/cursor-timer nil
  "Timer object for cursor motion amount resetting.")

(defconst danylo/cursor-keep-column-commands
  '(danylo/cursor-up-smart
    danylo/cursor-down-smart)
  "Commands which when called without any other intervening command should
keep the `danylo/cursor-goal-visual-column' value.")

(defvar danylo/cursor-goal-visual-column 0
  "Goal column for the cursor.")

(defun danylo/goto-visual-line-start ()
  "Go to the start of the visual line and return the position."
  (if truncate-lines
      (move-beginning-of-line nil)
    (beginning-of-visual-line)))

(defun danylo/cursor-smart-move (dir &optional arg)
  (interactive "P")
  (when arg
    ;; Cancel existing timer
    (when danylo/cursor-timer
      (cancel-timer danylo/cursor-timer))
    ;; Change step increment and create new timer
    (setq danylo/cursor-current-step danylo/cursor-big-step
          danylo/cursor-timer (run-with-idle-timer
                               0.5 nil
                               (lambda ()
                                 (setq danylo/cursor-current-step 1)))))
  (unless (memq last-command danylo/cursor-keep-column-commands)
    (setq danylo/cursor-goal-visual-column
          (- (point) (save-excursion (danylo/goto-visual-line-start)))))
  (if (eq dir 'up)
      (previous-line danylo/cursor-current-step)
    (next-line danylo/cursor-current-step))
  (let ((line-length (- (line-end-position) (line-beginning-position))))
    ;; Move to the same column as before.
    (move-to-column
     (+ (save-excursion (danylo/goto-visual-line-start) (current-column))
        (min danylo/cursor-goal-visual-column line-length)))
    )
  )

(defun danylo/cursor-up-smart (&optional arg)
  (interactive "P")
  (danylo/cursor-smart-move 'up arg))

(defun danylo/cursor-down-smart (&optional arg)
  (interactive "P")
  (danylo/cursor-smart-move 'down arg))

(provide 'danylo-cursor-motion)
;;; danylo-cursor-motion.el ends here
