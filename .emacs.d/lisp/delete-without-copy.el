;;; delete-without-copy.el --- 

;;; Commentary:

;;; A function from http://ergoemacs.org/emacs/emacs_kill-ring.html
;;; which makes sure Emacs does not put words deleted with C-backspace or C-delete into the kill ring (i.e. does not "cut" them)

;;; Code:

(defun my-forward-delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument, do this that many times.
This command does not push text to `kill-ring'."
  (interactive "p")
  (delete-region
   (point)
   (progn
     (forward-word arg)
     (point))))

(defun my-backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (backward-word arg) (point))))

(provide 'delete-without-copy)
;;; delete-without-copy.el ends here
