;;; my-python.el --- A better python shell mode

;; Author: Danylo Malyuta

;; This file is not part of GNU Emacs

;;; Commentary:
;;
;; Provides facilities to better work with Python inside Emacs.
;;
;;; Code:

(require 'python)

(defcustom my-python-shell-type "ipython"
  "Which shell type to use?."
  :type 'string)

(defcustom my-python-buffer-name "*MyPython*"
  "Name of the Python buffer."
  :type 'string)

(defcustom my-python-shell-position '((side . right))
  "Position of the python shell."
  :type 'alist)

(defun my-python-check-open-shell ()
  "Check if a shell is running."
  (if (get-buffer my-python-buffer-name)
      t
    nil))

(defun my-python-shell ()
  "Create a Python shell.
Placed in the current buffer."
  (interactive)
  (if (my-python-check-open-shell)
      (progn
	;; The Python shell buffer exists
	(if (get-buffer-window my-python-buffer-name)
	    ;; The buffer is already displayed, switch to it
	    (progn
	      (pop-to-buffer my-python-buffer-name))
	  ;; The buffer is hidden, show it
	  (progn
	    (switch-to-buffer my-python-buffer-name)
	    (display-buffer-in-side-window
	     my-python-buffer-name my-python-shell-position)
	    (switch-to-buffer (other-buffer))
	    ))
	)
    (progn
      ;; The Python shell buffer does not exist
      (let ((this-buffer (buffer-name)))
	(progn
	  ;; Create a shell buffer
	  (ansi-term my-python-shell-type)
	  (rename-buffer my-python-buffer-name)
	  ;; Create a side window to hold the vterm buffer
	  (display-buffer-in-side-window
	   my-python-buffer-name my-python-shell-position)
	  ;; Switch back to current buffer
	  (switch-to-buffer this-buffer)
	  )
	)
      )
    )
  )

(defun my-python-run-file ()
  "Run the current file in the python shell.
If there is to python shell open, prints a message to inform."
  (interactive)
  (if (my-python-check-open-shell)
      (progn
	;; Send a run command for the current file
	(let ((file-name (file-name-nondirectory (buffer-file-name)))
	      (this-buffer (buffer-name)))
	  (progn
	    (pop-to-buffer my-python-buffer-name)
	    (term-send-raw-string (format "%%run -i %s\n" file-name))
	    (pop-to-buffer this-buffer))
	  )
	)
    (message "No Python buffer."))
  )

(defun my-python-config ()
  ;; Key bindings
  (define-key python-mode-map (kbd "C-c C-p") 'my-python-shell)
  (define-key python-mode-map (kbd "C-c C-l") 'my-python-run-file)
  ;; Hide shell buffers from buffer list, so they may only be accessed
  ;; through the above key bindings
  ;; See https://github.com/abo-abo/swiper/issues/644
  (setq ivy-ignore-buffers '("\\*MyPython\\*"))
  )

(provide 'my-python)
;;; my-python.el ends here
