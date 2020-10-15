;;; my-python.el --- A better python shell mode

;; Author: Danylo Malyuta

;; This file is not part of GNU Emacs

;;; Commentary:
;;
;; Provides facilities to better work with Python inside Emacs.
;;
;;; Code:

(require 'vterm)

(defun my-python-config ()
  (define-key vterm-mode-map (kbd "S-SPC") 'vterm-send-tab)
  )

(provide 'my-python)
;;; my-python.el ends here
