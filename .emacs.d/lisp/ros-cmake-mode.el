;;; ros-cmake-mode.el --- major-mode for editing ROS CMakeLists.txt files

;;------------------------------------------------------------------------------

;;; Commentary:

;; Derived from cmake-lists.el
;; https://cmake.org/gitweb?p=cmake.git;a=blob_plain;hb=master;f=Auxiliary/cmake-mode.el
;; See the cmake-lists.el file for all the commentary
;;
;; The only change done here is a line indent function is used which gives zero
;; indent to ), i.e. the end of a list of options

;;------------------------------------------------------------------------------

;;; Code:

(require 'cmake-mode)

;;
;; Line indentation function.
;; Mostly copied from cmake-mode.el except for the "ADDED BY danyloM" part
(defun ros-cmake-indent ()
  "Indent current line as CMake code."
  (interactive)
  (unless (cmake-line-starts-inside-string)
    (if (bobp)
        (cmake-indent-line-to 0)
      (let (cur-indent)
        (save-excursion
          (beginning-of-line)
          (let ((point-start (point))
                (case-fold-search t)  ;; case-insensitive
                token)
            ;; Search back for the last indented line.
            (cmake-find-last-indented-line)
	    ;; Start with the indentation on this line.
            (setq cur-indent (current-indentation))
	    ;; Counter for how many strings matched in previous line
	    (setq match-counter 0)
            ;; Search forward counting tokens that adjust indentation.
            (while (re-search-forward cmake-regex-token point-start t)
              (setq token (match-string 0))
	      (setq match-counter (+ match-counter 1))
              (when (or (string-match (concat "^" cmake-regex-paren-left "$") token)
                        (and (string-match cmake-regex-block-open token)
                             (looking-at (concat "[ \t]*" cmake-regex-paren-left))))
                (setq cur-indent (+ cur-indent cmake-tab-width)))
	      (when (string-match (concat "^" cmake-regex-paren-right "$") token)
		(setq cur-indent (- cur-indent cmake-tab-width)))
              )
            (goto-char point-start)
            ;; If next token closes the block, decrease indentation
            (when (looking-at cmake-regex-close)
              (setq cur-indent (- cur-indent cmake-tab-width))
              )
	    ;; ADDED BY danyloM: do not indent the closing parenthesis ")"
	    (if (looking-at "^[ \t]*\)[ \t]*")
		(setq cur-indent (- cur-indent cmake-tab-width)) ;; if
              (when (and (string-match (concat "^" cmake-regex-paren-right "$") token)
			 (= match-counter 1))
		;; else: in case previous line was just ")", then do not decrease indent
		(setq cur-indent (+ cur-indent cmake-tab-width))))
            )
          )
        ; Indent this line by the amount selected.
        (cmake-indent-line-to (max cur-indent 0))
        )
      )
    )
  )

;;;###autoload
(define-derived-mode ros-cmake-mode cmake-mode "CMake[ROS]"
  "Major mode for editing ROS CMakeLists.txt files."
  
  ;; Setup indentation function.
  (set (make-local-variable 'indent-line-function) 'ros-cmake-indent))

(provide 'ros-cmake-mode)

;;; ros-cmake-mode.el ends here
