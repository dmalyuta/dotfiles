;;; c-block-comment.el --- 

;;; Commentary:

;;; A function from http://emacs.stackexchange.com/questions/14563/how-to-automatically-create-neat-c-comment-blocks-while-typing
;;; which allows to automatically type C-style block comments

;;; Code:

(defun my-prettify-c-block-comment (orig-fun &rest args)
  (let* ((first-comment-line (looking-back "/\\*\\s-*.*"))
         (star-col-num (when first-comment-line
                         (save-excursion
                           (re-search-backward "/\\*")
                           (1+ (current-column))))))
    (apply orig-fun args)
    (when first-comment-line
      (save-excursion
        (newline)
        (dotimes (cnt star-col-num)
          (insert " "))
        (move-to-column star-col-num)
        (insert "*/"))
      ;;(move-to-column star-col-num) ; comment this line if using bsd style
      ;;(insert "*") ; comment this line if using bsd style
     ))
  ;; Ensure one space between the asterisk and the comment
  (when (not (looking-back " "))
    (insert " ")))
(advice-add 'c-indent-new-comment-line :around #'my-prettify-c-block-comment)
;; (advice-remove 'c-indent-new-comment-line #'my-prettify-c-block-comment)

(provide 'c-block-comment)
;;; c-block-comment.el ends here
