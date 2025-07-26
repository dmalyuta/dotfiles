;;; danylo-scrollbar.el --- Better scrolling. -*- lexical-binding: t; -*-
;;
;; Author: Danylo Malyuta
;;
;; Keywords: scroll
;;
;; This file is not part of GNU Emacs
;;
;;; Commentary:
;;
;;  Scrollbar implementation that works both in GUI and terminal modes. Built
;;  on top of Yascroll.
;;
;;; Code:

(defconst danylo/scrollbar-max-repeat-interval 0.025
  "Allowed interval between subsequent scrollbar refreshes, before it gets
hidden temporarily.")

(defconst danylo/scrollbar-initial-hide-interval 0.25
  "Time interval to not show the scrollbar for after it first gets hidden.")

(defvar danylo/scrollbar-show-always t
  "Always update and show the scrollbar, which effectively disables the state
machine below.")

(defvar-local danylo/scrollbar-last-show-time 0.0
  "Last time that the scrollbar show function was called.")

(defvar-local danylo/scrollbar-hide nil
  "Hides the scrollbar when active.")

(defun danylo/get-line-number (pos)
  "Get the line number."
  (let ((linum (save-excursion
                 (goto-char pos)
                 (string-to-number (format-mode-line "%l")))))
    (if (eq linum 0)
        (count-lines (point-min) pos)
      linum)))

(defun danylo/yascroll--display ()
  "The main code to display the scrollbar. It's a more performant version of
`yascroll:show-scroll-bar-internal'."
  (when-let ((scroll-bar (yascroll:choose-scroll-bar)))
    (let ((window-lines (yascroll:window-height))
          (buffer-lines (danylo/get-line-number (point-max))))
      (when (< window-lines buffer-lines)
        (save-excursion
          (font-lock-fontify-region
           (line-beginning-position) (line-end-position))
          (font-lock-flush))
        (let* ((scroll-top (danylo/get-line-number (window-start)))
               (thumb-window-line (yascroll:compute-thumb-window-line
                                   window-lines buffer-lines scroll-top))
               (thumb-buffer-line (+ scroll-top thumb-window-line))
               (thumb-size (yascroll:compute-thumb-size
                            window-lines buffer-lines))
               (make-thumb-overlay
                (cl-ecase scroll-bar
                  (right-fringe 'yascroll:make-thumb-overlay-right-fringe)
                  (text-area 'yascroll:make-thumb-overlay-text-area))))
          (yascroll:make-thumb-overlays make-thumb-overlay
                                        thumb-window-line
                                        thumb-size)
          (yascroll:schedule-hide-scroll-bar))))))

(defun danylo/yascroll:show-scroll-bar-internal (&rest _)
  "Show scroll bar in buffer."
  (if danylo/scrollbar-show-always
      ;; Always show.
      (progn
        (setq danylo/scrollbar-hide nil)
        (danylo/yascroll--display))
    ;; Conditional display logic.
    (unless danylo/scrollbar-hide
      (let* ((now (float-time))
             (dt (- now danylo/scrollbar-last-show-time)))
        (setq danylo/scrollbar-last-show-time now)
        (if (> dt danylo/scrollbar-max-repeat-interval)
            (danylo/yascroll--display)
          (setq danylo/scrollbar-hide t)
          (run-with-timer
           danylo/scrollbar-initial-hide-interval nil
           (lambda (buffer)
             (run-with-idle-timer
              (danylo/run-after-idle-interval danylo/scrollbar-max-repeat-interval)
              nil
              (lambda (buffer)
                (with-current-buffer buffer
                  (setq danylo/scrollbar-hide nil)
                  (danylo/yascroll:show-scroll-bar-internal)))
              buffer))
           (current-buffer)))))))
(advice-add 'yascroll:show-scroll-bar-internal
            :around #'danylo/yascroll:show-scroll-bar-internal)

(defun danylo/yascroll:debounced-scroll (orig-fun &rest args)
  (let ((danylo/scrollbar-show-always nil))
    (apply orig-fun args)))
(advice-add 'yascroll:after-window-scroll
            :around #'danylo/yascroll:debounced-scroll)
(advice-add 'yascroll:after-window-configuration-change
            :around #'danylo/yascroll:debounced-scroll)

(defun danylo/yascroll:after-mouse-scroll (&rest _)
  (yascroll:after-window-scroll (selected-window) nil))
(advice-add 'pixel-scroll-precision :after
            #'danylo/yascroll:after-mouse-scroll)

(defun danylo/yascroll:make-thumb-overlays-fast
    (orig-fun make-thumb-overlay window-line size)
  (save-excursion
    ;; Jump to the line.
    (goto-char (window-start))
    ;; `forward-line' is faster, but moves by logical lines instead of visual
    ;; lines. This will result in scrollbar gaps for wrapped
    ;; lines. `vertical-line' moves by visual lines, but is computationally
    ;; slower.
    ;; (forward-line window-line)
    (vertical-motion window-line)
    ;; Make thumb overlays.
    (condition-case nil
        (cl-loop repeat size
                 do
                 (progn
                   (push (funcall make-thumb-overlay) yascroll:thumb-overlays)
                   ;; (forward-line 1)
                   (vertical-motion 1))
                 until (eobp))
      (end-of-buffer nil))))
(advice-add 'yascroll:make-thumb-overlays
            :around #'danylo/yascroll:make-thumb-overlays-fast)

(defun danylo/yascroll:make-thumb-overlay-fringe (orig-fun left-or-right)
  "Make thumb overlay on the LEFT-OR-RIGHT fringe.
Fixed such that fringe is not absent on lines where text received
properties later on, which is common in in org-mode."
  (let* ((pos (save-excursion
                (end-of-visual-line)
                (point)))
         ;; If `pos' is at the beginning of line, overlay of the
         ;; fringe will be on the previous visual line.
         (pos (if (= (line-end-position) pos) pos (1+ pos)))
         (display-string `(,left-or-right filled-rectangle yascroll:thumb-fringe))
         (after-string (propertize "." 'display display-string))
         (overlay (make-overlay pos pos)))
    (overlay-put overlay 'after-string after-string)
    (overlay-put overlay 'fringe-helper t)
    (overlay-put overlay 'window (selected-window))
    (overlay-put overlay 'priority yascroll:priority)
    overlay))
(advice-add 'yascroll:make-thumb-overlay-fringe
            :around #'danylo/yascroll:make-thumb-overlay-fringe)

(defun danylo/yascroll:make-thumb-overlay-text-area (orig-fun)
  "Not documented."
  ;; Make sure that text is contified, i.e, no JIT behavior.
  (cl-destructuring-bind (edge-pos edge-padding)
      (yascroll:line-edge-position)
    ;; Find if scrollbar already exists at point.
    (let ((overlays (overlays-at edge-pos))
          found
          scrollbar-overlay)
      (while overlays
        (let ((overlay (car overlays)))
          (if (overlay-get overlay 'scrollbar-overlay)
              (setq found overlay)))
        (setq overlays (cdr overlays)))
      (if found
          found
        (let* ((lep (line-end-position))
               (is-folded (invisible-p lep)))
          (if (= edge-pos lep)
              (let ((overlay (make-overlay edge-pos edge-pos))
                    (after-string
                     (concat (make-string (1- edge-padding) ?\ )
                             (propertize " " 'face 'yascroll:thumb-text-area))))
                (put-text-property 0 1 'cursor t after-string)
                (overlay-put overlay 'after-string after-string)
                (overlay-put overlay 'window (selected-window))
                (overlay-put overlay 'scrollbar-overlay t)
                overlay)
            (let ((overlay (make-overlay edge-pos (1+ edge-pos)))
                  (display-string
                   (propertize " "
                               'face 'yascroll:thumb-text-area
                               'cursor t)))
              (unless is-folded
                (overlay-put overlay 'display display-string)
                (overlay-put overlay 'window (selected-window))
                (overlay-put overlay 'priority yascroll:priority)
                (overlay-put overlay 'scrollbar-overlay t))
              overlay)))))))
(advice-add 'yascroll:make-thumb-overlay-text-area
            :around #'danylo/yascroll:make-thumb-overlay-text-area)

;; Disable scrollbar update on text change.
(add-hook
 'yascroll-bar-mode-hook
 (lambda ()
   (remove-hook 'before-change-functions 'yascroll:before-change t)
   (remove-hook 'after-change-functions 'yascroll:after-change t)))

(provide 'danylo-scrollbar)
;;; danylo-scrollbar.el ends here
