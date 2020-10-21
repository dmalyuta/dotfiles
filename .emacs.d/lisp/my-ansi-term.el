;;; my-ansi-term.el
;;
;; Improvements to ansi-term

;;;;;; Improving ansi-term

;; make that C-c t e launches an ansi-term buffer in the current window
(global-set-key (kbd "C-c t e") 'ansi-term)

;; avoid ansi-term asking always which shell to run (always run bash)
(defvar my-term-shell "/bin/bash")
(defadvice ansi-term (before force-bash)
  (interactive (list my-term-shell)))
(ad-activate 'ansi-term)

;; display of certain characters and control codes to UTF-8
(defun my-term-use-utf8 ()
  (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))
(add-hook 'term-exec-hook 'my-term-use-utf8)

;; clickable URLs
(defun my-term-hook ()
  (goto-address-mode))
(add-hook 'term-mode-hook 'my-term-hook)

;; make that typing exit in ansi-term (which exits the shell) also
;; closes the buffer
(defadvice term-sentinel (around my-advice-term-sentinel (proc msg))
  (if (memq (process-status proc) '(signal exit))
      (let ((buffer (process-buffer proc)))
	ad-do-it
	(kill-buffer buffer))
    ad-do-it))
(ad-activate 'term-sentinel)

;; fix the problem of moving cursor left/right being captured by emacs
;; instead of underlying terminal, leading to jumbling when jumping
;; words then editing the middle of a command. Same for deleting
;; backward/forward whole words.
(eval-after-load "term"
  '(progn
     ;; rename buffer
     (define-key term-raw-map (kbd "C-c r") 'rename-buffer)

     ;; unbind C-z, which minimizes current frame
     (define-key term-raw-map (kbd "C-z") 'nil)

     ;; make sure typical key combos work in term-char-mode
     (define-key term-raw-map (kbd "M-x") 'nil)
     (define-key term-raw-map (kbd "M-&") 'nil)
     (define-key term-raw-map (kbd "M-!") 'nil)

     ;; make sure C-c t e launches a new ansi-term buffer when current
     ;; buffer is also ansi-term
     (define-key term-raw-map (kbd "C-c t e") 'nil)

     ;; move by whole words fix
     (defun term-send-Cright () (interactive) (term-send-raw-string "\e[1;5C"))
     (defun term-send-Cleft  () (interactive) (term-send-raw-string "\e[1;5D"))
     (defun term-send-Mbackspace () (interactive)(term-send-raw-string "\e\d"))

     ;; word deletion fix
     (define-key term-raw-map (kbd "C-w") 'term-send-Mbackspace)
     (define-key term-raw-map (kbd "M-<backspace>") 'term-send-Mbackspace)

     ;; Completion via S-SPC as well as TAB
     ;; (Bind S-SPC to send TAB to term)
     (defun term-send-tab () (interactive) (term-send-raw-string "\t"))
     (define-key term-raw-map (kbd "S-SPC") 'term-send-tab)

     ;; switch between char and line mode with logical keystrokes make
     ;; sure that in line mode, the buffer is auto-set to read-only
     ;; such that I don't accidentally edit the terminal in
     ;; term-line-mode, thus messing up the command in term-char-mode
     ;; (the mode in which I normally work, with term-line-mode used
     ;; for scrolling up/down the buffer)
     (defun my-switch-to-term-line-mode ()
       (interactive)
       (term-line-mode)
       (if (not buffer-read-only)
	   (toggle-read-only)))
     (defun my-switch-to-term-char-mode ()
       (interactive)
       (term-char-mode)
       (if buffer-read-only
	   (toggle-read-only)))
     (add-hook 'term-mode-hook (lambda () (local-set-key (kbd "C-c t c") 'my-switch-to-term-char-mode)))
     (define-key term-raw-map (kbd "C-c t l") 'my-switch-to-term-line-mode)

     ;; copy/paste native Emacs keystrokes
     (define-key term-raw-map (kbd "C-k") 'term-send-raw)
     (define-key term-raw-map (kbd "C-y") 'term-paste)

     ;; ensure that scrolling doesn't break on output
     ;;(setq term-scroll-show-maximum-output t)
     (setq term-scroll-to-bottom-on-output t)

     ;; max history (# lines) to keep (0 == keep everything)
     (setq term-buffer-maximum-size 50000)))

;; make sure window movement keys are not captured by shell
(add-hook 'matlab-shell-mode-hook
	  (lambda ()
	    (define-key matlab-shell-mode-map (kbd "C-<up>") 'nil)
	    (define-key matlab-shell-mode-map (kbd "C-<down>") 'nil)
	    (define-key matlab-shell-mode-map (kbd "C-<left>") 'nil)
	    (define-key matlab-shell-mode-map (kbd "C-<right>") 'nil)))

;; make sure window movement keys are not captured by terminal
(add-hook 'term-mode-hook
	  (lambda ()
	    (define-key term-raw-map (kbd "C-<up>") 'nil)
	    (define-key term-raw-map (kbd "C-<down>") 'nil)
	    (define-key term-raw-map (kbd "C-<left>") 'nil)
	    (define-key term-raw-map (kbd "C-<right>") 'nil)))

;; Turn off line wrap in ansi term
(add-hook 'term-mode-hook
	  (lambda ()
	    (setq truncate-lines t)))

;; ansi-term bi-directional text support problem fix, which seems to
;; be the cause of text jumbling when going back commands in
;; ansi-term. This fixes it, yay!
(add-hook 'term-mode-hook 'my-term-mode-hook)
(defun my-term-mode-hook ()
  ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=20611
  (setq bidi-paragraph-direction 'left-to-right))

(eval-after-load 'term
  '(progn
     (defun my-term-send-delete-word-forward () (interactive) (term-send-raw-string "\ed"))
     (defun my-term-send-delete-word-backward () (interactive) (term-send-raw-string "\e\C-h"))
     (define-key term-raw-map [C-delete] 'my-term-send-delete-word-forward)
     (define-key term-raw-map [C-backspace] 'my-term-send-delete-word-backward)
     (defun my-term-send-forward-word () (interactive) (term-send-raw-string "\ef"))
     (defun my-term-send-backward-word () (interactive) (term-send-raw-string "\eb"))
     (define-key term-raw-map [C-left] 'my-term-send-backward-word)
     (define-key term-raw-map [C-right] 'my-term-send-forward-word)
     (defun my-term-send-m-right () (interactive) (term-send-raw-string "\e[1;3C"))
     (defun my-term-send-m-left () (interactive) (term-send-raw-string "\e[1;3D"))
     (define-key term-raw-map [M-right] 'my-term-send-m-right)
     (define-key term-raw-map [M-left] 'my-term-send-m-left)
     ))

;; No "are you sure" query on exit
(defun set-no-process-query-on-exit ()
  (let ((proc (get-buffer-process (current-buffer))))
    (when (processp proc)
      (set-process-query-on-exit-flag proc nil))))
(add-hook 'term-exec-hook 'set-no-process-query-on-exit)

(provide 'my-ansi-term)
