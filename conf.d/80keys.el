(bind-key "¥" "\\")
(bind-key "C-M-g" 'top-level)
(bind-key "C-_" 'undo)
(bind-key "C-z" 'call-last-kbd-macro)
(bind-key "C-h" 'backward-delete-char-untabify)
(bind-key "M-h" 'backward-kill-word)
(bind-key "M-SPC" 'cycle-spacing)
(bind-key* "C-<return>" 'cua-rectangle-mark-mode)
(bind-key "M-g" 'goto-line)
(bind-key "M-o" 'dabbrev-expand)
(bind-key "C-o" 'other-window)
;(bind-key [(super ?o)] 'ispell-complete-word)
;(bind-key "M-@" 'lisp-complete-symbol)

(bind-key "C-x q"
                '(lambda ()
                   (interactive)
                   (setq truncate-lines (not truncate-lines))
                   (recenter)))

;; copy when read only mode
(bind-key "C-k"
                '(lambda ()
                   (interactive)
                   (if (not buffer-read-only)
                       (kill-line)
                     (kill-new (buffer-substring (point) (line-end-position))))))

;; define-keys
;(define-key esc-map "&" 'query-replace-regexp)
(define-key global-map "\C-x\C-h" 'help-command)
(define-key minibuffer-local-completion-map "\C-w" 'backward-kill-word)
