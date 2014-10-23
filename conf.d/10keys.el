(global-set-key "(J\(B" "\\")
(global-set-key "\C-_" 'undo)
(global-set-key "\C-z" 'call-last-kbd-macro)
(global-set-key "\C-h" 'backward-delete-char-untabify)
(global-set-key "\M-h" 'backward-kill-word)
(global-set-key (kbd "M-SPC") 'cycle-spacing)
(global-set-key [(control return)] 'cua-rectangle-mark-mode)
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key "\M-o" 'dabbrev-expand)
(global-set-key (kbd "C-o") 'other-window)
;(global-set-key [(super ?o)] 'ispell-complete-word)
;(global-set-key "\M-@" 'lisp-complete-symbol)
;(global-set-key "\C-xq" 'menu-bar-mode)

(global-set-key "\C-xq"
                '(lambda ()
                   (interactive)
                   (setq truncate-lines (not truncate-lines))
                   (recenter)))

;; copy when read only mode
(global-set-key "\C-k"
                '(lambda ()
                   (interactive)
                   (if (not buffer-read-only)
                       (kill-line)
                     (kill-new (buffer-substring (point) (line-end-position))))))

;; define-keys
;(define-key esc-map "&" 'query-replace-regexp)
(define-key global-map "\C-x\C-h" 'help-command)
(define-key minibuffer-local-completion-map "\C-w" 'backward-kill-word)
