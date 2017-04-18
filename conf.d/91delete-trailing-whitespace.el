;http://syohex.hatenablog.com/entry/20130617/1371480584

(defvar my/current-cleanup-state "")

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq-default mode-line-format
              (cons '(:eval my/current-cleanup-state)
                    mode-line-format))

(defun toggle-delete-trailing-whitespaces ()
  (interactive)
  (cond ((memq 'delete-trailing-whitespace before-save-hook)
         (setq my/current-cleanup-state
               (propertize "WS" 'face '((:foreground "magenta" :weight bold))))
         (remove-hook 'before-save-hook 'delete-trailing-whitespace))
        (t
         (setq my/current-cleanup-state "")
         (add-hook 'before-save-hook 'delete-trailing-whitespace)))
  (force-mode-line-update))
(global-set-key (kbd "C-x w") 'toggle-delete-trailing-whitespaces)
