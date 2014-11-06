(add-hook 'c-mode-common-hook
          '(lambda()
             (require 'find-file)
             (nconc (cadr (assoc "\\.h\\'" cc-other-file-alist)) '(".m" ".mm"))
             (setq cc-other-file-alist (append cc-other-file-alist '(("\\.m\\'" (".h")) ("\\.mm\\'" (".h")))))

             (c-set-style "cc-mode")
             (global-set-key  (kbd "C-c C-h") 'ff-find-other-file)
             (setq-default indent-tabs-mode nil)
             (setq c-basic-offset 4)
             (setq-default tab-width 4)))
