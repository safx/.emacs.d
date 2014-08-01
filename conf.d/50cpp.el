(add-hook 'c-mode-common-hook
          '(lambda()
             (c-set-style "cc-mode")
             (local-set-key  (kbd "C-c C-h") 'ff-find-other-file)
             (setq-default indent-tabs-mode nil)
             (setq c-basic-offset 4)
             (setq-default tab-width 4)))
