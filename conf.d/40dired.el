(add-hook 'dired-mode-hook
          (lambda () (load "dired-x")
            (global-set-key (kbd "C-x C-j") 'skk-mode)))
