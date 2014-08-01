(require 'iedit)
(global-set-key (kbd "C-;") 'iedit-mode)
(add-hook 'iedit-mode-hook
          '(lambda()
             (local-set-key "\C-h" 'backward-delete-char-untabify)))
