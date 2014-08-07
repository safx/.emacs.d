(require 'iedit)
(global-set-key (kbd "C-;") 'iedit-mode)
(add-hook 'iedit-mode-hook
          (lambda ()
            (define-key iedit-mode-keymap (kbd "C-h") 'backward-delete-char-untabify)))
