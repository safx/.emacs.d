(require 'lsp-mode)

(add-hook 'web-mode-hook #'lsp)
(add-hook 'rust-mode-hook #'lsp)
(add-hook 'rjsx-mode-hook #'lsp)


(require 'lsp-ui)
(add-hook 'lsp-mode-hook 'lsp-ui-mode)
