(require 'web-mode)

;; (setenv "NODE_PATH" "/usr/local/share/npm/lib/node_modules")
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/share/npm/bin")))

;; web-mode
(add-to-list 'auto-mode-alist '("\\.js$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (when (equal web-mode-content-type "javascript")
              (web-mode-set-content-type "jsx")
              (require 'flycheck)
              (flycheck-add-mode 'javascript-eslint 'web-mode)
              (flycheck-mode t)
              )))
