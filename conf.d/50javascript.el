;(setenv "NODE_PATH" "/usr/local/share/npm/lib/node_modules")
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/share/npm/bin")))


;;; js2-mode
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))


;;; flymake-jshint
(add-hook 'js2-mode-hook '(lambda ()
              (require 'flymake-jshint)
              (flymake-jshint-load)
              ;(color-identifiers-mode)
              ))

