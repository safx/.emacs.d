(add-to-list 'exec-path "/usr/local/go/bin")
(add-to-list 'exec-path (expand-file-name "~/dev/golang/bin"))

;; golang
(add-hook 'go-mode-hook
          '(lambda ()
             (require 'go-eldoc)
             ;;(require 'flymake-go)
             (require 'helm-go-package)

             (setq-default) 
             (setq tab-width 2) 
             (setq standard-indent 2) 
             (setq indent-tabs-mode nil)
             
             (go-eldoc-setup)
             ;;(flymake-mode t)
             ))

