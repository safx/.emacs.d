(add-to-list 'exec-path "/usr/local/go/bin")
(add-to-list 'exec-path (expand-file-name "~/dev/golang/bin"))

;; golang
(add-hook 'go-mode-hook  '(lambda ()
                            (require 'go-eldoc)
                            ;(require 'flymake-go)
                            (require 'company)
                            (require 'helm-go-package)

                            (setq-default) 
                            (setq tab-width 2) 
                            (setq standard-indent 2) 
                            (setq indent-tabs-mode nil)

                            (setq company-tooltip-limit 20)
                            (setq company-minimum-prefix-length 0)
                            (setq company-idle-delay .3)
                            (setq company-echo-delay 0)
                            (setq company-begin-commands '(self-insert-command))
                            (set (make-local-variable 'company-backends)
                                 '(company-go))
                            (company-mode t)

                            (custom-set-faces
                             '(company-preview
                               ((t (:foreground "darkgray" :underline t))))
                             '(company-preview-common
                               ((t (:inherit company-preview))))
                             '(company-tooltip
                               ((t (:background "lightgray" :foreground "black"))))
                             '(company-tooltip-selection
                               ((t (:background "steelblue" :foreground "white"))))
                             '(company-tooltip-common
                               ((((type x)) (:inherit company-tooltip :weight bold))
                                (t (:inherit company-tooltip))))
                             '(company-scrollbar-fg
                               ((t (:background "gray64"))))
                             '(company-scrollbar-bg
                               ((t (:background "white"))))
                             '(company-tooltip-common-selection
                               ((((type x)) (:inherit company-tooltip-selection :weight bold))
                                (t (:inherit company-tooltip-selection)))))
                            
                            (go-eldoc-setup)
                            ;(flymake-mode t)
                            ))

