(require 'jedi-core)

(setq jedi:complete-on-dot t)
(setq jedi:use-shortcuts t)
(add-hook 'python-mode-hook
          '(lambda ()
             (company-mode t)
             (jedi:setup)
             (jedi:install-server)))

(add-to-list 'company-backends 'company-jedi)
