(require 'helm-projectile)

(global-set-key (kbd "C-x C-p") 
                '(lambda ()
                   (interactive)
                   (if (projectile-project-p)
                       (helm-projectile)
                     (helm-projectile-switch-project))))
