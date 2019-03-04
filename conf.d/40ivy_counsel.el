(require 'counsel)
(require 'ivy-rich)

(ivy-rich-mode t)
;(counsel-mode t)
(all-the-icons-ivy-setup)

(setq ivy-height 30)

(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "H-o") 'counsel-find-file)
(global-set-key (kbd "H-p") 'counsel-M-x)
(global-set-key (kbd "C-H-p") 'ivy-resume)
(global-set-key (kbd "H-g") 'counsel-git-grep)
(global-set-key (kbd "H-f") 'counsel-ag)
(global-set-key (kbd "H-b") 'ivy-switch-buffer)
(global-set-key (kbd "C-x C-g") 'counsel-git)
