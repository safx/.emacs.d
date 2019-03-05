(require 'counsel)
(require 'ivy-rich)

(ivy-rich-mode t)
;(counsel-mode t)
(all-the-icons-ivy-setup)

(setq ivy-height 30)

(define-key global-map (kbd "H-s") 'swiper)
(define-key global-map (kbd "H-r") 'counsel-recentf)
(define-key global-map (kbd "H-o") 'counsel-find-file)
(define-key global-map (kbd "H-p") 'counsel-M-x)
(define-key global-map (kbd "C-H-p") 'ivy-resume)
(define-key global-map (kbd "H-g") 'counsel-git-grep)
(define-key global-map (kbd "H-f") 'counsel-ag)
(define-key global-map (kbd "H-q") 'counsel-apropos)
(define-key global-map (kbd "C-x C-g") 'counsel-git)
(define-key global-map (kbd "H-b") 'ivy-switch-buffer)

(define-key ivy-minibuffer-map (kbd "C-w") 'ivy-yank-word)
