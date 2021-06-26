(require 'vertico)
(vertico-mode)
(setq vertico-count 40)


(require 'orderless)
(setq completion-styles '(orderless)
      completion-category-defaults nil
      completion-category-overrides '((file (styles . (partial-completion)))))


(add-hook
 'after-init-hook
 (lambda ()
   (vertico-mode)
   (marginalia-mode)
   (savehist-mode)))


(require 'consult)
(require 'embark)
(require 'embark-consult)


(setq consult-async-min-input 2)
(setq consult-async-input-throttle 0.3)
(setq consult-find-command "fd --color=never --full-path ARG OPTS")

;;(setq consult-project-root-function #'vc-root-dir)
(autoload 'projectile-project-root "projectile")
(setq consult-project-root-function #'projectile-project-root)


(define-key global-map (kbd "C-x C-r") 'consult-recent-file)
(define-key global-map (kbd "C-x b") 'consult-buffer)
(define-key global-map (kbd "H-t") 'consult-outline)
(define-key global-map (kbd "H-l") 'consult-line)
(define-key global-map (kbd "H-g") 'consult-git-grep)
(define-key global-map (kbd "H-f") 'consult-ripgrep)
(define-key global-map (kbd "H-q") 'consult-apropos)
(define-key global-map (kbd "C-x C-g") 'consult-find)
(define-key global-map (kbd "M-y") 'consult-yank-pop)
