(global-set-key (kbd "C-x C-g") 'helm-ls-git-ls)
(global-set-key (kbd "C-x C-r") 'helm-for-files)
(global-set-key (kbd "H-f") 'helm-do-grep-ag)
(global-set-key (kbd "H-g") 'helm-git-grep)
(global-set-key (kbd "H-p") 'helm-M-x)

(define-key isearch-mode-map (kbd "H-g") 'helm-git-grep-from-isearch)

(add-hook
 'helm-after-initialize-hook
 (lambda ()
   (define-key helm-map (kbd "C-h") 'delete-backward-char)
   (define-key helm-read-file-map (kbd "C-h") 'delete-backward-char)
   (define-key helm-find-files-map (kbd "C-h") 'delete-backward-char)
   (define-key helm-generic-files-map (kbd "C-h") 'delete-backward-char)

   (define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action)
   (define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)
   (define-key helm-generic-files-map (kbd "TAB") 'helm-execute-persistent-action)
))
