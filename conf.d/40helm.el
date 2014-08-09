(global-set-key (kbd "C-x C-g") 'helm-ls-git-ls)
(global-set-key (kbd "C-x C-r") 'helm-for-files)
;(global-set-key (kbd "M-x") 'helm-M-x)

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
