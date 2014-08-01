(add-to-list 'load-path "~/.emacs.d/lisp")



(require 'redo)
(global-set-key "\C-\\" 'redo)


(require 'yank-pop-summary)
(global-set-key "\M-y" 'yank-pop-forward)
(global-set-key "\C-\M-y" 'yank-pop-backward)


(require 'dabbrev-highlight)
(set-face-background dabbrev-highlight-face "#0000c0")


(require 'narrow-or-widen-dwim)
(global-set-key (kbd "M-N") 'narrow-or-widen-dwim)
