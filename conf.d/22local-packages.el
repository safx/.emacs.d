(add-to-list 'load-path "~/.emacs.d/lisp")


(require 'redo)
(global-set-key "\C-\\" 'redo)


(require 'dabbrev-highlight)
(set-face-background dabbrev-highlight-face "#0000c0")
