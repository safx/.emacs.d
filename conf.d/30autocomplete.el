(require 'auto-complete)

(unless (file-exists-p "~/.emacs.d/ac-dict")
  (make-directory "~/.emacs.d/ac-dict")
  (chmod "~/.emacs.d/ac-dict" #o777))
