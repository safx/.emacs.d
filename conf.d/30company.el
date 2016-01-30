(require 'company)

;(unless (file-exists-p "~/.emacs.d/ac-dict")
;  (make-directory "~/.emacs.d/ac-dict")
;  (chmod "~/.emacs.d/ac-dict" #o777))

(setq company-tooltip-limit 20)
(setq company-minimum-prefix-length 0)
(setq company-idle-delay .3)
(setq company-echo-delay 0)
(setq company-begin-commands '(self-insert-command))

(define-key company-active-map (kbd "C-h") nil)

(custom-set-faces
 '(company-preview
   ((t (:foreground "darkgray" :underline t))))
 '(company-preview-common
   ((t (:inherit company-preview))))
 '(company-tooltip
   ((t (:background "lightgray" :foreground "black"))))
 '(company-tooltip-selection
   ((t (:background "steelblue" :foreground "white"))))
 '(company-tooltip-common
   ((((type x)) (:inherit company-tooltip :weight bold))
    (t (:inherit company-tooltip))))
 '(company-scrollbar-fg
   ((t (:background "gray64"))))
 '(company-scrollbar-bg
   ((t (:background "white"))))
 '(company-tooltip-common-selection
   ((((type x)) (:inherit company-tooltip-selection :weight bold))
    (t (:inherit company-tooltip-selection)))))
