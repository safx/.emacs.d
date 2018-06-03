;; langage settings
(setenv "LANG" "C")
(setenv "LC_ALL" "C")
(setenv "LC_TIME" "C")

;; coding system
(set-language-environment "Japanese")

(prefer-coding-system 'utf-8)
;(set-default-coding-systems 'utf-8)
;(setq coding-system-for-read 'utf-8)

;; global-settings
(setq-default major-mode 'lisp-interaction-mode)
(setq-default indent-tabs-mode nil)
(setq-default truncate-lines t)
(setq inhibit-startup-message t)
(setq next-line-add-newlines nil)
(setq scalable-fonts-allowed t)
(setq debug-on-error t)
(line-number-mode t)
(global-font-lock-mode t)
(setq global-auto-revert-mode t)
(setq eval-expression-print-length nil)
(transient-mark-mode 0)
(delete-selection-mode 0)

;; Show-paren-mode
(show-paren-mode t)
;(set-face-background 'show-paren-match-face "#008000")

;; ask when exit
(setq confirm-kill-emacs 'yes-or-no-p)
