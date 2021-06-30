(eval-when-compile
  ;; package setup
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
  (package-initialize t)
  (setq package-enable-at-startup nil) ; for slightly faster startup

  ;;; install use-package if not installed
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))

  (require 'use-package)
  (require 'use-package-ensure)
  (setq use-package-always-ensure t))



(use-package emacs :ensure nil :no-require
  :init
  ;; langage settings
  (setenv "LANG" "C")
  (setenv "LC_ALL" "C")
  (setenv "LC_TIME" "C")

  ;; coding system
  (set-language-environment "Japanese")

  (prefer-coding-system 'utf-8)
  ;;(set-default-coding-systems 'utf-8)
  ;;(setq coding-system-for-read 'utf-8)

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
  ;;(set-face-background 'show-paren-match "#c0c000")

  (setq confirm-kill-emacs 'yes-or-no-p)

  ;; common appearances
  (set-fringe-mode '(1 . 1))
  (set-cursor-color "light green")
  (setq initial-frame-alist
        (append '(
                  ;;(foreground-color . "#ffff90") (background-color . "#303020")
                  (mouse-color . "white")
                  (cursor-color . "light green")
                  (tool-bar-lines . 0) (menu-bar-lines . 0)
                  ;;(inhibit-double-buffering . t)
                  (width . 83) (height . 63)
                  (left . 486) (top . 0))))

  ;; common key binds
  :bind
  ("¥" . "\\")
  ("C-M-g" . top-level)
  ("C-_" . undo)
  ("C-z" . call-last-kbd-macro)
  ("C-h" . backward-delete-char-untabify)
  ("M-h" . backward-kill-word)
  ("M-SPC" . cycle-spacing)
  ("C-<return>" . cua-rectangle-mark-mode)
  ("M-g" . goto-line)
  ("M-o" . dabbrev-expand)
  ("C-o" . other-window)

  ("C-x q" . (lambda ()
               (interactive)
               (setq truncate-lines (not truncate-lines))
               (recenter)))

  ("C-k" . (lambda ()
             (interactive)
             (if (not buffer-read-only)
                 (kill-line)
               (kill-new (buffer-substring (point) (line-end-position))))))

  (:map minibuffer-local-completion-map
         ("C-w" . backward-kill-word)))

;;; end use-package emacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package appearance-macos :ensure nil :no-require
  :if (and (eq system-type 'darwin) window-system)
  :config
    (let* ((size 14)
           (jpfont "Hiragino Maru Gothic ProN")
           (asciifont "Menlo")
           (h (* size 10)))
      (set-face-attribute 'default nil :family asciifont :height h)
      (set-fontset-font t 'katakana-jisx0201 jpfont)
      (set-fontset-font t 'japanese-jisx0208 jpfont)
      (set-fontset-font t 'japanese-jisx0212 jpfont)
      (set-fontset-font t 'japanese-jisx0213-1 jpfont)
      (set-fontset-font t 'japanese-jisx0213-2 jpfont)
      (set-fontset-font t '(#x0080 . #x024F) asciifont))
    (setq face-font-rescale-alist
          '(("^-apple-hiragino.*" . 1.2)
            (".*-Hiragino Maru Gothic ProN-.*" . 1.2)
            (".*osaka-bold.*" . 1.2)
            (".*osaka-medium.*" . 1.2)
            (".*courier-bold-.*-mac-roman" . 1.0)
            (".*monaco cy-bold-.*-mac-cyrillic" . 0.9)
            (".*monaco-bold-.*-mac-roman" . 0.9)
            ("-cdac$" . 1.3)))

    (setq frame-inherited-parameters '(font tool-bar-lines))

    (setq initial-frame-alist
          (append initial-frame-alist
                  '((ns-appearance . dark)
                    (ns-transparent-titlebar . t)
                    )))

    ;; mouse setting
    (setq mouse-wheel-tilt-scroll t)
    (setq mouse-wheel-flip-direction t))


(use-package appearance-macos :ensure nil :no-require
  :if (eq system-type 'darwin)
  :init
  (setq mac-option-modifier 'meta)
  (setq mac-command-modifier 'hyper)
  :bind
  ("H-a" . mark-whole-buffer)
  ("H-v" . yank)
  ("H-c" . kill-ring-save)
  ("H-l" . goto-line)
  ("H-z" . undo))


(use-package appearance-windows :ensure nil :no-require
  :if (and (eq system-type 'windows-nt) window-system)
  :config
  (set-face-attribute 'default nil
                      :family "Lucida Console"
                      :height 80)   ;; 9pt
  (set-fontset-font "fontset-default"
                    'japanese-jisx0208
                    '("M+ 1c light" . "iso10646-1"))

  (setq face-font-rescale-alist '((".*M\\+.*" . 1.3))))


(use-package delete-trailing-whitespace :ensure nil :no-require
  :config
  (defvar my/current-cleanup-state "")
  (add-hook 'before-save-hook 'delete-trailing-whitespace)

  (setq-default mode-line-format
                (cons '(:eval my/current-cleanup-state)
                      mode-line-format))

  (defun toggle-delete-trailing-whitespaces ()
    (interactive)
    (cond ((memq 'delete-trailing-whitespace before-save-hook)
           (setq my/current-cleanup-state
                 (propertize "WS" 'face '((:foreground "magenta" :weight bold))))
           (remove-hook 'before-save-hook 'delete-trailing-whitespace))
          (t
           (setq my/current-cleanup-state "")
           (add-hook 'before-save-hook 'delete-trailing-whitespace)))
    (force-mode-line-update))

  (global-set-key (kbd "C-x w") 'toggle-delete-trailing-whitespaces))

;;; end no-require scripts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package redo :ensure nil :no-require
  :load-path "~/.emacs.d/lisp"
  :bind
  ("C-\\" . redo))

;;; end local scriptsa
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package server :ensure nil
  :defer 6
  :config
  (server-start))


(use-package isearch :ensure nil
  :bind
  (:map isearch-mode-map
        ("C-h" . isearch-delete-char)))


(use-package org :ensure nil
  :defer 9
  :config
  (setq org-src-fontify-natively t)

  :custom-face
  (org-block-begin-line ((t (:foreground "#900000" :background "#2c2c22"))))
  (org-block-end-line   ((t (:foreground "#900000" :background "#2c2c22"))))
  (org-block            ((t (:background "#2c2c22"))))
  (org-level-1 ((t (:foreground "green" :background "#183818" :bold t :height 1.15))))
  (org-level-2 ((t (:foreground "green" :background "#182818" :bold t :height 1.10))))
  (org-level-3 ((t (:foreground "green" :background "#182818" :bold t :height 1.05))))
  (org-level-4 ((t (:foreground "green" :background "#182818" :bold t :height 1.00))))
  (org-level-5 ((t (:foreground "green" :background "#182818" :bold t :height 1.00)))))


(use-package recentf :ensure nil
  :custom
  (recentf-auto-cleanup 99)
  :config
  (recentf-mode))


(use-package savehist :ensure nil
  :config
  (savehist-mode))

;;; end built-in packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-tomorrow-night t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Enable custom neotree theme (all-the-icons must be installed!)
  ;;(doom-themes-neotree-config);

  ;; or for treemacs users
  ;;(setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  ;;(doom-themes-treemacs-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)

  (set-face-background 'show-paren-match "#c0c000"))


(use-package flycheck
  :custom-face
  :hook (prog-mode . flycheck-mode))


(use-package company
  :hook (prog-mode . company-mode)
  :config
  (setq company-tooltip-limit 20)
  (setq company-minimum-prefix-length 0)
  (setq company-idle-delay .3)
  (setq company-echo-delay 0)
  (setq company-begin-commands '(self-insert-command))

  :bind (:map company-active-map
              ("C-h" . nil))

  :custom-face
  (company-preview ((t (:foreground "darkgray" :underline t))))
  (company-preview-common ((t (:inherit company-preview))))
  (company-tooltip ((t (:background "lightgray" :foreground "black"))))
  (company-tooltip-selection ((t (:background "steelblue" :foreground "white"))))
  (company-tooltip-common ((((type x)) (:inherit company-tooltip :weight bold))
                           (t (:inherit company-tooltip))))
  (company-scrollbar-fg ((t (:background "gray64"))))
  (company-scrollbar-bg ((t (:background "white"))))
  (company-tooltip-common-selection ((((type x)) (:inherit company-tooltip-selection :weight bold))
                                     (t (:inherit company-tooltip-selection)))))


(use-package lsp-mode
  :bind ("H-:" . lsp-describe-thing-at-point)
  :hook (rust-mode . lsp))

(use-package lsp-ui)


(use-package anzu
  :bind (:map esc-map
              ("%" . anzu-query-replace)
              ("&" . query-replace-regexp)))


(use-package beacon
  :custom
  (beacon-mode t)
  (beacon-blink-delay 0.1)
  (beacon-blink-duration 0.1)
  (beacon-color "#eeee44"))


(use-package dired-single)


(use-package exec-path-from-shell
  :if (not (eq system-type 'windows-nt))
  :config
  (let ((envs '("PATH")))
    (exec-path-from-shell-copy-envs envs)))


(use-package git-gutter
  :config
  (global-git-gutter-mode +1)

  :custom
  (git-gutter:modified-sign "!")
  (git-gutter:added-sign "+")
  (git-gutter:deleted-sign "-")

  :custom-face
  (git-gutter:modified ((t (:foreground "purple"))))
  (git-gutter:added    ((t (:foreground "green" ))))
  (git-gutter:deleted  ((t (:foreground "red"   )))))


(use-package iedit
  :bind
  ("C-;" . iedit-mode)

  :bind (:map iedit-mode-keymap
              ("M-p" . iedit-prev-occurrence)
              ("M-n" . iedit-next-occurrence)
              ("C-h" . backward-delete-char-untabify)))


(use-package magit
  :defer 3
  :bind ("C-#" . magit-status))


(use-package vertico
  :config
  (vertico-mode)
  (setq vertico-count 40)
  (setq vertico-cycle t)
  (define-key vertico-map (kbd "C-w") 'backward-kill-word))


(use-package orderless
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))


(use-package projectile)


(use-package consult
  :init
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)

  :hook (completion-list-mode . consult-preview-at-point-mode)

  :config
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-root-function #'projectile-project-root)

  (setq consult-async-min-input 2)
  (setq consult-async-input-throttle 0.3)
  (setq consult-find-command "fd --color=never --full-path ARG OPTS")

  :bind
  (("C-x C-r" . consult-recent-file)
   ("C-x b" . consult-buffer)
   ("H-t" . consult-outline)
   ("H-l" . consult-line)
   ("H-g" . consult-git-grep)
   ("H-f" . consult-ripgrep)
   ("H-q" . consult-apropos)
   ("C-x C-g" . consult-find)
   ("M-y" . consult-yank-pop)))


(use-package marginalia
  :config
  (marginalia-mode))


(use-package embark
  :bind
  (("H-." . embark-act)         ;; pick some comfortable binding
   ("H-;" . embark-dwim))       ;; good alternative: M-.

  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))


(use-package embark-consult
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))


(use-package volatile-highlights
  :config
  (volatile-highlights-mode))


(use-package web-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.js$" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode)))


;;; other modes
(use-package rjsx-mode)
(use-package rust-mode)
(use-package yaml-mode)
(use-package markdown-mode)
(use-package dockerfile-mode)
(use-package toml-mode)


(use-package docker-tramp
  :config
  (set-variable 'docker-tramp-use-names t))


(use-package ddskk
  :config
  (setq skk-date-ad nil)
  (setq skk-number-style nil)
  (setq skk-rom-kana-rule-list
        '(("@" nil "@")
          ("z " nil "　")
          ("z^" nil "♥")
          ("z," nil "・")
          ("z/" nil "／")
          ("z!" nil "！")
          ("z{" nil "【】")))
  :bind
  ("C-x C-j" . skk-mode))

;;; Rust
(use-package rust-mode
  ;; :custom (rust-format-on-save t)
  :hook (rust-mode . lsp))


(use-package cargo
  :hook (rust-mode . cargo-minor-mode))


(use-package flycheck-rust
  :config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))
