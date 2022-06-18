;; <leaf-install-code>
(eval-and-compile
  (customize-set-variable
   'package-archives '(("org" . "https://orgmode.org/elpa/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("gnu" . "https://elpa.gnu.org/packages/")))
  (setq package-native-compile t)
  (setq package-enable-at-startup nil) ; for slightly faster startup
  (package-initialize t)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))

  (leaf leaf-keywords :ensure t
    :init
    ;; optional packages if you want to use :hydra, :el-get, :blackout,,,
    (leaf hydra :ensure nil)
    (leaf el-get :ensure t)
    (leaf blackout :ensure nil)

    :config
    (leaf-keywords-init)))
;; </leaf-install-code>


(leaf *emacs
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

  (setq completion-cycle-threshold 3)
  (setq tab-always-indent 'complete)

  (setq custom-file (concat user-emacs-directory "/custom-set-variables.el"))

  ;; global-settings
  (setq-default major-mode 'lisp-interaction-mode)
  (setq-default indent-tabs-mode nil)
  (setq-default truncate-lines t)
  (setq inhibit-startup-message t)
  (setq next-line-add-newlines nil)
  (setq scalable-fonts-allowed t)
  (setq debug-on-error t)
  ;;(global-display-line-numbers-mode t)
  (global-font-lock-mode t)
  (save-place-mode t)
  (setq global-auto-revert-mode t)
  (setq eval-expression-print-length nil)
  (transient-mark-mode 0)
  (delete-selection-mode 0)

  ;; Show-paren-mode
  (show-paren-mode t)
  ;;(set-face-background 'show-paren-match "#c0c000")

  (setq confirm-kill-emacs 'yes-or-no-p)

  (defun my/kill-line-for-readonly ()
    (interactive)
    (if (not buffer-read-only)
        (kill-line)
      (kill-new (buffer-substring (point) (line-end-position)))))

  (defun my/toggle-truncate-lines ()
    (interactive)
    (setq truncate-lines (not truncate-lines))
    (recenter))

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
  ("C-x q" . my/toggle-truncate-lines)
  ("C-k" . my/kill-line-for-readonly)
  (:minibuffer-local-completion-map
         ("C-w" . backward-kill-word)))


;;; end leaf emacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(leaf *appearance-macos
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


(leaf *appearance-macos
  :if (eq system-type 'darwin)
  :init
  (setq mac-option-modifier 'meta)
  (setq mac-command-modifier 'hyper)
  :bind
  ("H-v" . yank)
  ("H-c" . kill-ring-save)
  ("H-l" . goto-line)
  ("H-z" . undo)
  ("H-r" . jump-to-register)
  ("H-b" . bookmark-jump)
  ("H-C-b" . bookmark-set))


(leaf *appearance-windows
  :if (and (eq system-type 'windows-nt) window-system)
  :config
  (set-face-attribute 'default nil
                      :family "Lucida Console"
                      :height 80)   ;; 9pt
  (set-fontset-font "fontset-default"
                    'japanese-jisx0208
                    '("M+ 1c light" . "iso10646-1"))

  (setq face-font-rescale-alist '((".*M\\+.*" . 1.3))))


(leaf *delete-trailing-whitespace
  :config
  (defvar my/current-cleanup-state "")
  (add-hook 'before-save-hook 'delete-trailing-whitespace)

  (setq-default mode-line-format
                (cons '(:eval my/current-cleanup-state)
                      mode-line-format))

  (defun my/toggle-delete-trailing-whitespaces ()
    (interactive)
    (cond ((memq 'delete-trailing-whitespace before-save-hook)
           (setq my/current-cleanup-state
                 (propertize "WS" 'face '((:foreground "magenta" :weight bold))))
           (remove-hook 'before-save-hook 'delete-trailing-whitespace))
          (t
           (setq my/current-cleanup-state "")
           (add-hook 'before-save-hook 'delete-trailing-whitespace)))
    (force-mode-line-update))

  (global-set-key (kbd "C-x w") 'my/toggle-delete-trailing-whitespaces))


(leaf *load-local-files
  :config
  (let ((file "~/.emacs.d/local-settings.el"))
    (when (file-exists-p file)
      (load-file file))))

;;; end no-require scripts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(leaf *redo
  :load-path "~/.emacs.d/lisp"
  :bind
  ("C-\\" . redo))

;;; end local scriptsa
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(leaf *server
  :hook (emacs-startup-hook . server-start))


(leaf *isearch
  :bind
  (:isearch-mode-map
        ("C-h" . isearch-delete-char)))


(leaf org
  :custom
  (org-agenda-format-date . "%Y-%m-%d %a")
  (org-agenda-prefix-format
   . '((agenda . " ◇ %?-12t % s")
       (todo   . " ◇ ")
       (tags   . " ◇ ")
       (search . " ◇ ")))

  :config
  (setq org-directory "~/Documents/org")
  (setq org-agenda-files (list "~/Documents/org/cal"))
  (setq org-src-fontify-natively t)
  (setq org-special-ctrl-a/e t)
  (setq org-hide-leading-stars t)

;  (setq org-capture-templates
;          '(("t" "Todo" entry (file+headline "~/Documents/org/cal/todo.org" "Tasks")
;             "* TODO %?\n  %i\ncreated in %t")))

  (defun my/org-capture-todo ()
    (interactive)
    (setq org-capture-templates
          (list (list "t" "Today" 'entry (list 'file+headline "~/Documents/org/cal/todo.org" "Tasks")
             (concat "* TODO %?\n  %i\ncreated in " (format-time-string "[%Y-%m-%d %a %H:%M]")))))
    (org-capture nil "t"))

  (defun my/org-insert-today-header ()
    (interactive)
    (end-of-buffer)
    (insert (concat "* " (format-time-string "[%Y-%m-%d %a]\n"))))

  (defun my/affe-grep-org ()
    "search in org directory"
    (interactive)
    (let ((initial-directory org-directory)
          (initial-input "^\\*+ "))
      (consult-ripgrep initial-directory initial-input)))

  (defun my/pull-request-path (tag)
    (let ((project (car (split-string tag "/")))
          (repository (cadr (split-string tag "/")))
          (pr-number (caddr (split-string tag "/"))))
      (concat "git/" project "/" repository "/pullRequests/" pr-number "/diff")))

  :bind
  ("H-u" . my/affe-grep-org)
  ("H-a" . org-agenda)
  ("H-@" . my/org-insert-today-header)
  ("H-^" . my/org-capture-todo)
  (:org-mode-map
              ("H-o" . consult-org-heading))

  :custom-face
  (org-block-begin-line . '((t (:foreground "#900000" :background "#2c2c22"))))
  (org-block-end-line   . '((t (:foreground "#900000" :background "#2c2c22"))))
  (org-block            . '((t (:background "#2c2c22"))))
  (org-level-1 . '((t (:foreground "green" :background "#183818" :bold t :height 1.15))))
  (org-level-2 . '((t (:foreground "green" :background "#182818" :bold t :height 1.10))))
  (org-level-3 . '((t (:foreground "green" :background "#182818" :bold t :height 1.05))))
  (org-level-4 . '((t (:foreground "green" :background "#182818" :bold t :height 1.00))))
  (org-level-5 . '((t (:foreground "green" :background "#182818" :bold t :height 1.00))))
  (org-drawer          . '((t (:foreground "gray" :height 0.66))))
  (org-special-keyword . '((t (:foreground "gray" :height 0.66))))
  (org-property-value  . '((t (:foreground "gray" :height 0.66)))))

(leaf ob-js
  :require org
  :config
  (add-to-list 'org-babel-load-languages '(js . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
  (add-to-list 'org-babel-tangle-lang-exts '("js" . "js")))


(leaf *org-python
  :require org
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t))))


(leaf *recentf
  :custom
  (recentf-auto-cleanup . 99)
  :config
  (recentf-mode))


(leaf *savehist
  :config
  (savehist-mode))

;;; end built-in packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(leaf doom-themes :ensure t
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


(leaf flycheck :ensure t
  :hook (prog-mode-hook . flycheck-mode))


(leaf company :ensure t
  :hook (prog-mode-hook . company-mode)
  :config
  (setq company-tooltip-limit 20)
  (setq company-minimum-prefix-length 0)
  (setq company-idle-delay .3)
  (setq company-echo-delay 0)
  (setq company-begin-commands '(self-insert-command))

  :bind (:company-active-map
              ("C-h" . nil))

  :custom-face
  (company-preview . '((t (:foreground "darkgray" :underline t))))
  (company-preview-common . '((t (:inherit company-preview))))
  (company-tooltip . '((t (:background "lightgray" :foreground "black"))))
  (company-tooltip-selection . '((t (:background "steelblue" :foreground "white"))))
  (company-tooltip-common . '((((type x)) (:inherit company-tooltip :weight bold))
                           (t (:inherit company-tooltip))))
  (company-scrollbar-fg . '((t (:background "gray64"))))
  (company-scrollbar-bg . '((t (:background "white"))))
  (company-tooltip-common-selection . '((((type x)) (:inherit company-tooltip-selection :weight bold))
                                     (t (:inherit company-tooltip-selection)))))


(leaf lsp-mode :ensure t
  :commands lsp
  :bind ("H-:" . lsp-describe-thing-at-point))


(leaf lsp-ui :ensure t)


(leaf anzu :ensure t
  :bind (:esc-map
              ("%" . anzu-query-replace)
              ("&" . query-replace-regexp)))


(leaf beacon :ensure t
  :custom
  (beacon-mode . t)
  (beacon-blink-delay . 0.1)
  (beacon-blink-duration . 0.1)
  (beacon-color . "#eeee44"))


(leaf dired-single :ensure t)


(leaf exec-path-from-shell :ensure t
  :if (not (eq system-type 'windows-nt))
  :config
  (let ((envs '("PATH")))
    (exec-path-from-shell-copy-envs envs)))


(leaf git-gutter :ensure t
  :config
  (global-git-gutter-mode +1)

  :custom
  (git-gutter:modified-sign . "┃")
  (git-gutter:added-sign    . "┃")
  (git-gutter:deleted-sign  . "┃")

  :custom-face
  (git-gutter:modified . '((t (:foreground "#c96cd5"))))
  (git-gutter:added    . '((t (:foreground "#43a234"))))
  (git-gutter:deleted  . '((t (:foreground "#e7766d")))))


(leaf iedit :ensure t
  :bind
  ("C-;" . iedit-mode)

  :bind (:iedit-mode-keymap
              ("M-p" . iedit-prev-occurrence)
              ("M-n" . iedit-next-occurrence)
              ("C-h" . backward-delete-char-untabify)))


(leaf magit :ensure t
  :bind ("C-#" . magit-status))


(leaf vertico :ensure t
  :config
  (vertico-mode)
  (setq vertico-count 40)
  (setq vertico-cycle t)
  (define-key vertico-map (kbd "C-w") 'backward-kill-word))
  ;; It does not working correctly
  ;:bind (:vertico-map
  ;       ("C-w" . backward-kill-word)))

(leaf orderless :ensure t
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))


(leaf projectile :ensure t)


(leaf consult :ensure t
  :init
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)

  :hook (completion-list-mode-hook . consult-preview-at-point-mode)

  :config
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-root-function #'projectile-project-root)

  (setq consult-async-min-input 2)
  (setq consult-async-input-throttle 0.3)
  (setq consult-find-command "fd --color=never --full-path ARG OPTS")

  :bind
  (("C-x C-r" . consult-recent-file)
   ("C-x b" . consult-buffer)
   ("H-o" . consult-outline)
   ("H-l" . consult-line)
   ("H-g" . consult-git-grep)
   ("H-f" . consult-ripgrep)
   ("H-q" . consult-apropos)
   ("C-x C-g" . consult-find)
   ("M-y" . consult-yank-pop)))


(leaf affe :ensure t)


(leaf marginalia :ensure t
  :config
  (marginalia-mode))


(leaf embark :ensure t
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


(leaf embark-consult :ensure t
  :after (embark consult)
  ;:demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode-hook . consult-preview-at-point-mode))


(leaf volatile-highlights :ensure t
  :config
  (volatile-highlights-mode))


(leaf web-mode :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.js$" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode)))


(leaf ddskk :ensure t
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


(leaf highlight-indent-guides :ensure t
  :config
  (setq highlight-indent-guides-method 'bitmap)
  :hook
  (yaml-mode-hook . highlight-indent-guides-mode)
  (prog-mode-hook . highlight-indent-guides-mode))


(leaf origami :ensure t)
(leaf tree-sitter :ensure t)
(leaf tree-sitter-langs :ensure t)
(leaf *tsi
  :el-get "orzechowskid/tsi.el")
(leaf *tsx-mode
  :require origami tree-sitter tree-sitter-langs tsi
  :el-get "orzechowskid/tsx-mode.el"
  :config
  (add-to-list 'auto-mode-alist '("\\.tsx$" . tsx-mode)))

;;; other modes
(leaf rjsx-mode :ensure t)
(leaf yaml-mode :ensure t)
(leaf markdown-mode :ensure t)
(leaf dockerfile-mode :ensure t)
(leaf docker-compose-mode :ensure t)
(leaf toml-mode :ensure t)


(leaf rust-mode :ensure t
  ;; :custom (rust-format-on-save . t)
  :hook (rust-mode-hook . lsp))


(leaf cargo :ensure t
  :hook (rust-mode-hook . cargo-minor-mode))


(leaf flycheck-rust :ensure t
  :require flycheck
  :config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))


(leaf docker :ensure t
  :bind ("H-d" . docker))


(leaf docker-tramp :ensure t :disabled t
  :config
  (set-variable 'docker-tramp-use-names t))

;;; init.el ends here
