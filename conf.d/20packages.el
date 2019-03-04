(defvar installing-package-list
  '(
    ;all-the-icons
    all-the-icons-ivy
    anzu
    aggressive-indent
    beacon
    bind-key
    company
    company-jedi
    counsel
    dired-efap
    dired-k
    dired-single
    ddskk
    dockerfile-mode
    doom-themes
    dumb-jump
    exec-path-from-shell
    expand-region
    flycheck
    flymake
    flymake-jshint
    git-gutter
    go-mode
    guide-key
    haskell-mode
    iedit
    ivy-rich
    js2-mode
    magit
    markdown-mode
    paredit
    rust-mode
    ;scss-mode
    ;swift-mode
    volatile-highlights
    web-mode
    yaml-mode
    ))



(require 'cl)

(defun my:package-dependencies (package)
  "Returns a list of dependencies from a given PACKAGE."
  (let* ((pkg-desc (assq package package-alist))
         (reqs (and pkg-desc (package-desc-reqs (cadr pkg-desc)))))
    (mapcar 'car reqs)))

(defun my:package-all-dependencies (list)
  "Returns a list of dependencies from a given LIST of packages."
  (cl-union
   '(init-loader
     ;pkg-info
     epl)
   (cl-union list
             (cl-mapcan 'my:package-dependencies list))))

(defun my:package-remove (package)
  "Remove a given PACKAGE."
  (let* ((pkg-desc (assq package package-alist))
         (desc (and pkg-desc (cadr pkg-desc))))
    (package-delete desc)))

(defun my:require (package)
  (dolist (pkg (my:package-dependencies package))
    (my:package-install-if-needed pkg)
  (require package)))



(let ((not-installed-packages (loop for x in installing-package-list
                                    when (not (package-installed-p x))
                                    collect x))
      (not-listed-packages (cl-nset-difference package-activated-list
                                               (my:package-all-dependencies installing-package-list))))
  (when not-installed-packages
    (package-refresh-contents)
    (dolist (pkg not-installed-packages)
      (package-install pkg)))

;  (when not-listed-packages
;    (my:require 'pkg-info)
;    (dolist (pkg not-listed-packages)
;      (my:package-remove pkg)))
  )
