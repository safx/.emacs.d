(defvar installing-package-list
  '(
    company
    company-go
    expand-region
    flymake
    flymake-jshint
    git-gutter
    go-mode
    go-eldoc
    haskell-mode
    helm
    helm-ls-git
    helm-go-package
    iedit
    js2-mode
    magit
    markdown-mode
    swift-mode
    volatile-highlights
    ))



(require 'cl)

(defun my:package-dependencies (package)
  "Returns a list of dependencies from a given PACKAGE."
  (let* ((pkg-desc (assq package package-alist))
         (reqs (and pkg-desc (package-desc-reqs (cdr pkg-desc)))))
    (mapcar 'car reqs)))

(defun my:package-all-dependencies (list)
  "Returns a list of dependencies from a given LIST of packages."
  (cl-union '(init-loader pkg-info epl)
            (cl-union list
                      (cl-mapcan 'my:package-dependencies list))))

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

  (when not-listed-packages
    (my:require 'pkg-info)
    (dolist (pkg not-listed-packages)
      (let* ((pkgs (format "%s" pkg))
             (ver (pkg-info-package-version pkg))
             (vers (pkg-info-format-version ver)))
        (package-delete pkgs vers))))
  )
