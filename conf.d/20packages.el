(require 'cl)

(defvar installing-package-list
  '(
    company
    company-go
    flymake
    flymake-jshint
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
    ))

(let ((not-installed (loop for x in installing-package-list
                           when (not (package-installed-p x))
                           collect x)))
  (when not-installed
    (package-refresh-contents)
    (dolist (pkg not-installed)
      (package-install pkg))))
