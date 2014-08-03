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


(defun my:package-dependencies (package)
  "Returns a list of dependencies from a given PACKAGE."
  (let* ((pkg-desc (assq package package-alist))
         (reqs (and pkg-desc (package-desc-reqs (cdr pkg-desc)))))
    (mapcar 'car reqs)))

(defun my:package-all-dependencies (list)
  "Returns a list of dependencies from a given LIST of packages."
  (cl-union '(init-loader)
            (cl-union list
                      (cl-mapcan 'my:package-dependencies list))))




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
;    (dolist (pkg not-listed-packages)
;      (package-uninstall pkg nil)))
  )
