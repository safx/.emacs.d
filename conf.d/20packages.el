(require 'cl)

(defvar installing-package-list
  '(
    haskell-mode
    helm
    magit
    markdown-mode
    ))

(let ((not-installed (loop for x in installing-package-list
                           when (not (package-installed-p x))
                           collect x)))
  (when not-installed
    (package-refresh-contents)
    (dolist (pkg not-installed)
      (package-install pkg))))
