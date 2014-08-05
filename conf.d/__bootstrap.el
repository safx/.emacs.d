(defun my:package-install-if-needed (pkg)
  (when (not (package-installed-p pkg))
    (when my:package-refresh-contents
      (setq package-refresh-contents t)
      (package-refresh-contents))
    (package-install pkg)))


;; ensure package.el is ready
(require 'package)
(package-initialize)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))

(my:package-install-if-needed 'init-loader)
(require 'init-loader)
(init-loader-load (expand-file-name "~/.emacs.d/conf.d"))
