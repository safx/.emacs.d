;; ensure package.el is ready
(require 'package)
(package-initialize)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))

;; ensure init-loader is ready
(when (not (package-installed-p 'init-loader))
  (package-refresh-contents)
  (package-install 'init-loader))

(require 'init-loader)
(init-loader-load (expand-file-name "~/.emacs.d/conf.d"))
