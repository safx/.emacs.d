(defun my:package-install-if-needed (pkg)
  (when (not (package-installed-p pkg))
    (when (not (boundp 'my:package-refresh-contents))
      (setq my:package-refresh-contents t)
      (package-refresh-contents))
    (package-install pkg)))


;; ensure package.el is ready
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(my:package-install-if-needed 'init-loader)
(require 'init-loader)
(init-loader-load (expand-file-name "~/.emacs.d/conf.d"))
