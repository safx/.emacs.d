(require 'cask "~/.cask/cask.el")
(cask-initialize)

(require 'init-loader)
(init-loader-load (expand-file-name "~/.emacs.d/conf.d"))
