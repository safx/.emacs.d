(require 'swift-mode)

(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(unless (file-exists-p "~/.emacs.d/ac-dict/swift-mode")
  (url-copy-file "https://raw.githubusercontent.com/andelf/Defines-Swift/master/misc/swift-mode"
                 "~/.emacs.d/ac-dict/swift-mode"))

(add-hook 'swift-mode-hook
          #'(lambda ()
              (auto-complete-mode t)
              (add-to-list 'ac-sources 'ac-source-files-in-current-dir)
              (electric-pair-mode t)
              (add-to-list 'ac-sources 'ac-source-dictionary)
              ))
