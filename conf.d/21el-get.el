(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(require 'el-get)
(setq el-get-dir "~/.emacs.d/el-get")

;; Avoiding build error on installing some package in Windows 
;; http://misohena.jp/blog/2014-03-01-el-get-error-async-install-on-windows.html
(when (eq system-type 'windows-nt) 
  (setq my-el-get-shell-file-name
        (or (and (eq system-type 'windows-nt)
                 (executable-find "cmdproxy.exe"))
            shell-file-name))
  
  (defadvice el-get-start-process-list (around my-el-get-start-process-list--modify-shell-file-name activate)
    (let ((shell-file-name my-el-get-shell-file-name))
      ad-do-it)))

;; define personal el-get packages
(setq el-get-sources
      '(
        (:name gh-ddskk
               :type github
               :pkgname "hsaito/ddskk"
	       :autoloads nil
	       :info "doc/skk.info"
	       :features ("skk-setup")
	       :build `((,el-get-emacs "-batch" "-q" "-no-site-file" "-l" "SKK-MK" "-f" "SKK-MK-compile")
			(,el-get-emacs "-batch" "-q" "-no-site-file" "-l" "SKK-MK" "-f" "SKK-MK-compile-info")
			(,(if (eq system-type 'windows-nt) "move" "mv") "skk-setup.el.in" "skk-setup.el")))
        ))


;; install personal packages
(el-get 'sync 
  '(
    gh-ddskk
    ))
