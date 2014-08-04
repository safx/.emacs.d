
(when (eq system-type 'windows-nt)
  (set-face-attribute 'default nil
                      :family "Lucida Console"
                      :height 80)   ;; 9pt
  (set-fontset-font "fontset-default"
                    'japanese-jisx0208
                    '("M+ 1c light" . "iso10646-1"))

  (setq face-font-rescale-alist '((".*M\\+.*" . 1.3))))

