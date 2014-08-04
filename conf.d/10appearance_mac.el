
(when (eq system-type 'darwin)
  (set-face-attribute 'default nil
                      :family "Monaco"
                      :height 120)

  (set-fontset-font "fontset-default"
                    'japanese-jisx0208
                    '("Hiragino Maru Gothic ProN"))

  (set-fontset-font "fontset-default"
                    'katakana-jisx0201
                    '("Hiragino Maru Gothic ProN")))

