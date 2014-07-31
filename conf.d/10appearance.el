(set-cursor-color "light green")
(setq initial-frame-alist
        (append '(;;(font . "-shinonome-gothic-medium-r-normal--12-110-75-75-c-60-iso8859-1")
                  (foreground-color . "#ffff90") (background-color . "#303020")
                  (mouse-color . "white")
                  (cursor-color . "light green")
                  (tool-bar-lines . 0) (menu-bar-lines . 0) (vertical-scroll-bars . right)
                  (width . 83) (height . 63)
                  (left . 486) (top . 0))))

(set-face-attribute 'default nil
                    :family "Monaco"
                    :height 120)

(set-fontset-font "fontset-default"
                  'japanese-jisx0208
                  '("Hiragino Maru Gothic ProN"))

(set-fontset-font "fontset-default"
                  'katakana-jisx0201
                  '("Hiragino Maru Gothic ProN"))

