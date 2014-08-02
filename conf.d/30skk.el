(require 'skk-autoloads)
;(setq skk-large-jisyo "~/.emacs.d/skk-jisyo.ML")
(setq skk-date-ad nil)
(setq skk-number-style nil)
(setq skk-rom-kana-rule-list
      '(("@" nil "@")
        ("z " nil "　")
        ("z{" nil "【】")
        ("z}" nil "〔〕")))


;(global-set-key "\C-x\C-j" 'toggle-input-method)
(global-set-key "\C-x\C-j" 'skk-mode)
(global-set-key (kbd "C-x j") 'skk-auto-fill-mode)



