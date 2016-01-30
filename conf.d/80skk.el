;(require 'skk-autoloads)
(setq skk-date-ad nil)
(setq skk-number-style nil)
(setq skk-rom-kana-rule-list
      '(("@" nil "@")
        ("z " nil "　")
        ("z^" nil "♥")
        ("z," nil "・")
        ("z/" nil "／")
        ("z!" nil "！")
        ("z{" nil "【】")))

(global-set-key "\C-x\C-j" 'skk-mode)
