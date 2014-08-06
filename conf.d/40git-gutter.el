(require 'git-gutter)

(global-git-gutter-mode +1)

(custom-set-variables
 '(git-gutter:modified-sign "!")
 '(git-gutter:added-sign "+")
 '(git-gutter:deleted-sign "-"))

(set-face-background 'git-gutter:modified "purple")
(set-face-foreground 'git-gutter:added "green")
(set-face-foreground 'git-gutter:deleted "red")
