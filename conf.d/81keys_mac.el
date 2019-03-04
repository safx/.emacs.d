(when (eq system-type 'darwin)

  ;; https://gist.github.com/railwaycat/3498096

  (setq mac-option-modifier 'meta)
  (setq mac-command-modifier 'hyper)

  (global-set-key (kbd "H-a") 'mark-whole-buffer)
  (global-set-key (kbd "H-v") 'yank)
  (global-set-key (kbd "H-c") 'kill-ring-save)
  ;(global-set-key (kbd "H-s") 'save-buffer)
  (global-set-key (kbd "H-l") 'goto-line)
  (global-set-key (kbd "H-z") 'undo)
)
