(require 'buffer-move)

;; up/down need "Mission Control" and"Application windows" disabled in:
;; "System Preferences" -> "Mission Control" -> "Keyboard and Mouse Shortcuts"
(global-set-key (kbd "<C-S-up>")     'buf-move-up)
(global-set-key (kbd "<C-S-down>")   'buf-move-down)
(global-set-key (kbd "<C-S-left>")   'buf-move-left)
(global-set-key (kbd "<C-S-right>")  'buf-move-right)
