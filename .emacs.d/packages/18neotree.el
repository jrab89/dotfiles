(setq-default neo-keymap-style 'concise) ;; this has to be before requiring neotree, not sure why

(require 'neotree)

(setq
  neo-banner-message nil
  neo-create-file-auto-open t
  neo-smart-open t)
