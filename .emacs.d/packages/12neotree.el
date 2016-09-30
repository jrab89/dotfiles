(setq-default neo-keymap-style 'concise) ;; this has to be before requiring neotree, not sure why

(require 'neotree)

(setq
  neo-show-hidden-files t
  neo-modern-sidebar t
  neo-dont-be-alone t
  neo-banner-message nil
  neo-create-file-auto-open t
  neo-persist-show nil
  ;; neo-vc-integration '(face char) ;; this makes lots of files show up as added (+)
  neo-smart-open t)
