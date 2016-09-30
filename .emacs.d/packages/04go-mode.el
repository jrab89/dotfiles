(require 'go-mode)

(setq gofmt-command "goimports")
(add-hook 'before-save-hook 'gofmt-before-save)

(add-hook 'go-mode-hook
  (lambda ()
    (setq tab-width 4)))

(add-hook 'go-mode-hook 'go-eldoc-setup)
