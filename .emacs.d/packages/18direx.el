(require 'direx)

(add-hook 'direx:direx-mode-hook
  (function
    (lambda()
      (setq show-trailing-whitespace nil)
      (linum-mode 0))))

(setq direx:leaf-icon "  ")
(setq direx:closed-icon "▷ ")
(setq direx:open-icon "▽ ")
