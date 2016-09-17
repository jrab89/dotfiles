(require 'flycheck)

(add-hook 'after-init-hook #'global-flycheck-mode)
(setq flycheck-display-errors-delay 0.1)
(setq flycheck-checker-error-threshold 800)
(with-eval-after-load 'flycheck
  (flycheck-pos-tip-mode)
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc ruby-lint)))
