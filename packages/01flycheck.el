(require 'flycheck)

(add-hook 'after-init-hook #'global-flycheck-mode)
(setq flycheck-display-errors-delay 0.1)
(with-eval-after-load 'flycheck
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc ruby-lint)))

(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-typescript-tslint-setup))
