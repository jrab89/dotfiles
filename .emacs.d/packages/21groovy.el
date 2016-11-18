(require 'cl)
(require 'groovy-mode)

(add-to-list 'auto-mode-alist '("\\Jenkinsfile\\'" . groovy-mode))
(add-hook 'groovy-mode-hook
 (lambda ()
   (setq c-basic-offset 4)
   (c-set-offset 'label 4)))
