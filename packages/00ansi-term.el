(require 'term)

;; don't show trailing whitespace in term
(add-hook 'term-mode-hook
  (function
    (lambda ()
      (setq show-trailing-whitespace nil))))

;; http://stackoverflow.com/a/36344479
(with-eval-after-load "term"
  (define-key term-raw-map (kbd "C-c C-y") 'term-paste)
  (define-key term-raw-map (kbd "s-v") 'term-paste))
