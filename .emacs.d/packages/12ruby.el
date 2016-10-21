(require 'inf-ruby)

(setq inf-ruby-default-implementation "pry")
(add-hook 'inf-ruby-mode-hook
  (function
    (lambda ()
      (setq show-trailing-whitespace nil)
      (linum-mode 0))))

(add-hook 'inf-ruby-mode-hook
  (function
    (lambda ()
      (setq show-trailing-whitespace nil)
      (linum-mode 0))))

;; https://github.com/purcell/ac-inf-ruby
;; (eval-after-load 'auto-complete
;;  '(add-to-list 'ac-modes 'inf-ruby-mode))

;; (add-hook 'inf-ruby-mode-hook 'ac-inf-ruby-enable)
;; (eval-after-load 'inf-ruby '
;;  '(define-key inf-ruby-mode-map (kbd "TAB") 'auto-complete))
