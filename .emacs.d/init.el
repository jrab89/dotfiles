(require 'package)

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")))

(defvar my-packages '(auto-complete
                      auto-highlight-symbol
                      better-defaults
                      exec-path-from-shell
                      flycheck
                      git-gutter
                      helm
                      helm-projectile
                      jedi
                      json-mode
                      markdown-mode
                      projectile
                      terraform-mode
                      windresize
                      yaml-mode
                      zenburn-theme
                      zygospore))

(package-initialize)
(package-refresh-contents)

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(add-to-list 'load-path "~/.emacs.d/vendor")
(dolist (file-name (directory-files "~/.emacs.d/vendor" nil "\\.el$"))
  (load file-name))

(load-theme 'zenburn t)

;; autocomplete
(ac-config-default)
(global-auto-complete-mode t)

;; helm
(require 'helm-config)
(require 'helm)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)

;; projectile
(require 'projectile)
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)
(global-set-key (kbd "C-c c") 'helm-projectile)

;; auto-highlight-symbol
(require 'auto-highlight-symbol)
(setq ahs-modes (append ahs-modes '()))
(global-auto-highlight-symbol-mode t)
(setq ahs-idle-interval 0.1)

;; highlight matching xml/html tags
(require 'hl-tags-mode)
(set-face-background 'highlight "dark slate blue")
(add-hook 'sgml-mode-hook (lambda () (hl-tags-mode 1)))
(add-hook 'nxml-mode-hook (lambda () (hl-tags-mode 1)))

;; exec-path-from-shell
(exec-path-from-shell-initialize)

;; without this, flycheck will run "/usr/local/bin/flake8", causing errors that look like:
;; "Suspicious state from syntax checker python-flake8: Checker python-flake8 returned non-zero exit code 1, but no errors from output"
(setq flycheck-python-flake8-executable "/Users/jrabovsky/git_repos/dotfiles/.emacs.d/.python-environments/default/bin/flake8")
(setq jedi:server-args '("--virtual-env" "/Users/jrabovsky/git_repos/dotfiles/.emacs.d/.python-environments/default"))
(add-hook 'python-mode-hook 'jedi:setup)
(add-hook 'python-mode-hook 'jedi:ac-setup)
(setq jedi:get-in-function-call-delay 200)
(setq jedi:complete-on-dot t)
(setq jedi:tooltip-method nil)

;; Show line numbers
(global-linum-mode)

;; Highlight current line
(global-hl-line-mode t)

(windmove-default-keybindings)

(setq-default show-trailing-whitespace t)

(defun toggle-show-trailing-whitespace ()
  "Toggle show-trailing-whitespace"
  (interactive)
  (setq show-trailing-whitespace (not show-trailing-whitespace)))

;; minibuffer
(add-hook 'minibuffer-setup-hook
          (lambda ()
            (setq show-trailing-whitespace nil)))

;; Refresh buffers when files change on disk
(global-auto-revert-mode t)

;; Save Emacs state between sessions
(desktop-save-mode t)

;; http://stackoverflow.com/a/24809045
;; increase font
(global-set-key (kbd "s-=")
  (lambda ()
    (interactive)
    (let* ((old-face-attribute (face-attribute 'default :height))
           (new-face-attribute (+ 10 old-face-attribute)))
      (message (number-to-string (/ new-face-attribute 10)))
      (set-face-attribute 'default nil :height new-face-attribute))))

;; decrease font
(global-set-key (kbd "s--")
  (lambda ()
    (interactive)
    (let* ((old-face-attribute (face-attribute 'default :height))
           (new-face-attribute (- old-face-attribute 10)))
      (message (number-to-string (/ new-face-attribute 10)))
      (set-face-attribute 'default nil :height new-face-attribute))))

(require 'server)
(unless (server-running-p)
  (server-start))

;; git-gutter
(global-git-gutter-mode t)
(git-gutter:linum-setup)

;; eldoc
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'python-mode-hook 'turn-on-eldoc-mode)
(setq eldoc-idle-delay 0.2)

;; zygospore
(require 'zygospore)
(global-set-key (kbd "C-x 1") 'zygospore-toggle-delete-other-windows)

;; term
(add-hook 'term-mode-hook
  (function
    (lambda ()
      (setq show-trailing-whitespace nil)
      (linum-mode 0))))

;; http://stackoverflow.com/a/36344479
(with-eval-after-load "term"
  (define-key term-raw-map (kbd "C-y") 'term-paste)
  (define-key term-raw-map (kbd "s-v") 'term-paste))

;; ibuffer
(setq ibuffer-default-sorting-mode 'filename/process)

;; flycheck
(require 'flycheck)
(setq-default flycheck-disabled-checkers '(python-pylint emacs-lisp-checkdoc))
(global-flycheck-mode)

;; makes the big dumb warning sign go away in emacs 25
(setq visible-bell nil)

;; allow these without asking
(put 'erase-buffer 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; TODO
;; ruby
;; no-easy-keys
;; better python virtualenv support?
;; sessions/desktop
;; magit
;; which-key
;; elscreen
;; dumb jump
