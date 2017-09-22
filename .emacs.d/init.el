(require 'package)

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")))

(defvar my-packages '(ag
                      auto-highlight-symbol
                      better-defaults
                      cider
                      company
                      company-go
                      company-jedi
                      exec-path-from-shell
                      find-file-in-project
                      flycheck
                      git-gutter
                      go-eldoc
                      go-mode
                      ido-ubiquitous
                      ido-vertical-mode
                      json-mode
                      markdown-mode
                      neotree
                      smex
                      solarized-theme
                      terraform-mode
                      tide
                      windresize
                      yaml-mode
                      zygospore))

(package-initialize)
(package-refresh-contents)

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(add-to-list 'load-path "~/.emacs.d/vendor")
(dolist (file-name (directory-files "~/.emacs.d/vendor" nil "\\.el$"))
  (load file-name))

(load-theme 'solarized-dark t)

;; auto-highlight-symbol
(require 'auto-highlight-symbol)
(setq ahs-modes (append ahs-modes '(go-mode)))
(global-auto-highlight-symbol-mode t)
(setq ahs-idle-interval 0.1)

;; highlight matching xml/html tags
(require 'hl-tags-mode)
(set-face-background 'highlight "dark slate blue")
(add-hook 'sgml-mode-hook (lambda () (hl-tags-mode 1)))
(add-hook 'nxml-mode-hook (lambda () (hl-tags-mode 1)))

;; exec-path-from-shell
(exec-path-from-shell-initialize)
(exec-path-from-shell-copy-env "PYTHONPATH")
(exec-path-from-shell-copy-env "GOPATH")

;; python
(add-hook 'python-mode-hook
          (lambda ()
            (add-to-list 'company-backends 'company-jedi)))
(add-hook 'python-mode-hook 'jedi:setup)
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

;; neotree
(setq neo-smart-open t)
(setq neo-keymap-style 'concise)
(setq neo-vc-integration '(face char))
(setq neo-autorefresh nil) ;; neotree 0.5.2 will constantly change root if this is non-nil
(require 'neotree)
(add-to-list 'neo-hidden-regexp-list "__pycache__")
(add-to-list 'neo-hidden-regexp-list ".egg-info")

(defun neotree-opened-in-other-frame-p ()
    (and (neo-global--get-window)
         (not (eq (window-frame (neo-global--get-window))
                  (window-frame (selected-window))))))

(defun neotree-project-root ()
  "Open NeoTree using ffip-project-root"
  (interactive)
  (when (neotree-opened-in-other-frame-p)
    (neotree-hide))
  (let ((project-dir (ffip-project-root))
        (file-name (buffer-file-name)))
    (if project-dir
        (progn
          (neotree-dir project-dir)
          (neotree-find file-name))
      (message "ffip couldn't find the project root."))))

;; ido
(ido-mode 1)
(ido-everywhere 1)
(ido-ubiquitous-mode 1)
(ido-vertical-mode 1)
(setq ido-vertical-show-count 1)
(setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)

;; smex
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command) ;; old M-x.

;; find-file-in-project
(setq ffip-prefer-ido-mode t)

;; git-gutter
(global-git-gutter-mode t)
(git-gutter:linum-setup)

;; company
(add-hook 'after-init-hook 'global-company-mode)
(setq company-idle-delay 0.2)
(setq company-minimum-prefix-length 1)

;; eldoc
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'clojure-mode-hook 'turn-on-eldoc-mode)
(add-hook 'python-mode-hook 'turn-on-eldoc-mode)
(add-hook 'go-mode-hook 'go-eldoc-setup)
(setq eldoc-idle-delay 0.2)

;; go
(require 'go-mode)
(require 'company)
(require 'company-go)
(setq gofmt-command "goimports")
(add-hook 'before-save-hook 'gofmt-before-save)
(add-hook 'go-mode-hook (lambda ()
                          (set (make-local-variable 'company-backends) '(company-go))
                          (company-mode)))
(add-hook 'go-mode-hook
          (lambda ()
            (setq tab-width 4)))

(setq company-go-show-annotation t)
(setq company-tooltip-align-annotations t)

;; zygospore
(require 'zygospore)
(defvar zygospore-reopen-neotree-p nil)

(defun zygospore-should-restore-p ()
  (and (equal (selected-window) (next-window))
       (equal (selected-window) zygospore-last-full-frame-window)
       (equal (current-buffer) zygospore-last-full-frame-buffer)))

(global-set-key (kbd "C-x 1")
                (lambda ()
                  (interactive)
                  (if (zygospore-should-restore-p)
                      (progn
                        (zygospore-restore-other-windows)
                        (when zygospore-reopen-neotree-p
                          (neotree-show)
                          (setq zygospore-reopen-neotree-p nil)))
                    (when (neo-global--window-exists-p)
                      (neotree-hide)
                      (setq zygospore-reopen-neotree-p t))
                    (zygospore-delete-other-window))))

;; term
(add-hook 'term-mode-hook
  (function
    (lambda ()
      (setq show-trailing-whitespace nil)
      (linum-mode 0))))

;; http://stackoverflow.com/a/36344479
(with-eval-after-load "term"
  (define-key term-raw-map (kbd "C-y") 'term-paste)
  (define-key term-raw-map (kbd "M-x") 'smex)
  (define-key term-raw-map (kbd "s-v") 'term-paste))

;; TypeScript
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))
(add-hook 'typescript-mode-hook #'setup-tide-mode)

;; ibuffer
(setq ibuffer-default-sorting-mode 'filename/process)

;; flycheck
(global-flycheck-mode)

;; makes the big dumb warning sign go away in emacs 25
(setq visible-bell nil)

;; allow these without asking
(put 'erase-buffer 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; TODO
;; ruby
;; python virtualenv
;; sessions/desktop
;; magit
;; which-key
;; elscreen
;; projectile and/or helm
