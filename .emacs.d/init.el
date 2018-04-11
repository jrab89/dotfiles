(require 'package)

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")))

(defvar my-packages '(auto-complete
                      auto-highlight-symbol
                      better-defaults
                      counsel-projectile
                      dockerfile-mode
                      exec-path-from-shell
                      flycheck
                      git-gutter
                      haml-mode
                      ivy
                      jedi
                      json-mode
                      magit
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

;; prevent emacs from adding coding information in the first line
;; https://stackoverflow.com/a/6454077
(setq ruby-insert-encoding-magic-comment nil)

;; https://emacs.stackexchange.com/a/28077
(global-set-key (kbd "<backspace>")
                '(lambda () (interactive) (backward-delete-char 1 nil)))

;; https://stackoverflow.com/a/42488960
(setq select-enable-primary nil)

;; https://stackoverflow.com/a/384346
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))


;; Set projectile project name as frame title
;; https://emacs.stackexchange.com/a/35443
(setq frame-title-format
      '(""
        "%b"
        (:eval
         (let ((project-name (projectile-project-name)))
           (unless (string= "-" project-name)
             (format " in [%s]" project-name))))))

(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(setq ivy-use-selectable-prompt t)
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)

(add-hook 'ruby-mode-hook
  (lambda () (hs-minor-mode)))

;; https://coderwall.com/p/u-l0ra/ruby-code-folding-in-emacs
(eval-after-load "hideshow"
  '(add-to-list 'hs-special-modes-alist
    `(ruby-mode
      ,(rx (or "def" "class" "module" "do" "{" "[")) ; Block start
      ,(rx (or "}" "]" "end"))                       ; Block end
      ,(rx (or "#" "=begin"))                        ; Comment start
      ruby-forward-sexp nil)))

(global-set-key (kbd "C-c h") 'hs-hide-block)
(global-set-key (kbd "C-c s") 'hs-show-block)


;; TODO: change the mode line to show files' paths (inside and outside of projectile projects), and show if that file has been saved
(defun show-file-name ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (message (buffer-file-name)))

;; projectile
(require 'projectile)
(projectile-global-mode)
(counsel-projectile-mode)

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
;; TODO:
;; You appear to be setting environment variables ("PATH") in your .bashrc or .zshrc: those files are only read by interactive shells, so you should instead set environment variables in startup files like .profile, .bash_profile or .zshenv.  Refer to your shell’s man page for more info.  Customize ‘exec-path-from-shell-arguments’ to remove "-i" when done, or disable ‘exec-path-from-shell-check-startup-files’ to disable this message.
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
;; irony-mode for c/c++ ? (needs company-mode)
;; ruby
;; no-easy-keys
;; better python virtualenv support?
;; sessions/desktop
;; magit
;; which-key
;; elscreen
;; dumb jump
