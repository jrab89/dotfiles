(require 'package)

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")))

(defvar my-packages '(ag
                      better-defaults
                      counsel
                      counsel-projectile
                      dockerfile-mode
                      exec-path-from-shell
                      git-gutter-fringe
                      haml-mode
                      idle-highlight-mode
                      ivy
                      json-mode
                      markdown-mode
                      projectile
                      salt-mode
                      swiper
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

(load-theme 'zenburn t)

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(require 'git-gutter-fringe)
(global-git-gutter-mode +1)

(require 'better-defaults)

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

(require 'counsel)
(require 'swiper)
(require 'ivy)

(ivy-mode)
(counsel-mode)

(require 'projectile)
(projectile-global-mode)

(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(setq ivy-use-selectable-prompt t)
(setq projectile-completion-system 'ivy)
(global-set-key "\C-s" 'swiper)
;; only using `counsel-projectile-ag` from the counsel-projectile package
(define-key projectile-mode-map [remap projectile-ag] 'counsel-projectile-ag)

;; Set projectile project name as frame title
;; https://emacs.stackexchange.com/a/35443
(setq frame-title-format
      '(""
        "%b"
        (:eval
         (let ((project-name (projectile-project-name)))
           (unless (string= "-" project-name)
             (format " in [%s]" project-name))))))

;; TODO: change the mode line to show files' paths (inside and outside of projectile projects), and show if that file has been saved
(defun show-file-name ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (message (buffer-file-name)))

(setq show-paren-style 'parenthesis)
(setq show-paren-delay 0)

;; Highlight current line
(global-hl-line-mode t)

(windmove-default-keybindings)

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

;; zygospore
(require 'zygospore)
(global-set-key (kbd "C-x 1") 'zygospore-toggle-delete-other-windows)

(require 'idle-highlight-mode)
(set-face-attribute 'idle-highlight nil :inherit 'show-paren-match)
(setq idle-highlight-idle-time 0.1)
(setq idle-highlight-exceptions '("do" "end"))

(add-hook 'prog-mode-hook
          (lambda ()
            (linum-mode)
            (idle-highlight-mode t)
            (setq show-trailing-whitespace t)))

;; http://stackoverflow.com/a/36344479
(with-eval-after-load "term"
  (define-key term-raw-map (kbd "M-x") 'counsel-M-x)
  (define-key term-raw-map (kbd "C-y") 'term-paste)
  (define-key term-raw-map (kbd "C-x") nil)
  (define-key term-raw-map (kbd "s-v") 'term-paste))

;; ibuffer
(setq ibuffer-default-sorting-mode 'filename/process)

;; makes the big dumb warning sign go away in emacs 25
(setq visible-bell nil)

;; allow these without asking
(put 'erase-buffer 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; TODO
;; better python virtualenv support?
;; company
;; dumb jump
;; flycheck
;; go
;; irony-mode for c/c++ ? (needs company-mode)
;; magit
;; no-easy-keys
;; rainbow-delimiters
;; ruby
;; which-key
