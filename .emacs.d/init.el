(require 'package)

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")))

(defvar my-packages '(better-defaults
                      counsel-projectile
                      dockerfile-mode
                      exec-path-from-shell
                      git-gutter
                      haml-mode
                      idle-highlight-mode
                      ivy
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

(load-theme 'zenburn t)

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

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
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-h f") 'counsel-describe-function)
(global-set-key (kbd "C-h v") 'counsel-describe-variable)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)

;; TODO: change the mode line to show files' paths (inside and outside of projectile projects), and show if that file has been saved
(defun show-file-name ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (message (buffer-file-name)))

;; projectile
(require 'projectile)
(projectile-global-mode)
(counsel-projectile-mode)

(setq show-paren-style 'parenthesis)
(setq show-paren-delay 0)

;; Highlight current line
(global-hl-line-mode t)

(windmove-default-keybindings)

(defun toggle-show-trailing-whitespace ()
  "Toggle show-trailing-whitespace"
  (interactive)
  (setq show-trailing-whitespace (not show-trailing-whitespace)))

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

;; git-gutter
(global-git-gutter-mode t)
(git-gutter:linum-setup)

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
;; irony-mode for c/c++ ? (needs company-mode)
;; ruby
;; rainbow-delimiters
;; no-easy-keys
;; better python virtualenv support?
;; magit
;; which-key
;; elscreen or projectile perspective?
;; LSP stuff
;; dumb jump
