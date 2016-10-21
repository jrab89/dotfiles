(setq package-archives
  '(("gnu" . "https://elpa.gnu.org/packages/")
    ("marmalade" . "https://marmalade-repo.org/packages/")
    ("melpa" . "https://melpa.org/packages/")))

(require 'package)
(setq package-enable-at-startup nil)
(package-initialize)
(package-refresh-contents)

(defvar my-packages
  '(ac-inf-ruby
    auto-complete
    auto-highlight-symbol
    buffer-move
    cider
    clojure-mode
    company
    direx
    elpy
    exec-path-from-shell
    feature-mode
    find-file-in-project
    flycheck
    flycheck-pos-tip
    git-gutter
    go-autocomplete
    go-eldoc
    go-mode
    haml-mode
    inf-ruby
    markdown-mode
    puppet-mode
    smex
    solarized-theme
    ssh-config-mode
    terraform-mode
    web-mode
    windresize
    yaml-mode
    zygospore))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(add-to-list 'load-path "~/.emacs.d/packages")
(dolist (file-name (directory-files "~/.emacs.d/packages" nil "\\.el$"))
  (load file-name))

(add-to-list 'load-path "~/.emacs.d/vendor")
(dolist (file-name (directory-files "~/.emacs.d/vendor" nil "\\.el$"))
  (load file-name))

(load-theme 'solarized-dark t)
;; (load-theme 'solarized-light t)

(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(require 'ibuffer)
(setq ibuffer-default-sorting-mode 'filename/process)

;;; UI
(setq inhibit-startup-message t) ;; Go straight to scratch buffer on startup
(menu-bar-mode -1) ;; Turn off menu bar at top of frame
(global-linum-mode) ;; Show line numbers
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1)) ;; Turn off toolbar at top
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1)) ;; Turn off OS scrollbars
(setq-default frame-title-format "%b (%f)") ;; Show full path in frame title
(global-hl-line-mode 1) ;; Highlight current line
(fset 'yes-or-no-p 'y-or-n-p)
(show-paren-mode 1)
(setq make-backup-files nil)
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

(setq-default show-trailing-whitespace t)
(defun toggle-show-trailing-whitespace ()
  "Toggle show-trailing-whitespace"
  (interactive)
  (setq show-trailing-whitespace (not show-trailing-whitespace)))

(add-hook 'help-mode-hook
  (function (lambda ()
    (setq show-trailing-whitespace nil))))

;;; Editing
(setq create-lockfiles nil) ;; No need for ~ files when editing
(setq tab-width 4)
(setq-default indent-tabs-mode nil)
(global-auto-revert-mode t) ;; Refresh buffers when files change on disk
(desktop-save-mode t) ;; Save Emacs state between sessions

;; enable some disabled commands: http://stackoverflow.com/q/10026221
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(defun toggle-comment-on-line ()
  "comment or uncomment current line"
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))
(global-set-key (kbd "C-;") 'toggle-comment-on-line)

(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))

;; When several buffers visit identically-named files,
;; Emacs must give the buffers distinct names. The usual method
;; for making buffer names unique adds ‘<2>’, ‘<3>’, etc. to the end
;; of the buffer names (all but one of them).
;; The forward naming method includes part of the file's directory
;; name at the beginning of the buffer name
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Uniquify.html
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(require 'emacs-pager)
(add-to-list 'auto-mode-alist '("\\.emacs-pager$" . emacs-pager-mode))
(setq emacs-pager-max-line-coloring 5000)

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

;;; things I should look at:
;; magit
;; helm/projectile
;; nyan cat mode
;; beacon-mode
;; restclient
;; skewer (js repl)
;; expand region
;; org mode
