;;------------------------------------------------------------------------------
;; Packages

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/") ))

(require 'package)
(setq package-enable-at-startup nil)
(package-initialize)
(package-refresh-contents)

(defvar my-packages
  '(solarized-theme
    git-gutter
    smex
    neotree
    find-file-in-project
    auto-highlight-symbol
    web-mode
    yaml-mode
    zygospore))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(global-set-key (kbd "C-x 1") 'zygospore-toggle-delete-other-windows)

(setq ahs-idle-interval 0.2)
(global-auto-highlight-symbol-mode)

(global-git-gutter-mode +1)
(git-gutter:linum-setup)
(custom-set-variables '(git-gutter:update-interval 2))

(global-set-key [f8] 'neotree-toggle)

(load-theme 'solarized-dark t)

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\.yml$" . yaml-mode))

(require 'smex)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-x C-M") 'smex-major-mode-commands)

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . web-mode))

;;------------------------------------------------------------------------------
;; UI

(setq create-lockfiles nil) ;; No need for ~ files when editing
(setq inhibit-startup-message t) ;; Go straight to scratch buffer on startup
(menu-bar-mode -1) ;; Turn off menu bar at top of frame
(global-linum-mode) ;; Show line numbers
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1)) ;; Turn off toolbar at top
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1)) ;; Turn off OS scrollbars
(setq-default frame-title-format "%b (%f)") ;; Show full path in frame title
(global-hl-line-mode 1) ;; Highlight current line
(fset 'yes-or-no-p 'y-or-n-p)
(show-paren-mode 1)
(setq show-paren-delay 0)

(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (concat user-emacs-directory "places"))

(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))

(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; C-; to toggle comments
(defun toggle-comment-on-line ()
  "comment or uncomment current line"
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))
(global-set-key (kbd "C-;") 'toggle-comment-on-line)

(require 'whitespace)
(setq-default show-trailing-whitespace t)
(defun toggle-show-trailing-whitespace ()
  "Toggle show-trailing-whitespace"
  (interactive)
  (setq show-trailing-whitespace (not show-trailing-whitespace)))

(global-auto-revert-mode t) ;; Refresh buffers when files change on disk
(desktop-save-mode t) ;; Save Emacs state between sessions

;;------------------------------------------------------------------------------
;; Navigation

(ido-mode t)
(setq ido-enable-flex-matching t) ;; Allow partial matches, e.g. "tl" will match "Tyrion Lannister"
(setq ido-use-virtual-buffers t) ;; Includes buffer names of recently open files, even if they're not open

;; "When several buffers visit identically-named files,
;; Emacs must give the buffers distinct names. The usual method
;; for making buffer names unique adds ‘<2>’, ‘<3>’, etc. to the end
;; of the buffer names (all but one of them).
;; The forward naming method includes part of the file's directory
;; name at the beginning of the buffer name
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Uniquify.html
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;;------------------------------------------------------------------------------
;; Custom functions

;; This is the greatest and best function ever.
(defun reload ()
  "Reloads the .emacs file"
  (interactive)
  (load-file "~/.emacs.d/init.el") )

