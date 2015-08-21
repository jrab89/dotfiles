;; Packages
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/") ))

(require 'package)
(setq package-enable-at-startup nil)
(package-initialize)
(package-refresh-contents)

(defvar my-packages
  '(solarized-theme
    smex
    yaml-mode))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(load-theme 'solarized-dark t)

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\.yml$" . yaml-mode))

(require 'smex)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-x C-M") 'smex-major-mode-commands)

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

(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (concat user-emacs-directory "places"))

(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))

;; C-; to toggle comments
(defun toggle-comment-on-line ()
  "comment or uncomment current line"
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))
(global-set-key (kbd "C-;") 'toggle-comment-on-line)

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

