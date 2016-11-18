(require 'zygospore)
(require 'neotree)

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
