(require 'git-gutter)

(global-git-gutter-mode)
(git-gutter:linum-setup)
(custom-set-variables '(git-gutter:update-interval 2))
