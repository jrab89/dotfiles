(require 'ido)

(ido-mode t)
(setq ido-enable-flex-matching t) ;; Allow partial matches, e.g. "tl" will match "Tyrion Lannister"
(setq ido-use-virtual-buffers t) ;; Includes buffer names of recently open files, even if they're not open
