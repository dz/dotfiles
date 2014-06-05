(require 'projectile)
(projectile-global-mode)

;; always cache file list
(setq projectile-enable-caching t)

;; open a dired on the project after switching to it
(setq projectile-switch-project-action 'projectile-dired)

;; mode line prefix
(setq projectile-mode-line-lighter "Proj")

;; mac keys
(global-set-key "\M-e" 'projectile-find-file)
(global-set-key "\M-E" 'projectile-invalidate-cache)
(global-set-key "\M-t" 'projectile-find-file)
(global-set-key "\M-F" 'projectile-ack)
(global-set-key "\M-O" 'projectile-switch-project)
