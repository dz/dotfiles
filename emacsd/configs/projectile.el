(require 'projectile)
(require 'helm-projectile)

(projectile-mode)
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)

(setq projectile-use-git-grep t)

(setq projectile-mode-line
      '(:eval (if (projectile-project-p)
                  (format "[%s]"
                          (projectile-project-name))
                "")))

(global-set-key "\M-O" 'projectile-switch-project)
(global-set-key "\M-e" 'helm-ls-git-ls)
(global-set-key "\M-p" 'helm-ls-git-ls)
