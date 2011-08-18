;; project level settings

(require 'mk-project)
(global-set-key (kbd "C-c p c") 'project-compile)
(global-set-key (kbd "C-c p l") 'project-load)
(global-set-key (kbd "C-c p a") 'project-ack)
(global-set-key (kbd "C-c p g") 'project-grep)
(global-set-key (kbd "C-c p o") 'project-multi-occur)
(global-set-key (kbd "C-c p u") 'project-unload)
(global-set-key (kbd "C-c p f") 'project-find-file-ido) ; or project-find-file-ido
(global-set-key (kbd "C-c p i") 'project-index)
(global-set-key (kbd "C-c p s") 'project-status)
(global-set-key (kbd "C-c p h") 'project-home)
(global-set-key (kbd "C-c p d") 'project-dired)
(global-set-key (kbd "C-c p t") 'project-tags)

;; mac keys

(global-set-key "\M-e" 'project-find-file-ido)
(global-set-key "\M-t" 'project-find-file-ido)
(global-set-key "\M-F" 'project-grep)
(global-set-key "\M-O" 'project-load)

;; project definitions

(project-def "disqus"
      '((basedir          "/Users/dzhou/Disqus/repo/")
        (src-patterns     ("*.html" "*.py" "*.js" "*.css" "*.tmpl" "*.txt" "*.sh"))
        (ignore-patterns  ("*.jpg" "*.gif" "*.png" "*.pyc" "~*" "#*"))
        (file-list-cache  "/Users/dzhou/.emacs.d/projects/disqus/file-list-cache")
        (open-files-cache "/Users/dzhou/.emacs.d/projects/disqus/open-files-cache")
        (vcs              git)
        (shutdown-hook    nil)))
