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
(global-set-key "\M-F" 'project-find-file-ido)
(global-set-key "\M-O" 'project-load)

;; project definitions

(project-def "vox"
      '((basedir          "/Users/dzhou/Vox/sbn")
        (src-patterns     ("*.html" "*.rb" "*.js" "*.sass" "*.erb" "*.txt" "*.sh" "*.rxml"))
        (ignore-patterns  ("*.jpg" "*.gif" "*.png" "*.pyc" "~*" "#*"))
        (file-list-cache  "/Users/dzhou/.emacs.d/projects/vox/file-list-cache")
        (open-files-cache "/Users/dzhou/.emacs.d/projects/vox/open-files-cache")
        (startup-hook     vox-startup)
        (vcs              git)
        (shutdown-hook    nil)))

(defun vox-startup ()
  (rvm-activate-corresponding-ruby))

(project-def "hermano"
      '((basedir          "/Users/dzhou/Vox/hermano")
        (src-patterns     ("*.html" "*.rb" "*.js" "*.sass" "*.erb" "*.txt" "*.sh" "*.rxml"))
        (ignore-patterns  ("*.jpg" "*.gif" "*.png" "*.pyc" "~*" "#*"))
        (file-list-cache  "/Users/dzhou/.emacs.d/projects/hermano/file-list-cache")
        (open-files-cache "/Users/dzhou/.emacs.d/projects/hermano/open-files-cache")
        (startup-hook     hermano-startup)
        (vcs              git)
        (shutdown-hook    nil)))

(defun hermano-startup ()
  (rvm-activate-corresponding-ruby))


(project-def "peacecorps"
      '((basedir          "/Users/dzhou/Documents/Code/peacecorps/")
        (src-patterns     ("*.html" "*.py" "*.js" "*.css" "*.tmpl" "*.txt" "*.sh"))
        (ignore-patterns  ("*.jpg" "*.gif" "*.png" "*.pyc" "~*" "#*"))
        (file-list-cache  "/Users/dzhou/.emacs.d/projects/peacecorps/file-list-cache")
        (open-files-cache "/Users/dzhou/.emacs.d/projects/peacecorps/open-files-cache")
        (vcs              git)
        (shutdown-hook    nil)))
