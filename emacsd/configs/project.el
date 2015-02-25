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
(global-set-key "\M-t" 'project-find-file-ido)
(global-set-key "\M-O" 'project-load)

;; project global config
(setq mk-proj-use-ido-selection t)

;; helper functions
(defun sbn-find-cmd (context)
  (let* ((ignore-clause  (concat "\\( -path " mk-proj-basedir ".?* -prune \\)"))
         (src-clause     "\\( -type f \\( -name '*.cpp' -o -name '*.[cChH]' -o -name '*.java' \\) -print \\)"))
    (ecase context
      ('src   (concat "find " mk-proj-basedir " " ignore-clause " -o " src-clause))
      ('grep  (replace-regexp-in-string "-print" "-print0" (concat "find . " src-clause) t))
      ('index (concat "find " mk-proj-basedir " " ignore-clause " -o -print")))))

;; project definitions
(project-def "sbn"
      '((basedir          "/Users/dzhou/Vox/sbn")
        (src-patterns     ("*.html" "*.rb" "*.js" "*.sass" "*.erb" "*.txt" "*.sh" "*.rxml" "*.scss"))
        (ignore-patterns  ("*.jpg" "*.gif" "*.png" "*.pyc" "~*" "#*" "\.*" "*.scssc"))
        (ignore-path-patterns ("*/.bundle/*" "*/log/*" "*/tmp/*" "*/public/cache/*"))
        (file-list-cache  "/Users/dzhou/.emacs.d/projects/vox/file-list-cache")
        ;; (open-files-cache "/Users/dzhou/.emacs.d/projects/vox/open-files-cache")
        (startup-hook     nil)
        (vcs              git)
        (shutdown-hook    nil)))

(project-def "hermano"
      '((basedir          "/Users/dzhou/Vox/hermano")
        (src-patterns     ("*.html" "*.rb" "*.js" "*.sass" "*.erb" "*.txt" "*.sh" "*.rxml"))
        (ignore-patterns  ("*.jpg" "*.gif" "*.png" "*.pyc" "~*" "#*"))
        (file-list-cache  "/Users/dzhou/.emacs.d/projects/hermano/file-list-cache")
        ;; (open-files-cache "/Users/dzhou/.emacs.d/projects/hermano/open-files-cache")
        (startup-hook     nil)
        (vcs              git)
        (shutdown-hook    nil)))

(project-def "beacon"
      '((basedir          "/Users/dzhou/Vox/beacon")
        (src-patterns     ("*.html" "*.rb" "*.js" "*.sass" "*.erb" "*.txt" "*.sh" "*.rxml"))
        (ignore-patterns  ("*.jpg" "*.gif" "*.png" "*.pyc" "~*" "#*"))
        (file-list-cache  "/Users/dzhou/.emacs.d/projects/beacon/file-list-cache")
        ;; (open-files-cache "/Users/dzhou/.emacs.d/projects/hermano/open-files-cache")
        (startup-hook     nil)
        (vcs              git)
        (shutdown-hook    nil)))

(project-def "vox-dfp"
      '((basedir          "/Users/dzhou/Vox/vox-dfp")
        (src-patterns     ("*.html" "*.rb" "*.js" "*.sass" "*.erb" "*.txt" "*.sh" "*.rxml"))
        (ignore-patterns  ("*.jpg" "*.gif" "*.png" "*.pyc" "~*" "#*"))
        (file-list-cache  "/Users/dzhou/.emacs.d/projects/vox-dfp/file-list-cache")
        ;; (open-files-cache "/Users/dzhou/.emacs.d/projects/hermano/open-files-cache")
        (startup-hook     nil)
        (vcs              git)
        (shutdown-hook    nil)))
