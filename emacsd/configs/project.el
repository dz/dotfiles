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
(global-set-key "\M-O" 'project-load)

;; helper functions
(defun sbn-find-cmd (context)
  (let* ((ignore-clause  (concat "\\( -path " mk-proj-basedir ".?* -prune \\)"))
         (src-clause     "\\( -type f \\( -name '*.cpp' -o -name '*.[cChH]' -o -name '*.java' \\) -print \\)"))
    (ecase context
      ('src   (concat "find " mk-proj-basedir " " ignore-clause " -o " src-clause))
      ('grep  (replace-regexp-in-string "-print" "-print0" (concat "find . " src-clause) t))
      ('index (concat "find " mk-proj-basedir " " ignore-clause " -o -print")))))

;; project definitions
(project-def "emacs"
             '((basedir          "/Users/dzhou/.emacs.d")
               (src-patterns     ("*.el"))
               (ignore-patterns  ("*.jpg" "*.gif" "*.png" "*.pyc" "~*" "#*" "\.*" "*.scssc"))
               (ignore-path-patterns ("*/elpa/*" "*/vendor/*"))
               (file-list-cache  "/Users/dzhou/.emacs.d/projects/emacs/file-list-cache")
               (startup-hook     nil)
               (vcs              git)
               (shutdown-hook    nil)))

(project-def "nodnod"
             '((basedir          "/Users/dzhou/Code/nodnod")
               (src-patterns     ("*.html" "*.php" "*.js" "*.sass" "*.less" "*.txt" "*.sh" "*.rxml" "*.scss"))
               (ignore-patterns  ("*.jpg" "*.gif" "*.png" "*.pyc" "~*" "#*" "\.*" "*.scssc"))
               (ignore-path-patterns ("*/.bundle/*" "*/log/*" "*/tmp/*" "*/public/cache/*"))
               (file-list-cache  "/Users/dzhou/.emacs.d/projects/nodnod/file-list-cache")
               (startup-hook     nil)
               (vcs              git)
               (shutdown-hook    nil)))


(project-def "shopify-core"
             '((basedir          "/Users/dzhou/Code/shopify/shopify")
               (src-patterns     ("*.html" "*.rb" "*.js" "*.sass" "*.less" "*.txt" "*.sh" "*.rxml" "*.scss"))
               (ignore-patterns  ("*.jpg" "*.gif" "*.png" "*.pyc" "~*" "#*" "\.*" "*.scssc"))
               (ignore-path-patterns ("*/.bundle/*" "*/log/*" "*/tmp/*" "*/public/cache/*"))
               (file-list-cache  "/Users/dzhou/.emacs.d/projects/shopify/file-list-cache")
               (startup-hook     nil)
               (vcs              git)
               (shutdown-hook    nil)))

(project-def "shopify-web"
             '((basedir          "/Users/dzhou/Code/shopify/web")
               (src-patterns     ("*.html" "*.rb" "*.js" "*.sass" "*.less" "*.txt" "*.sh" "*.rxml" "*.scss"))
               (ignore-patterns  ("*.jpg" "*.gif" "*.png" "*.pyc" "~*" "#*" "\.*" "*.scssc"))
               (ignore-path-patterns ("*/.bundle/*" "*/log/*" "*/tmp/*" "*/public/cache/*"))
               (file-list-cache  "/Users/dzhou/.emacs.d/projects/shopify/file-list-cache")
               (startup-hook     nil)
               (vcs              git)
               (shutdown-hook    nil)))

(project-def "self-serve"
             '((basedir          "/Users/dzhou/Code/shopify/self-serve")
               (src-patterns     ("*.html" "*.rb" "*.js" "*.sass" "*.less" "*.txt" "*.sh" "*.rxml" "*.scss"))
               (ignore-patterns  ("*.jpg" "*.gif" "*.png" "*.pyc" "~*" "#*" "\.*" "*.scssc"))
               (ignore-path-patterns ("*/.bundle/*" "*/log/*" "*/tmp/*" "*/public/cache/*"))
               (file-list-cache  "/Users/dzhou/.emacs.d/projects/shopify/file-list-cache")
               (startup-hook     nil)
               (vcs              git)
               (shutdown-hook    nil)))
