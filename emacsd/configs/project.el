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

(project-def "sbn"
             '((basedir          "/Users/dzhou/Vox/sbn")
               (src-patterns     ("*.html" "*.rb" "*.js" "*.sass" "*.erb" "*.txt" "*.sh" "*.rxml" "*.scss"))
               (ignore-patterns  ("*.jpg" "*.gif" "*.png" "*.pyc" "~*" "#*" "\.*" "*.scssc"))
               (ignore-path-patterns ("*/.bundle/*" "*/log/*" "*/tmp/*" "*/public/cache/*"))
               (file-list-cache  "/Users/dzhou/.emacs.d/projects/vox/file-list-cache")
               (startup-hook     nil)
               (vcs              git)
               (shutdown-hook    nil)))

(project-def "anthem"
             '((basedir          "/Users/dzhou/Vox/anthem")
               (src-patterns     ("*.html" "*.rb" "*.js" "*.sass" "*.erb" "*.txt" "*.sh" "*.rxml" "*.scss"))
               (ignore-patterns  ("*.jpg" "*.gif" "*.png" "*.pyc" "~*" "#*" "\.*" "*.scssc"))
               (ignore-path-patterns ("*/.bundle/*" "*/log/*" "*/tmp/*" "*/public/cache/*"))
               (file-list-cache  "/Users/dzhou/.emacs.d/projects/anthem/file-list-cache")
               (startup-hook     nil)
               (vcs              git)
               (shutdown-hook    nil)))

(project-def "hermano"
             '((basedir          "/Users/dzhou/Vox/hermano")
               (src-patterns     ("*.html" "*.rb" "*.js" "*.sass" "*.erb" "*.txt" "*.sh" "*.rxml"))
               (ignore-patterns  ("*.jpg" "*.gif" "*.png" "*.pyc" "~*" "#*"))
               (file-list-cache  "/Users/dzhou/.emacs.d/projects/hermano/file-list-cache")
               (startup-hook     nil)
               (vcs              git)
               (shutdown-hook    nil)))

(project-def "beacon"
             '((basedir          "/Users/dzhou/Vox/beacon")
               (src-patterns     ("*.html" "*.rb" "*.js" "*.sass" "*.erb" "*.txt" "*.sh" "*.rxml"))
               (ignore-patterns  ("*.jpg" "*.gif" "*.png" "*.pyc" "~*" "#*"))
               (file-list-cache  "/Users/dzhou/.emacs.d/projects/beacon/file-list-cache")
               (startup-hook     nil)
               (vcs              git)
               (shutdown-hook    nil)))

(project-def "vox-dfp"
             '((basedir          "/Users/dzhou/Vox/vox-dfp")
               (src-patterns     ("*.html" "*.rb" "*.js" "*.sass" "*.erb" "*.txt" "*.sh" "*.rxml"))
               (ignore-patterns  ("*.jpg" "*.gif" "*.png" "*.pyc" "~*" "#*"))
               (file-list-cache  "/Users/dzhou/.emacs.d/projects/vox-dfp/file-list-cache")
               (startup-hook     nil)
               (vcs              git)
               (shutdown-hook    nil)))


(project-def "insights"
             '((basedir          "/Users/dzhou/Vox/chorus-insights-platform")
               (src-patterns     ("*.html" "*.rb" "*.js" "*.sass" "*.erb" "*.txt" "*.sh" "*.rxml"))
               (ignore-patterns  ("*.jpg" "*.gif" "*.png" "*.pyc" "~*" "#*"))
               (file-list-cache  "/Users/dzhou/.emacs.d/projects/chorus-insights-platform/file-list-cache")
               (startup-hook     nil)
               (vcs              git)
               (shutdown-hook    nil)))

(project-def "tools"
             '((basedir          "/Users/dzhou/Vox/voxmedia-editorial-tools")
               (src-patterns     ("*.html" "*.rb" "*.js" "*.sass" "*.erb" "*.txt" "*.sh" "*.rxml"))
               (ignore-patterns  ("*.jpg" "*.gif" "*.png" "*.pyc" "~*" "#*"))
               (file-list-cache  "/Users/dzhou/.emacs.d/projects/tools/file-list-cache")
               (startup-hook     nil)
               (vcs              git)
               (shutdown-hook    nil)))

(project-def "extant"
             '((basedir          "/Users/dzhou/Code/extant")
               (src-patterns     ("*.html" "*.rb" "*.js" "*.sass" "*.erb" "*.txt" "*.sh" "*.rxml"))
               (ignore-patterns  ("*.jpg" "*.gif" "*.png" "*.pyc" "~*" "#*"))
               (file-list-cache  "/Users/dzhou/.emacs.d/projects/extant/file-list-cache")
               (startup-hook     nil)
               (vcs              git)
               (shutdown-hook    nil)))

(project-def "cantrip"
             '((basedir          "/Users/dzhou/Code/cantrip")
               (src-patterns     ("*.html" "*.rb" "*.js" "*.sass" "*.erb" "*.txt" "*.sh" "*.rxml"))
               (ignore-patterns  ("*.jpg" "*.gif" "*.png" "*.pyc" "~*" "#*"))
               (file-list-cache  "/Users/dzhou/.emacs.d/projects/cantrip/file-list-cache")
               (startup-hook     nil)
               (vcs              git)
               (shutdown-hook    nil)))

(project-def "goof"
             '((basedir          "/Users/dzhou/Code/go/src/github.com/dz/goof")
               (src-patterns     ("*.html" "*.rb" "*.js" "*.sass" "*.erb" "*.txt" "*.sh" "*.rxml"))
               (ignore-patterns  ("*.jpg" "*.gif" "*.png" "*.pyc" "~*" "#*"))
               (file-list-cache  "/Users/dzhou/.emacs.d/projects/goof/file-list-cache")
               (startup-hook     nil)
               (vcs              git)
               (shutdown-hook    nil)))

(project-def "legato"
             '((basedir          "/Users/dzhou/Vox/legato")
               (src-patterns     ("*.html" "*.rb" "*.js" "*.sass" "*.erb" "*.txt" "*.sh" "*.rxml"))
               (ignore-patterns  ("*.jpg" "*.gif" "*.png" "*.pyc" "~*" "#*"))
               (file-list-cache  "/Users/dzhou/.emacs.d/projects/legato/file-list-cache")
               (startup-hook     nil)
               (vcs              git)
               (shutdown-hook    nil)))

(project-def "presto"
             '((basedir          "/Users/dzhou/Vox/presto")
               (src-patterns     ("*.html" "*.rb" "*.js" "*.sass" "*.erb" "*.txt" "*.sh" "*.rxml" "*.es6"))
               (ignore-patterns  ("*.jpg" "*.gif" "*.png" "*.pyc" "~*" "#*"))
               (file-list-cache  "/Users/dzhou/.emacs.d/projects/presto/file-list-cache")
               (startup-hook     nil)
               (vcs              git)
               (shutdown-hook    nil)))

(project-def "pr-notifier"
             '((basedir          "/Users/dzhou/Vox/pr_notifier")
               (src-patterns     ("*.html" "*.rb" "*.js" "*.sass" "*.erb" "*.txt" "*.sh" "*.rxml" "*.jsx"))
               (ignore-patterns  ("*.jpg" "*.gif" "*.png" "*.pyc" "~*" "#*"))
               (file-list-cache  "/Users/dzhou/.emacs.d/projects/pr_notifier/file-list-cache")
               (startup-hook     nil)
               (vcs              git)
               (shutdown-hook    nil)))

(project-def "notebot"
             '((basedir          "/Users/dzhou/Vox/notebot")
               (src-patterns     ("*.html" "*.rb" "*.js" "*.sass" "*.erb" "*.txt" "*.sh" "*.rxml" "*.jsx"))
               (ignore-patterns  ("*.jpg" "*.gif" "*.png" "*.pyc" "~*" "#*"))
               (file-list-cache  "/Users/dzhou/.emacs.d/projects/notebot/file-list-cache")
               (startup-hook     nil)
               (vcs              git)
               (shutdown-hook    nil)))

(project-def "history"
             '((basedir          "/Users/dzhou/Vox/history")
               (src-patterns     ("*.html" "*.rb" "*.js" "*.sass" "*.erb" "*.txt" "*.sh" "*.rxml" "*.jsx"))
               (ignore-patterns  ("*.jpg" "*.gif" "*.png" "*.pyc" "~*" "#*"))
               (file-list-cache  "/Users/dzhou/.emacs.d/projects/history/file-list-cache")
               (startup-hook     nil)
               (vcs              git)
               (shutdown-hook    nil)))

(project-def "billboards-web"
             '((basedir          "/Users/dzhou/Vox/billboards-web")
               (src-patterns     ("*.html" "*.rb" "*.js" "*.sass" "*.erb" "*.txt" "*.sh" "*.rxml" "*.jsx"))
               (ignore-patterns  ("*.jpg" "*.gif" "*.png" "*.pyc" "~*" "#*"))
               (file-list-cache  "/Users/dzhou/.emacs.d/projects/billboards-web/file-list-cache")
               (startup-hook     nil)
               (vcs              git)
               (shutdown-hook    nil)))

(project-def "fastly"
             '((basedir          "/Users/dzhou/Vox/fastly-vcls")
               (src-patterns     ("*.html" "*.rb" "*.js" "*.sass" "*.erb" "*.txt" "*.sh" "*.rxml" "*.jsx"))
               (ignore-patterns  ("*.jpg" "*.gif" "*.png" "*.pyc" "~*" "#*"))
               (file-list-cache  "/Users/dzhou/.emacs.d/projects/fastly-vcls/file-list-cache")
               (startup-hook     nil)
               (vcs              git)
               (shutdown-hook    nil)))

(project-def "vax-syllabus"
             '((basedir          "/Users/dzhou/Vox/vax2016-syllabus")
               (src-patterns     ("*.html" "*.rb" "*.js" "*.sass" "*.erb" "*.txt" "*.sh" "*.rxml" "*.jsx"))
               (ignore-patterns  ("*.jpg" "*.gif" "*.png" "*.pyc" "~*" "#*"))
               (file-list-cache  "/Users/dzhou/.emacs.d/projects/vax2016-syllabus/file-list-cache")
               (startup-hook     nil)
               (vcs              git)
               (shutdown-hook    nil)))

(project-def "obscura"
             '((basedir          "/Users/dzhou/Vox/obscura")
               (src-patterns     ("*.html" "*.rb" "*.js" "*.sass" "*.erb" "*.txt" "*.sh" "*.rxml" "*.jsx" "*.esx" "*.eex"))
               (ignore-patterns  ("*.jpg" "*.gif" "*.png" "*.pyc" "~*" "#*"))
               (file-list-cache  "/Users/dzhou/.emacs.d/projects/obscura/file-list-cache")
               (startup-hook     nil)
               (vcs              git)
               (shutdown-hook    nil)))

(project-def "campfirebot"
             '((basedir          "/Users/dzhou/Vox/campfirebot")
               (src-patterns     ("*.html" "*.rb" "*.js" "*.sass" "*.erb" "*.txt" "*.sh" "*.rxml" "*.jsx"))
               (ignore-patterns  ("*.jpg" "*.gif" "*.png" "*.pyc" "~*" "#*"))
               (file-list-cache  "/Users/dzhou/.emacs.d/projects/campfirebot/file-list-cache")
               (startup-hook     nil)
               (vcs              git)
               (shutdown-hook    nil)))

(project-def "campaigns"
             '((basedir          "/Users/dzhou/Documents/RPG/Campaigns")
               (src-patterns     ("*.html" "*.rb" "*.js" "*.sass" "*.erb" "*.txt" "*.sh" "*.rxml" "*.jsx"))
               (ignore-patterns  ("*.jpg" "*.gif" "*.png" "*.pyc" "~*" "#*"))
               (file-list-cache  "/Users/dzhou/.emacs.d/projects/campaigns/file-list-cache")
               (startup-hook     nil)
               (vcs              git)
               (shutdown-hook    nil)))

(project-def "sousvid"
             '((basedir          "/Users/dzhou/Vox/sousvid")
               (src-patterns     ("*.html" "*.rb" "*.js" "*.sass" "*.erb" "*.txt" "*.sh" "*.rxml" "*.jsx" "*.esx" "*.eex"))
               (ignore-patterns  ("*.jpg" "*.gif" "*.png" "*.pyc" "~*" "#*"))
               (file-list-cache  "/Users/dzhou/.emacs.d/projects/sousvid/file-list-cache")
               (startup-hook     nil)
               (vcs              git)
               (shutdown-hook    nil)))


(project-def "hymnal"
             '((basedir          "/Users/dzhou/Vox/hymnal")
               (src-patterns     ("*.html" "*.rb" "*.js" "*.sass" "*.erb" "*.txt" "*.sh" "*.rxml" "*.jsx" "*.esx" "*.eex"))
               (ignore-patterns  ("*.jpg" "*.gif" "*.png" "*.pyc" "~*" "#*"))
               (file-list-cache  "/Users/dzhou/.emacs.d/projects/hymnal/file-list-cache")
               (startup-hook     nil)
               (vcs              git)
               (shutdown-hook    nil)))

(project-def "bulkcloset"
             '((basedir          "/Users/dzhou/Code/bulkcloset")
               (src-patterns     ("*.html" "*.rb" "*.js" "*.sass" "*.erb" "*.txt" "*.sh" "*.rxml" "*.jsx" "*.esx" "*.eex"))
               (ignore-patterns  ("*.jpg" "*.gif" "*.png" "*.pyc" "~*" "#*"))
               (file-list-cache  "/Users/dzhou/.emacs.d/projects/bulkcloset/file-list-cache")
               (startup-hook     nil)
               (vcs              git)
               (shutdown-hook    nil)))

(project-def "anw"
             '((basedir          "/Users/dzhou/Vox/sbnation-anw-contest")
               (src-patterns     ("*.html" "*.rb" "*.js" "*.sass" "*.erb" "*.txt" "*.sh" "*.rxml" "*.jsx" "*.esx" "*.eex"))
               (ignore-patterns  ("*.jpg" "*.gif" "*.png" "*.pyc" "~*" "#*"))
               (file-list-cache  "/Users/dzhou/.emacs.d/projects/anw/file-list-cache")
               (startup-hook     nil)
               (vcs              git)
               (shutdown-hook    nil)))

(project-def "chorus-ads"
             '((basedir          "/Users/dzhou/Vox/chorus-ads")
               (src-patterns     ("*.html" "*.rb" "*.js" "*.sass" "*.erb" "*.txt" "*.sh" "*.rxml" "*.jsx" "*.esx" "*.eex"))
               (ignore-patterns  ("*.jpg" "*.gif" "*.png" "*.pyc" "~*" "#*"))
               (file-list-cache  "/Users/dzhou/.emacs.d/projects/chorus-ads/file-list-cache")
               (startup-hook     nil)
               (vcs              git)
               (shutdown-hook    nil)))

(project-def "vax"
             '((basedir          "/Users/dzhou/Vox/vax")
               (src-patterns     ("*.html" "*.rb" "*.js" "*.sass" "*.erb" "*.txt" "*.sh" "*.rxml" "*.jsx" "*.esx" "*.eex"))
               (ignore-patterns  ("*.jpg" "*.gif" "*.png" "*.pyc" "~*" "#*"))
               (file-list-cache  "/Users/dzhou/.emacs.d/projects/vax/file-list-cache")
               (startup-hook     nil)
               (vcs              git)
               (shutdown-hook    nil)))

(project-def "concert-ads"
             '((basedir          "/Users/dzhou/Vox/concert-ads")
               (src-patterns     ("*.html" "*.rb" "*.js" "*.sass" "*.erb" "*.txt" "*.sh" "*.rxml" "*.jsx" "*.esx" "*.eex"))
               (ignore-patterns  ("*.jpg" "*.gif" "*.png" "*.pyc" "~*" "#*"))
               (file-list-cache  "/Users/dzhou/.emacs.d/projects/concert-ads/file-list-cache")
               (startup-hook     nil)
               (vcs              git)
               (shutdown-hook    nil)))
