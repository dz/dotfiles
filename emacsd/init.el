;; adjust emac's garbage collection
;; to only collect everything 30MB
(setq gc-cons-threshold 30000000)

;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; No splash screen please ... jeez
(setq inhibit-startup-message t)

;;turn on server mode
(require 'server)
(unless (server-running-p)
  (server-start))

(add-to-list 'load-path "~/.emacs.d/vendor")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

;; set emacs bash path to match shell bash path
(setenv "PATH" (shell-command-to-string "bash -i -c 'echo -n $PATH'"))

;; some packages use mapcan which was removed in emacs 24
(require 'cl)

;; ELPA/MELPA/Marlade package stuff
(load "package")
(package-initialize)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(defvar dz/packages '(
                      popup
                      auto-complete
                      deft
                      expand-region
                      exec-path-from-shell
                      flycheck
                      helm
                      helm-ag
                      helm-flycheck
                      helm-git-grep
                      helm-ls-git
                      helm-swoop
                      popwin
                      visual-regexp
                      visual-regexp-steroids
                      evil-surround
                      lusty-explorer
                      ;; lagnauge modes
                      color-identifiers-mode
                      dockerfile-mode
                      go-mode
                      coffee-mode
                      coffee-mode
                      scss-mode
                      web-mode
                      yaml-mode)
  "Default packages")

(defun dz/packages-installed-p ()
  (loop for pkg in dz/packages
        when (not (package-installed-p pkg)) do (return nil)
        finally (return t)))

(unless (dz/packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg dz/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

;; recursively add subdirs in vendor to load path
;; this might override things intalled via ELPA/MELPA/Marmalade
;; but that is what we want
(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
    (let* ((my-lisp-dir "~/.emacs.d/vendor")
           (default-directory my-lisp-dir))
      (setq load-path (cons my-lisp-dir load-path))
      (normal-top-level-add-subdirs-to-load-path)))

;; os x fix for executng things that depend
;; on shell path
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; autoload config files in the config folder
(defconst emacs-config-dir "~/.emacs.d/configs/" "")
(defun load-cfg-files (filelist)
  (dolist (file filelist)
    (load (expand-file-name
           (concat emacs-config-dir file)))
    (message "Loaded config file: %s" file)
    ))

;; load nxhtml mode
;; (load "~/.emacs.d/vendor/nxhtml/autostart.el")

(load-cfg-files '(
                  "main"
                  "autocomplete"
                  "helm"
                  "popwin"
                  "buffer"
                  "ido" ;; ido changes and enhancements
                  "mac" ;; mac specific key bindings and ui changes
                  "modeline"
                  "project" ;; project definitions
                  "vim" ;; vim keybindings yo
                  "grep"
                  "ack"
                  "ag"
                  "flycheck"
                  "regexp"
                  "zone"
                  "expandregion"
                  "shell"
                  ;; "git"
                  "github"
                  "killring"
                  ;; languages
                  "color_identifiers"
                  "python"
                  "javascript"
                  "ruby"
                  "yaml"
                  "html"
                  "css"
                  "docker"
                  ;; generic web mode multi-mode mode thing mode (mode) mode so many modes
                  "web"
                  ))

(setq custom-file "~/.emacs.d/configs/custom.el")
