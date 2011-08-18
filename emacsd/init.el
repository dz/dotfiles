;;turn on server mode
(server-start)

(load-library "iso-transl")

(add-to-list 'load-path "~/.emacs.d/vendor")
;; recursively add subdirs in vendor to load path
(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
    (let* ((my-lisp-dir "~/.emacs.d/vendor")
           (default-directory my-lisp-dir))
      (setq load-path (cons my-lisp-dir load-path))
      (normal-top-level-add-subdirs-to-load-path)))

(defconst emacs-config-dir "~/.emacs.d/configs/" "")
(setq load-path (cons "~/.emacs.d" load-path))
(defun load-cfg-files (filelist)
  (dolist (file filelist)
    (load (expand-file-name
           (concat emacs-config-dir file)))
    (message "Loaded config file: %s" file)
    ))

;; load nxhtml
(load "~/.emacs.d/vendor/nxhtml/autostart.el")
(setq mumamo-background-colors nil)
(setq nxhtml-skip-welcome t)
(setq rng-nxml-auto-validate-flag nil)

(load-cfg-files '("main"
                  "buffers" ;; buffer managements settings
                  "mac" ;; mac specific options
                  "interface"
                  "whitespace"
                  "project" ;;project settings
                  "irc" ;;irc settings
                  "vim" ;;vimpulse settings

                  ;;languages
                  "html"
                  "css"
                  "xml"
                  "js"
                  "python"
                  ;;custom file
                  "custom"
                  ))

(setq custom-file "~/.emacs.d/configs/custom.el")

;; remove font styles
;; (mapc
;; (lambda (face)
;;   (set-face-attribute face nil :weight 'normal :underline nil :slant 'normal))
;; (face-list))
