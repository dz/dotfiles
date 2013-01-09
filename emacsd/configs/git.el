;; git settings

;; magit settings

(require 'magit)
(require 'magit-blame)

;; i'm using a weird version of emacs maybe so this workaround
;; is needed
(eval-after-load 'magit
  '(setq magit-process-connection-type nil))

;; the default magit diff colors are *really* bad
;; change magit diff colors
(eval-after-load 'magit
  '(progn
     (set-face-foreground 'magit-diff-add "green3")
     (set-face-foreground 'magit-diff-del "red3")
     (set-face-foreground 'diff-refine-change "green3")
     (set-face-background 'diff-refine-change "DarkGreen")
     (when (not window-system)
       (set-face-background 'magit-item-highlight "black"))))

;; make magit status go full-screen but remember previous window
;; settings
;; from: http://whattheemacsd.com/setup-magit.el-01.html
(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

;; restore previously hidden windows
(defadvice magit-quit-window (around magit-restore-screen activate)
  (let ((current-mode major-mode))
    ad-do-it
    ;; we only want to jump to register when the last seen buffer
    ;; was a magit-status buffer.
    (when (eq 'magit-status-mode current-mode)
      (jump-to-register :magit-fullscreen))))

;; to play well with evil, i still want hljk in magit
;; (evil-add-hjkl-bindings magit-branch-manager-mode-map 'emacs
;;  "K" 'magit-discard-item
;;  "L" 'magit-key-mode-popup-logging)
;; (evil-add-hjkl-bindings magit-status-mode-map 'emacs
;;  "K" 'magit-discard-item
;;  "l" 'magit-key-mode-popup-logging
;;  "h" 'magit-toggle-diff-refine-hunk)

;; magit aliases
(defalias 'blame 'magit-blame-mode)
(defalias 'bl 'magit-blame-mode)
(defalias 'status 'magit-status)
(defalias 'st 'magit-status)

;; config options

;; use ido to look for branches
(setq magit-completing-read-function 'magit-ido-completing-read)
(setq magit-default-tracking-name-function 'magit-default-tracking-name-branch-only)
(setq magit-status-buffer-switch-function 'switch-to-buffer)
(setq magit-diff-refine-hunk t)
(setq magit-rewrite-inclusive 'ask)
(setq magit-save-some-buffers nil)

(defun magit-toggle-whitespace ()
  (interactive)
  (if (member "-w" magit-diff-options)
      (magit-dont-ignore-whitespace)
    (magit-ignore-whitespace)))

(defun magit-ignore-whitespace ()
  (interactive)
  (add-to-list 'magit-diff-options "-w")
  (magit-refresh))

(defun magit-dont-ignore-whitespace ()
  (interactive)
  (setq magit-diff-options (remove "-w" magit-diff-options))
  (magit-refresh))

(define-key magit-status-mode-map (kbd "W") 'magit-toggle-whitespace)
