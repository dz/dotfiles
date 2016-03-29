(require 'magit)
(require 'magit-blame)

;; this makes opening files faster i dunno why it just works
(remove-hook 'find-file-hooks 'vc-find-file-hook)

;; i'm using a weird version of emacs maybe so this workaround
;; is needed
(eval-after-load 'magit
  '(setq magit-process-connection-type nil))

;; magit aliases
(defalias 'blame 'magit-blame-mode)
(defalias 'bl 'magit-blame-mode)
(defalias 'status 'magit-status)
(defalias 'st 'magit-status)

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
