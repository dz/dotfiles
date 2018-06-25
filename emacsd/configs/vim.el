(setq evil-shift-width 2)
(require 'evil)
(require 'evil-surround)
( require 'evil-goggles )

(evil-goggles-mode)
(setq evil-goggles-pulse t)

(loop for (mode . state) in '((inferior-emacs-lisp-mode . emacs)
                              (nrepl-mode . insert)
                              (pylookup-mode . emacs)
                              (comint-mode . normal)
                              (shell-mode . insert)
                              (git-rebase-mode . emacs)
                              (term-mode . emacs)
                              (help-mode . emacs)
                              (helm-grep-mode . emacs)
                              (grep-mode . emacs)
                              (bc-menu-mode . emacs)
                              (rdictcc-buffer-mode . emacs)
                              (dired-mode . normal)
                              (magit-mode . normal)
                              (magit-diff-mode . normal)
                              (magit-status-mode . normal)
                              (magit-popup-mode . normal)
                              (wdired-mode . normal)
                              (magit-log-edit-mode . insert)
                              (magit-popup-mode . emacs)
                              (ivy-occur-mode . emacs)
                              (ivy-occur-grep-mode . emacs)
                              (git-commit-mode . insert))
      do (evil-set-initial-state mode state))

(evil-define-motion evil-little-word (count)
  :type exclusive
  (let* ((case-fold-search nil)
         (count (if count count 1)))
    (while (> count 0)
      (forward-char)
      (search-forward-regexp "[_A-Z]\\|\\W" nil t)
      (backward-char)
      (decf count))))

(define-key evil-operator-state-map (kbd "lw") 'evil-little-word)

(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)

(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

(define-key evil-normal-state-map "\C-e" 'end-of-line)
(define-key evil-motion-state-map "\C-e" 'end-of-line)
(define-key evil-insert-state-map "\C-e" 'end-of-line)
(define-key evil-normal-state-map "\C-n" 'evil-next-visual-line)
(define-key evil-insert-state-map "\C-n" 'evil-next-visual-line)
(define-key evil-normal-state-map "\C-e" 'end-of-line)
(define-key evil-normal-state-map "\C-p" 'evil-previous-visual-line)
(define-key evil-motion-state-map "\C-p" 'evil-previous-visual-line)
(define-key evil-insert-state-map "\C-p" 'evil-previous-visual-line)
(define-key evil-normal-state-map "\C-a" 'evil-first-non-blank)
(define-key evil-motion-state-map "\C-a" 'evil-first-non-blank)
(define-key evil-insert-state-map "\C-a" 'evil-first-non-blank)
(define-key evil-normal-state-map (kbd "TAB") 'evil-indent-line)
(define-key evil-visual-state-map (kbd "TAB") 'evil-indent-line)

;; grep/compilation mode thigns
(add-hook 'compilation-mode-hook '(lambda ()
                                    (local-unset-key "g")
                                    (local-unset-key "h")))
 (define-key helm-git-grep-map "g" nil)
 (evil-define-key 'normal helm-git-grep-map "g" nil)
 (evil-define-key 'normal helm-git-grep-map "gg" 'evil-beginning-of-buffer)

;; dired mode things
(evil-define-key 'normal dired-mode-map "h" 'dired-up-directory)
(evil-define-key 'normal dired-mode-map "l" 'dired-find-alternate-file)
(evil-define-key 'normal dired-mode-map "O" 'dired-sort-toggle-or-edit)
(evil-define-key 'normal dired-mode-map "o" 'dired-find-file-other-window)
(evil-define-key 'normal dired-mode-map "v" 'dired-toggle-marks)
(evil-define-key 'normal dired-mode-map "m" 'dired-mark)
(evil-define-key 'normal dired-mode-map "u" 'dired-unmark)
(evil-define-key 'normal dired-mode-map "R" 'dired-do-rename-regexp)
(evil-define-key 'normal dired-mode-map "U" 'dired-unmark-all-marks)
(evil-define-key 'normal dired-mode-map "c" 'dired-create-directory)
(evil-define-key 'normal dired-mode-map "n" 'evil-search-next)
(evil-define-key 'normal dired-mode-map "N" 'evil-search-previous)
(evil-define-key 'normal dired-mode-map "q" 'kill-this-buffer)

;; magit things
(with-eval-after-load 'magit
  (dolist (magitmap (list
                     magit-diff-mode-map
                     magit-status-mode-map
                     magit-mode-map
                     magit-file-section-map
                     magit-hunk-section-map
                     magit-unstaged-section-map
                     magit-staged-section-map
                     magit-stash-section-map
                     magit-stashes-section-map
                     magit-untracked-section-map
                     magit-branch-section-map
                     magit-remote-section-map
                     magit-tag-section-map
                     ))
    (define-key magitmap "c" 'magit-commit-popup)
    (define-key magitmap (kbd "TAB") 'magit-section-toggle)
    (define-key magitmap (kbd "<tab>") 'magit-section-toggle)
    (define-key magitmap "j" 'evil-next-line)
    (define-key magitmap "k" 'evil-previous-line)
    (define-key magitmap "K" 'magit-discard)))

;; change mode-line color by evil state
(lexical-let ((default-color (cons (face-background 'mode-line)
                                   (face-foreground 'mode-line))))
  (add-hook 'post-command-hook
            (lambda ()
              (let ((color (cond ((minibufferp) default-color)
                                 ((evil-insert-state-p) '("#124DA1" . "#ffffff"))
                                 (t default-color))))
                (set-face-background 'mode-line (car color))
                (set-face-foreground 'mode-line (cdr color))))))

(evil-mode 1)
(global-evil-surround-mode 1)
