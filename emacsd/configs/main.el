;; Backup files are annoying. put them in a centralized location.
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

;; disable auto-save
(setq auto-save-default nil)

;; dired buffers are temporary
(require 'tempbuf)
(add-hook 'dired-mode-hook 'turn-on-tempbuf-mode)

;; use proper redo
(require 'redo)

;; enable shift
(pc-selection-mode)

;; font lock mode is good
(global-font-lock-mode t)

;; visual feedback on selections
(setq-default transient-mark-mode t)

;; always end a file with a new line
(setq require-final-newline t)

;;stop at end of the file
(setq next-line-add-newlines nil)

;; change meta key to be the command key
(setq ns-command-modifier (quote meta))

;; when deleting, move file to Trash
(setq delete-by-moving-to-trash t)

;; I need my line numbers
(require 'linum)
(setq linum-format "%3d ")
(global-linum-mode 1)
;; but not in all modes
(require 'linum-off)

;; disable GUI stuff
(scroll-bar-mode -1)
(menu-bar-mode 1)
(tool-bar-mode -1)

;; splash screens suck.
(setq inhibit-splash-screen t)

;; macs have an issue with this apparently
(setq process-connect-type nil)

;; use UTF-8
(prefer-coding-system 'utf-8)

;; remove retarded beeping noises
(setq ring-bell-function 'ignore)

;; use spaces, not tabs
(setq-default indent-tabs-mode nil)
;; default tab width
(setq default-tab-width 4)
(setq-default default-tab-width 4)
(setq-default c-basic-offset 4)

;; disable line wrapping
(setq default-truncate-lines nil)

;; show paired parens
(show-paren-mode 1)
;; turn off paren matching delay
(setq show-paren-delay 0)

;; use y or n prompts
(defalias 'yes-or-no-p 'y-or-n-p)

;; hilight when searching and replacing
(setq search-highlight t
      query-replace-highlight t)

;; ignore case when completing
(setq read-buffer-completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)

;; change behavior of buffers with same name
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t) ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

;; activate easy buffer moving
(when (fboundp 'windmove-default-keybindings)
      (windmove-default-keybindings 'meta))

;; place saving is nice
(require 'saveplace)                          ;; get the package
(setq save-place-file "~/.emacs.d/saveplace") ;; keep my ~/ clean
(setq-default save-place t)                   ;; activate it for all buffers

;; make incremental search wrap around files
(defadvice isearch-search (after isearch-no-fail activate)
  (unless isearch-success
    (ad-disable-advice 'isearch-search 'after 'isearch-no-fail)
    (ad-activate 'isearch-search)
    (isearch-repeat (if isearch-forward 'forward))
    (ad-enable-advice 'isearch-search 'after 'isearch-no-fail)
    (ad-activate 'isearch-search)))

;; what-face command helps make life easy for theming
(defun what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

;; revert all buffer function
(defun revert-all-buffers ()
  "Refreshes all open buffers from their respective files"
  (interactive)
  (let* ((list (buffer-list))
         (buffer (car list)))
    (while buffer
      (when (buffer-file-name buffer)
        (progn
          (set-buffer buffer)
          (revert-buffer t t t)))
      (setq list (cdr list))
      (setq buffer (car list))))
  (message "Refreshing open files"))

;; set control-w to backwards kill word
(global-set-key "\C-w" 'backward-kill-word)

(global-set-key (kbd "RET") 'newline-and-indent)

;; recently opened files rock
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-saved-items 500)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;; browse kill ring rocks to
(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)

;;tab stops in 4 steps
(setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100 104 108 112 116 120))

(defun count-buffers (&optional display-anyway)
  "Display or return the number of buffers."
  (interactive)
  (let ((buf-count (length (buffer-list))))
    (if (or (interactive-p) display-anyway)
        (message "%d buffers in this Emacs" buf-count)) buf-count))

;; source: http://steve.yegge.googlepages.com/my-dot-emacs-file
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file name new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

;;enable scpaste
(autoload 'scpaste "scpaste" "Paste the current buffer." t nil)

;;enable smart scratch buffers
(autoload 'scratch "scratch" nil t)

;;auto reload files that have been changed on disk
(global-auto-revert-mode 1)
