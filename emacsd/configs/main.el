;; Backup files are annoying. put them in a centralized location.
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

;; disable auto-save
(setq auto-save-default nil)

;; dired buffers are temporary
(require 'tempbuf)
(add-hook 'dired-mode-hook 'turn-on-tempbuf-mode)

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
(setq linum-format "%4d ")
;; (global-linum-mode 1)
;; ;; but not in all modes
(require 'linum-off)

;; add keybinding to toggle
(global-set-key "\M-L" 'linum-mode)

;; give windows a margin
(add-hook 'window-configuration-change-hook
          (lambda ()
            (set-window-margins (car (get-buffer-window-list (current-buffer) nil t)) 1 1)))

(scroll-bar-mode -1)
(menu-bar-mode 1)
(tool-bar-mode -1)

;; splash screens suck.
(setq inhibit-splash-screen t)

;; use UTF-8
(prefer-coding-system 'utf-8)

;; remove retarded beeping noises
(setq ring-bell-function 'ignore)

;; use spaces, not tabs
(setq-default indent-tabs-mode nil)
;; default tab width
(setq default-tab-width 2)
(setq-default default-tab-width 2)
(setq-default c-basic-offset 2)
(setq standard-indent 2)

;; disable line wrapping
(setq default-truncate-lines nil)

;; emacs default undo/redo confuses me
(require 'redo+)

;; show paired parens
(show-paren-mode 1)
;; turn off paren matching delay
(setq show-paren-delay 0)

;; use y or n prompts
(defalias 'yes-or-no-p 'y-or-n-p)

;; hilight when searching and replacing
(setq search-highlight t
      query-replace-highlight t)

(define-key isearch-mode-map [remap isearch-delete-char] 'isearch-del-char)

;; ignore case when completing
(setq read-buffer-completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)

;; always remove trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; use winner-mode to remember window states
(winner-mode 1)

;; highlight indentation
(require 'highlight-indentation)
(setq highlight-indentation-offset 2)

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
(global-set-key "\C-h" 'backward-char)
(global-set-key "\C-l" 'forward-char)

(global-set-key (kbd "RET") 'newline-and-indent)

;; recently opened files rock
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-saved-items 5000)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

(defun count-buffers (&optional display-anyway)
  "Display or return the number of buffers."
  (interactive)
  (let ((buf-count (length (buffer-list))))
    (if (or (interactive-p) display-anyway)
        (message "%d buffers in this Emacs" buf-count)) buf-count))

(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))
(defalias 'rename-file-and-buffer 'rename-current-buffer-file)

;; don't put instructions into scratch buffer
(setq initial-scratch-message nil)

;;enable smart scratch buffers
(autoload 'scratch "scratch" nil t)

;;auto reload files that have been changed on disk
(global-auto-revert-mode 1)

(setq diff-switches "-u -w")

;; ace jump mode
(require 'ace-jump-mode)


(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(defalias 'ts 'toggle-window-split)

(defun delete-current-buffer-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

(defun open-line-below ()
  (interactive)
  (end-of-line)
  (newline)
  (indent-for-tab-command))

(defun open-line-above ()
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-for-tab-command))

(global-set-key (kbd "<C-return>") 'open-line-below)
(global-set-key (kbd "<C-S-return>") 'open-line-above)

(require 'simpleclip)
(simpleclip-mode 1)

;; imenu
(setq imenu-max-item-length 500)
