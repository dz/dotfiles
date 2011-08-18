;; options to make emacs work better in a mac env

(setq interprogram-cut-function nil)
(setq interprogram-paste-function nil)
(defun paste-from-pasteboard ()
  (interactive)
  (and mark-active (filter-buffer-substring (region-beginning) (region-end) t))
  (insert (ns-get-pasteboard))
  )
(defun copy-to-pasteboard (p1 p2)
  (interactive "r*")
  (ns-set-pasteboard (buffer-substring p1 p2))
  (message "Copied selection to pasteboard")
  )
(defun cut-to-pasteboard (p1 p2) (interactive "r*") (ns-set-pasteboard (filter-buffer-substring p1 p2 t)) )

;; allow command-c to copy as normal
(global-set-key "\M-c" 'copy-to-pasteboard)

;; allow command-C to cut as normal
(global-set-key "\M-C" 'cut-to-pasteboard)

;; set command-v to paste
(global-set-key "\M-v" 'paste-from-pasteboard)

;; use comamnd-1, command-2, etc instead of c-x number
(global-set-key "\M-1" 'delete-other-windows)
(global-set-key "\M-2" 'split-window-vertically)
(global-set-key "\M-3" 'split-window-horizontally)
(global-set-key "\M-0" 'delete-window)
(global-set-key "\M-+" 'balance-windows)

;; allow command-h to hide emacs
(global-set-key "\M-h" 'ns-do-hide-emacs)

;; allow command-a to select all
(global-set-key "\M-a" 'mark-whole-buffer)

;; allow command-shift-f to trigger fulscreen
(global-set-key [(meta return)] 'ns-toggle-fullscreen)

;; allow command-k to kill buffer
(global-set-key "\M-k" 'ido-kill-buffer)

;; allow command-o to open file
(global-set-key "\M-o" 'find-file)

;; allow command-t to find file in project
(global-set-key "\M-t" 'ido-find-file-in-tag-files)

;; allow command-r to open a recently opened file
;; (global-set-key "\M-r" 'recentf-ido-find-file)
;; (global-set-key "\M-r" 'recentf-interactive-complete)
(global-set-key "\M-r" 'xsteve-ido-choose-from-recentf)

;; command-R to refresh all open buffers
(global-set-key "\M-R" 'revert-all-buffers)

;; allow command-b to switch between buffers
(global-set-key "\M-b" 'ido-switch-buffer)

;; set command-shift-b to open ibuffer
(global-set-key "\M-B" 'ibuffer)

;; allow command-f to search in file
(global-set-key "\M-f" 'isearch-forward)
;; repeat search with command-g
(global-set-key "\M-g" 'isearch-repeat-forward)

;; allow comamnd-n to create a new frame
(global-set-key "\M-n" 'make-frame-command)

;; allow command-` to switch between frames
(global-set-key "\M-`" 'other-frame)


;; allow command-w to close a frame
(global-set-key "\M-w" 'delete-frame)

;; allow command-l to go to line
(global-set-key "\M-l" 'goto-line)

;; allow command-s to save
(global-set-key "\M-s" 'save-buffer)

;; set control-f to go forward a page
(global-set-key "\C-f" 'scroll-up)
;; set control-b to go back a page
(global-set-key "\C-b" 'scroll-down)


;; proper redo and undo
(global-set-key "\M-z" 'undo)
(global-set-key "\M-Z" 'redo)

