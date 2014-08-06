;; options to make emacs work better in a mac env
;; also to make the ui look prettier

;;set font
(setq my-font "Inconsolata-dz-15")
(set-frame-font my-font)
(add-hook 'after-make-frame-functions
          (lambda (cur-frame)
            (modify-frame-parameters cur-frame
                                     (list
                                      (cons 'font my-font)))))

(load-theme 'ir-black t)

;; set larger line spacing
(setq-default line-spacing 2)

;; stop cursor blinking
(blink-cursor-mode 0)

;; change vertical border color
(set-face-attribute 'vertical-border nil :foreground "#666666")

;; yes to line highlighting
(global-hl-line-mode 1)

;; turn off fringe
(set-fringe-mode 0)

(set-face-italic-p 'italic nil)

;; always display cursor's column position
(column-number-mode 1)

(defun smart-beginning-of-line ()
  "Move point to first non-whitespace character or beginning-of-line.

Move point to the first non-whitespace character on this line.
If point was already at that position, move point to beginning of line."
  (interactive)
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))

(global-set-key "\C-a" 'smart-beginning-of-line)
(define-key minibuffer-local-map (kbd "C-a") 'beginning-of-line)

;; allow command-o to open file
(global-set-key "\M-o" 'find-file)

(global-set-key "\M-r" 'xsteve-ido-choose-from-recentf)

;; allow command-c to copy as normal
(global-set-key "\M-c" 'simpleclip-copy)

;; allow command-C to cut as normal
(global-set-key "\M-C" 'simpleclip-cut)

;; set command-v to paste
(global-set-key "\M-v" 'simpleclip-paste)

;; use comamnd-1, command-2, etc instead of c-x number
(global-set-key "\M-1" 'delete-other-windows)
(global-set-key "\M-2" 'split-window-vertically)
(global-set-key "\M-3" 'split-window-horizontally)
(global-set-key "\M-0" 'delete-window)

(global-set-key "\M--" (kbd "C-u - 1 6 C-x {"))
(global-set-key "\M-+" (kbd "C-u - 1 6 C-x }"))
(global-set-key "\M-=" 'balance-windows)

;; bind for winner-mode
(global-set-key "\M-[" 'winner-undo)
(global-set-key "\M-]" 'winner-redo)

;; allow command-h to hide emacs
(global-set-key "\M-h" 'ns-do-hide-emacs)

;; allow command-a to select all
(global-set-key "\M-a" 'mark-whole-buffer)

(setq ns-use-native-fullscreen nil)

;; allow command-return to trigger fulscreen
(defun mac-toggle-max-window ()
  (interactive)
  (set-frame-parameter nil 'fullscreen
                       (if (frame-parameter nil 'fullscreen)
                           nil
                         'fullboth)))
(global-set-key [(meta return)] 'mac-toggle-max-window)

;; allow command-k to kill buffer
(global-set-key "\M-k" 'ido-kill-buffer)

;; kill the buffer *and* delete the file
(global-set-key "\M-K" 'delete-current-buffer-file)

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

;; command-j for ace jump
(global-set-key "\M-j" 'ace-jump-mode)

;; make command-J pop mark ring
(global-set-key "\M-J" 'pop-to-mark-command)
