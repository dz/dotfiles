(setq evil-shift-width 2)
(require 'evil)

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
(define-key evil-normal-state-map "\C-a" 'evil-first-non-blank)
(define-key evil-motion-state-map "\C-a" 'evil-first-non-blank)
(define-key evil-insert-state-map "\C-a" 'evil-first-non-blank)
(define-key evil-normal-state-map (kbd "TAB") 'evil-indent-line)
(define-key evil-visual-state-map (kbd "TAB") 'evil-indent-line)

(evil-mode 1)
