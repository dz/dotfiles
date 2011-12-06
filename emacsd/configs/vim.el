(add-to-list 'load-path "~/.emacs.d/evil")
(setq evil-shift-width 4)
(require 'evil)
(evil-mode 1)

(define-key evil-motion-state-map (kbd "C-e") 'end-of-line)
(define-key evil-insert-state-map (kbd "C-e") 'end-of-line)
(define-key evil-normal-state-map (kbd "C-e") 'end-of-line)

(define-key evil-motion-state-map (kbd "<down>") 'evil-next-line)
(define-key evil-motion-state-map (kbd "<up>") 'evil-previous-line)

(define-key evil-normal-state-map (kbd "<down>") 'evil-next-line)
(define-key evil-normal-state-map (kbd "<up>") 'evil-previous-line)
