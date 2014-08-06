(ac-config-default)
(global-set-key "\M-/" 'auto-complete)
(define-key ac-mode-map (kbd "M-/") 'auto-complete)
(setq ac-use-menu-map t)
;; Default settings
(define-key ac-menu-map "\C-n" 'ac-next)
(define-key ac-menu-map "\C-p" 'ac-previous)
;; Navigation in autocomplete menues gets hijacked by evil
(define-key ac-completing-map (kbd "C-n") 'ac-next)
(define-key ac-completing-map (kbd "C-p") 'ac-previous)
;; Let me stop autocompleting the emacs/evil way
(define-key ac-completing-map (kbd "C-g") 'ac-stop)
(define-key ac-completing-map (kbd "ESC") 'evil-normal-state)

(setq ac-ignore-case t)

(setq global-auto-complete-mode t
      ac-show-menu-timer 0.1
      ac-auto-show-menu t)
