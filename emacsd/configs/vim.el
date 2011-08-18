;; vimpulse
(setq viper-mode t
      viper-case-fold-search t
      viper-want-ctl-h-help t
      viper-auto-indent t
      viper-ex-style-editing nil
      viper-ex-style-motion nil
      viper-no-multiple-ESC t
      viper-inhibit-startup-message t
      viper-expert-level 5
      viper-always t
      viper-minibuffer-vi-face nil
      viper-minibuffer-emacs-face nil
      viper-vi-style-in-minibuffer nil)

(require 'viper)
(require 'vimpulse)

; Restore some Emacs keys
(define-key viper-vi-global-user-map "\C-n" 'next-line)
(define-key viper-insert-global-user-map "\C-n" 'next-line)
(define-key viper-insert-global-user-map "\C-p" 'previous-line)
(define-key viper-vi-global-user-map "\C-p" 'previous-line)

;; remap arrows
(global-set-key (kbd "<up>") 'viper-previous-line)
(global-set-key (kbd "<down>") 'viper-next-line)

;; alow / and ? to use incremental search
(define-key viper-vi-global-user-map "/" 'viper-isearch-forward)
(define-key viper-vi-global-user-map "?" 'viper-isearch-backward)

;; allow control-e and control-a to behave normally
(define-key viper-vi-global-user-map "\C-e" 'viper-goto-eol)
(define-key viper-vi-global-user-map "\C-a" 'viper-bol-and-skip-white)

;; Always end searches at the beginning of the matching expression.
(add-hook 'isearch-mode-end-hook 'custom-goto-match-beginning)
(defun custom-goto-match-beginning ()
  "Use with isearch hook to end search at first char of match."
  (when isearch-forward
    (unless (null isearch-other-end)
      (goto-char isearch-other-end))))

