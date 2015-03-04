;; javascript items
(add-to-list 'auto-mode-alist '("\\.js$" . js-mode))
(add-to-list 'auto-mode-alist '("\\.es6\\'" . js-mode))

(defun custom-js-mode-hook ()
  (flycheck-mode)
  ;; make these chars all behave like normal
  (define-key js-mode-map "," 'self-insert-command)
  (define-key js-mode-map ";" 'self-insert-command)
  (define-key js-mode-map "'" 'self-insert-command)
  (define-key js-mode-map "\"" 'self-insert-command)
  (define-key js-mode-map "{" 'self-insert-command)
  (define-key js-mode-map "}" 'self-insert-command)
  (define-key js-mode-map "(" 'self-insert-command)
  (define-key js-mode-map ")" 'self-insert-command)
 (setq js-indent-level 2))

(add-hook 'js-mode-hook 'custom-js-mode-hook)
