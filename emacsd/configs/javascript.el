;; javascript items
(add-to-list 'auto-mode-alist '("\\.js$" . js-mode))

(defun custom-js-mode-hook ()
  (flycheck-mode)
  (setq js-indent-level 2))

(add-hook 'js-mode-hook 'custom-js-mode-hook)
