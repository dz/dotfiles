(setq auto-mode-alist (cons '("\\.markdown" . markdown-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.md" . markdown-mode) auto-mode-alist))

;; unset keys in markdown to make window + fullscreen work
(add-hook 'markdown-mode-hook
          (lambda ()
            (local-unset-key (kbd "M-<return>"))
            (local-unset-key (kbd "M-<left>"))
            (local-unset-key (kbd "M-<right>"))
            (local-unset-key (kbd "M-<up>"))
            (local-unset-key (kbd "M-<down>"))))

(setq markdown-asymmetric-header t)
