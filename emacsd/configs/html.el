;; set indent to 2
(setq nxml-child-indent 2)

;; remap finish element
(global-set-key "\M-." 'nxml-finish-element)

(setq mumamo-background-colors nil)
;;(add-to-list 'auto-mode-alist '("\\.html$" . django-html-mumamo-mode))

(add-hook 'html-mode-hook
          (lambda ()
            (set (make-local-variable 'sgml-basic-offset) 2)))

(add-hook 'html-mode-hook 'highlight-indentation-mode)
