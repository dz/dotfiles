;; set emacs to use nxml-mode for html-mode instances
(fset 'html-mode 'nxml-mode)

;; set indent to 4
(setq nxml-child-indent 4)

;; remap finish element
(global-set-key "\M-." 'nxml-finish-element)

;; html filetypes
(add-to-list 'auto-mode-alist '("\\.dtpl$" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.jquery.tmpl$" . nxml-mode))
