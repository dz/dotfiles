;; rainbowwwwws
(require 'rainbow-mode)

(autoload 'rainbow-mode "rainbow-mode.el" "Minor mode for editing HTML colors" t)
(add-to-list 'auto-mode-alist '("\\.html$" . rainbow-mode))
(add-to-list 'auto-mode-alist '("\\.erb$" . rainbow-mode))
(add-to-list 'auto-mode-alist '("\\.less$" . rainbow-mode))
(add-to-list 'auto-mode-alist '("\\.css$" . rainbow-mode))
(add-to-list 'auto-mode-alist '("\\.scss$" . rainbow-mode))
(add-to-list 'auto-mode-alist '("\\.coffee$" . rainbow-mode))

;; less support
(require 'less-css-mode)
;; sass support
(require 'scss-mode)
;; disable autocompilation on save
(setq scss-compile-at-save nil)

(setq cssm-indent-level 2)
(setq cssm-newline-before-closing-bracket t)
(setq cssm-indent-function #'cssm-c-style-indenter)
(setq cssm-mirror-mode t)

(setq css-indent-offset 2)

(add-hook 'css-mode-hook 'highlight-indentation-mode)
