;; fix css mode
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

