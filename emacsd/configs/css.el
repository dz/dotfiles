;; fix css mode
(require 'less-css-mode)
(setq cssm-indent-level 4)
(setq cssm-newline-before-closing-bracket t)
(setq cssm-indent-function #'cssm-c-style-indenter)
(setq cssm-mirror-mode t)

;; sass support
(require 'scss-mode)
;; disable autocompilation on save
(setq scss-compile-at-save nil)

