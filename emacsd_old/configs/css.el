;; css mode
(autoload 'css-mode "css-mode" "Mode for editing CSS files" t)

(setq cssm-indent-level 4)
(setq cssm-newline-before-closing-bracket t)
(setq cssm-indent-function #'cssm-c-style-indenter)
(setq cssm-mirror-mode t)

(provide 'css)
