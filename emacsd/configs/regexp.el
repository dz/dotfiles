(require 'visual-regexp-steroids)

;; overide emacs built in replace methods
(defalias 'replace 'vr/replace)
(defalias 'query-replace 'vr/query-replace)

(setq vr/default-regexp-modifiers '(:I nil :M t :S nil :U nil))
