;; init file for company model
(require 'company)

;; tern options
(add-to-list 'company-backends 'company-tern)
(setq company-tern-property-marker "")
(setq company-tern-meta-as-single-line t)
(setq company-tooltip-align-annotations t)


(global-company-mode t)
(add-hook 'after-init-hook 'global-company-mode)
(setq company-idle-delay 0)
