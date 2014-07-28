;; init file for company model
(require 'company)
(global-company-mode t)
(add-hook 'after-init-hook 'global-company-mode)

(setq company-idle-delay t)
