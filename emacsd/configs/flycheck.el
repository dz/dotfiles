(require 'flycheck)

;; highlight the entire line
(setq flycheck-highlighting-mode 'lines)

(setq flycheck-display-errors-function 'flycheck-display-error-messages-unless-error-list)
