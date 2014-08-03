(require 'popwin)
(popwin-mode 1)
(setq popwin:pop-window-height 30)
(setq popwin:adjust-other-windows t)

;; helm
(setq popwin:special-display-config '(("^\*helm.+\*$" :regexp t :height 30)))

(push "*Backtrace*" popwin:special-display-config)

;; vc
(push "*vc-diff*" popwin:special-display-config)
(push "*vc-change-log*" popwin:special-display-config)

;; ag
(push "*ag*" popwin:special-display-config)
