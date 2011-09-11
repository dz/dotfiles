;; show trailing whitespace
(setq-default show-trailing-whitespace t)
;; delete trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

