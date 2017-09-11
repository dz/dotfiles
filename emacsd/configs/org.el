(require 'org)
(require 'org-download)

;; Make Org mode work with files ending in .org
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
;; always show inline images
(setq org-startup-with-inline-images t)

;; org download options
;; use curl to fetch images
(setq org-download-backend "curl")
