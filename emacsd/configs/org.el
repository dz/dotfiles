(require 'org)
(require 'org-download)

;; Make Org mode work with files ending in .org
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

;; always show inline images
(setq org-startup-with-inline-images t)

;; org download options
;; use curl to fetch images
(setq org-download-backend "curl")

;; let me use shift-arrows to select
(setq org-support-shift-select 'always)

;; give me back my arrow movements
(define-key org-mode-map (kbd "M-<right>") nil)
(define-key org-mode-map (kbd "M-<left>") nil)
(define-key org-mode-map (kbd "M-<up>") nil)
(define-key org-mode-map (kbd "M-<down>") nil)

;; make enter key expand
(define-key org-mode-map (kbd "<return>") 'org-show-children)


;;  make org mode with with evil
(require 'evil-org)
(add-hook 'org-mode-hook 'evil-org-mode)
(evil-org-set-key-theme '(return navigation insert textobjects additional calendar))
(require 'evil-org-agenda)
(evil-org-agenda-set-keys)

(evil-define-key 'normal evil-org-mode-map (kbd ">") 'org-metaright)
(evil-define-key 'normal evil-org-mode-map (kbd "<") 'org-metaleft)
