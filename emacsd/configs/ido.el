;; settings for ido
(require 'ido)
(require 'ido-ubiquitous)
(require 'ido-vertical-mode)
(ido-mode 1)
(ido-everywhere)
(ido-ubiquitous)
(ido-vertical-mode 1)

(setq ido-create-new-buffer (quote never))
(setq ido-enable-flex-matching t)
(setq ido-enable-last-directory-history nil)
(setq ido-enable-regexp nil)

;; override ido flex wth improved flx matching
;; https://github.com/lewang/flx
;; (require 'flx-ido)
;; (flx-ido-mode 1)
;; ;; disable ido faces to see flx highlights.
;; (setq ido-use-faces nil)
;; (setq flx-ido-threshhold 1000)

(defun xsteve-ido-choose-from-recentf ()
  "Use ido to select a recently opened file from the `recentf-list'"
  (interactive)
  (let ((home (expand-file-name (getenv "HOME"))))
    (find-file
     (ido-completing-read "Recentf open: "
                          (mapcar (lambda (path)
                                    (replace-regexp-in-string home "~" path))
                                  recentf-list)
                          nil t))))

;; allow ido to work with recentf
(defun recentf-ido-find-file ()
  "Find a recent file using Ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))

(defun recentf-interactive-complete ()
  "find a file in the recently open file using ido for completion"
  (interactive)
  (let* ((all-files recentf-list)
         (file-assoc-list (mapcar (lambda (x) (cons (file-name-nondirectory x) x)) all-files))
         (filename-list (remove-duplicates (mapcar 'car file-assoc-list) :test 'string=))
         (ido-make-buffer-list-hook
          (lambda ()
            (setq ido-temp-list filename-list)))
         (filename (ido-read-buffer "Find Recent File: "))
         (result-list (delq nil (mapcar (lambda (x) (if (string= (car x) filename) (cdr x))) file-assoc-list)))
         (result-length (length result-list)))
    (find-file
     (cond
      ((= result-length 0) filename)
      ((= result-length 1) (car result-list))
      ( t
        (let ( (ido-make-buffer-list-hook
                (lambda ()
                  (setq ido-temp-list result-list))))
          (ido-read-buffer (format "%d matches:" result-length))))
      ))))

;; add additional keybindings
(defun my-ido-keys ()
  (define-key ido-completion-map "\C-r" 'ido-reread-directory)
  (define-key ido-completion-map "\C-w" 'ido-delete-backward-updir)
  (define-key ido-completion-map [up] 'ido-prev-match)
  (define-key ido-completion-map [down] 'ido-next-match)
  (define-key ido-completion-map [(control n)] 'ido-next-match)
  (define-key ido-completion-map [(control p)] 'ido-prev-match))
(add-hook 'ido-setup-hook 'my-ido-keys)
