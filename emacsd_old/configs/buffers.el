;; ibuffer
(require 'ibuffer)
(require 'ibuf-ext)

;; Use human readable Size column instead of original one
(define-ibuffer-column size-h
  (:name "Size" :inline t)
  (cond
   ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
   ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
   (t (format "%8d" (buffer-size)))))

;; Modify the default ibuffer-formats
(setq ibuffer-formats
      '((mark modified read-only " "
              (name 18 18 :left :elide)
              " "
              (size-h 9 -1 :right)
              " "
              (mode 16 16 :left :elide)
              " "
              filename-and-process)))

;; don't show buffers that start with "*"
(add-to-list 'ibuffer-never-show-predicates "^\\*")

(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("Emacs Config" (or (filename . ".emacs.d")
                                   (filename . "emacs-config")))
               ("Disqus"
                (filename . "Disqus"))
               ("Help" (or (name . "\*Help\*")
                           (name . "\*Apropos\*")
                           (name . "\*info\*")))
               ("ERC"   (mode . erc-mode))))))

(add-hook 'ibuffer-mode-hook
  (lambda ()
    (ibuffer-auto-mode 1)
    (ibuffer-switch-to-saved-filter-groups "default")))

(setq ibuffer-expert t)
(setq ibuffer-show-empty-filter-groups nil) ;; hide empty groups

(global-set-key (kbd "C-x C-b") 'ibuffer) ;; Use Ibuffer for Buffer List

;; settings for ido
(require 'ido)
(require 'ido-hacks)
(ido-mode t)

;; enable fuzzy matching
(setq ido-enable-flex-matching t)

;; show choices vertically
(setq ido-decorations (quote ("\n> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))

;; truncate long lines in choices
(defun my-ido-minibuffer-setup-hook ()
  ;; allow line wrapping in the minibuffer
  (set (make-local-variable 'truncate-lines) nil))
(add-hook 'ido-minibuffer-setup-hook 'my-ido-minibuffer-setup-hook)

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
  (define-key ido-completion-map [up] 'ido-prev-match)
  (define-key ido-completion-map [down] 'ido-next-match)
  (define-key ido-completion-map [(control n)] 'ido-next-match)
  (define-key ido-completion-map [(control p)] 'ido-prev-match))
(add-hook 'ido-setup-hook 'my-ido-keys)
