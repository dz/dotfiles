(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq ivy-height 12)
(setq ivy-wrap t)
(setq ivy-count-format "(%d/%d) ")

(setq ivy-display-style 'fancy)

;; custom ag command to always prompt for a dir
(defun dz-counsel-ag-prompt-dir (&optional initial-input initial-directory)
  "Grep for a string in the current directory using ag.
INITIAL-INPUT can be given as the initial minibuffer input."
  (interactive
   (list nil
         (when t
           (read-directory-name (concat
                                 (car (split-string counsel-ag-base-command))
                                 " in directory: ")))))
  (setq counsel--git-grep-dir (or initial-directory default-directory))
  (ivy-read (funcall counsel-prompt-function
                     (car (split-string counsel-ag-base-command)))
            'counsel-ag-function
            :initial-input initial-input
            :dynamic-collection t
            :history 'counsel-git-grep-history
            :action #'counsel-git-grep-action
            :unwind (lambda ()
                      (counsel-delete-process)
                      (swiper--cleanup))
            :caller 'counsel-ag))

;; custom ag command to use either the first .git dir it finds
;; or fall back to the current dir
(defun dz-counsel-ag-git-dir (&optional initial-input initial-directory)
  "Grep for a string in the current directory using ag.
INITIAL-INPUT can be given as the initial minibuffer input."
  (interactive
   (list nil
         (when current-prefix-arg
           (read-directory-name (concat
                                 (car (split-string counsel-ag-base-command))
                                 " in directory: ")))))
  (setq counsel--git-grep-dir (or (locate-dominating-file default-directory ".git") default-directory))
  (pwd)
  (ivy-read (funcall counsel-prompt-function
                     (car (split-string counsel-ag-base-command)))
            'counsel-ag-function
            :initial-input initial-input
            :dynamic-collection t
            :history 'counsel-git-grep-history
            :action #'counsel-git-grep-action
            :unwind (lambda ()
                      (counsel-delete-process)
                      (swiper--cleanup))
            :caller 'counsel-ag))

(defun ivy-imenu-get-candidates-from (alist  &optional prefix)
  (cl-loop for elm in alist
           nconc (if (imenu--subalist-p elm)
                     (ivy-imenu-get-candidates-from
                      (cl-loop for (e . v) in (cdr elm) collect
                               (cons e (if (integerp v) (copy-marker v) v)))
                      (concat prefix (if prefix ".") (car elm)))
                   (and (cdr elm) ; bug in imenu, should not be needed.
                        (setcdr elm (copy-marker (cdr elm))) ; Same as [1].
                        (list (cons (concat prefix (if prefix ".") (car elm))
                                    (copy-marker (cdr elm))))))))

(defun ivy-imenu-goto ()
  "Go to buffer position"
  (interactive)
  (let ((imenu-auto-rescan t) items)
    (unless (featurep 'imenu)
      (require 'imenu nil t))
    (setq items (imenu--make-index-alist t))
    (ivy-read "imenu items:"
              (ivy-imenu-get-candidates-from (delete (assoc "*Rescan*" items) items))
              :action (lambda (k) (goto-char k)))))

(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "M-d") 'ivy-imenu-goto)
(global-set-key (kbd "M-e") 'counsel-git)
(global-set-key (kbd "M-E") 'counsel-git)
(global-set-key (kbd "M-o") 'counsel-find-file)
;; (global-set-key (kbd "M-F") 'counsel-git-grep)
(global-set-key (kbd "M-C-f") 'swiper)
(global-set-key (kbd "M-F") 'dz-counsel-ag-git-dir)
(global-set-key (kbd "M-G") 'dz-counsel-ag-prompt-dir)
(global-set-key (kbd "M-r") 'ivy-resume)
(global-set-key (kbd "M-b") 'ivy-switch-buffer)

;; ivy minibuffer keys
;; save results as occur lists
(define-key ivy-minibuffer-map (kbd "C-s") 'ivy-occur)
;; tab key should never open a file, just complete
(define-key ivy-minibuffer-map (kbd "TAB") 'ivy-partial)
;; return key completes partials or opens files
(define-key ivy-minibuffer-map (kbd "<return>") 'ivy-alt-done)
;; make this behave as normal
(define-key ivy-minibuffer-map (kbd "C-w") 'backward-kill-word)
;; make C-f and C-b work like everywhere else
(define-key ivy-minibuffer-map (kbd "C-f") 'ivy-scroll-up-command)
(define-key ivy-minibuffer-map (kbd "C-b") 'ivy-scroll-down-command)
