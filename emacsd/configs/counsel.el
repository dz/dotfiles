(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq ivy-height 10)
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


(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "M-e") 'counsel-git)
(global-set-key (kbd "M-E") 'counsel-git)
(global-set-key (kbd "M-o") 'counsel-find-file)
;; (global-set-key (kbd "M-F") 'counsel-git-grep)
(global-set-key (kbd "M-F") 'counsel-ag)
(global-set-key (kbd "M-G") 'dz-counsel-ag-prompt-dir)
(global-set-key (kbd "M-r") 'ivy-resume)
(global-set-key (kbd "M-b") 'ivy-switch-buffer)
