(require 'rails)

;; really need brackets in ruby code you guys
(add-hook 'ruby-mode-hook
          '(lambda ()
             (abbrev-mode 1)
             (electric-pair-mode t)
             (electric-indent-mode t)
             (electric-layout-mode t)))

(eval-after-load 'ruby-mode
  '(progn
     ;; work around possible elpa bug
     (ignore-errors (require 'ruby-compilation))
     (setq ruby-deep-indent-paren nil)
     (setq ruby-use-encoding-map nil)
     (setq ruby-insert-encoding-magic-comment nil)
     (define-key ruby-mode-map (kbd "RET") 'reindent-then-newline-and-indent)))

(add-hook 'ruby-mode-hook 'highlight-indentation-mode)

(setq ruby-insert-encoding-magic-comment nil)


;; This allows indentation like:
;; object.method(
;;   arg1
;; )
;; when ruby-deep-indent-paren is nil
(defadvice ruby-indent-line (after unindent-closing-paren activate)
  (if (eq ruby-deep-indent-paren nil)
      (let ((column (current-column))
            indent offset)
        (save-excursion
          (back-to-indentation)
          (let ((state (syntax-ppss)))
            (setq offset (- column (current-column)))
            (when (and (eq (char-after) ?\))
                       (not (zerop (car state))))
              (goto-char (cadr state))
              (setq indent (current-indentation)))))
        (when indent
          (indent-line-to indent)
          (when (> offset 0) (forward-char offset))))))

;; This allows indentation without parenthesis
;; object.method arg1,
;;               arg2
;; when ruby-deep-indent-paren is nil
;; object.method arg1,
;;   arg2
(defadvice ruby-indent-line (after line-up-args activate)
  (let (indent prev-indent arg-indent)
    (save-excursion
      (back-to-indentation)
      (when (zerop (car (syntax-ppss)))
        (setq indent (current-column))
        (skip-chars-backward " \t\n")
        (when (eq ?, (char-before))
          (ruby-backward-sexp)
          (back-to-indentation)
          (setq prev-indent (current-column))
          (skip-syntax-forward "w_.")
          (skip-chars-forward " ")
          (setq arg-indent (current-column)))))
    (when prev-indent
      (let ((offset (- (current-column) indent)))
        (cond ((< indent prev-indent)
               (indent-line-to prev-indent))
              ((= indent prev-indent)
               (if (eq ruby-deep-indent-paren nil)
                   (indent-line-to (+ prev-indent 2))
                 (indent-line-to arg-indent))))
        (when (> offset 0) (forward-char offset))))))

;; rinari
(add-to-list 'load-path "~/.emacs.d/vendor/rinari")
(add-to-list 'load-path "~/.emacs.d/vendor/rinari/util")
(require 'rinari)

(setq rinari-major-modes
      (list 'mumamo-after-change-major-mode-hook 'dired-mode-hook 'ruby-mode-hook
            'css-mode-hook 'yaml-mode-hook 'javascript-mode-hook))

(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rb\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru\\'" . ruby-mode))
(add-to-list 'completion-ignored-extensions ".rbc")

(setq
 indent-region-mode t)
(setq ruby-insert-encoding-magic-comment nil)
