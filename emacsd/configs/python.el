
(setq-default flymake-gui-warnings-enabled nil)
(require 'flymake)
(load-library "flymake-cursor")

(defun flymake-create-temp-intemp (file-name prefix)
  "Return file name in temporary directory for checking FILE-NAME.
This is a replacement for `flymake-create-temp-inplace'. The
difference is that it gives a file name in
`temporary-file-directory' instead of the same directory as
FILE-NAME.

For the use of PREFIX see that function.

Note that not making the temporary file in another directory
\(like here) will not if the file you are checking depends on
relative paths to other files \(for the type of checks flymake
makes)."
  (unless (stringp file-name)
    (error "Invalid file-name"))
  (or prefix
      (setq prefix "flymake"))
  (let* ((name (concat
                (file-name-nondirectory
                 (file-name-sans-extension file-name))
                "_" prefix))
         (ext  (concat "." (file-name-extension file-name)))
         (temp-name (make-temp-file name nil ext))
         )
    (flymake-log 3 "create-temp-intemp: file=%s temp=%s" file-name temp-name)
    temp-name))

(defun flymake-pyflakes-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-intemp))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list "pyflakes" (list local-file))))

(push '("\\.py\\'" flymake-pyflakes-init)
      flymake-allowed-file-name-masks)
(push '("could not compile '\\([^']+\\)':\\([0-9]+\\):\\(\n.*\\)" 1 2 nil nil)
      flymake-err-line-patterns)

(add-hook 'python-mode-hook
          (lambda ()
            ;; Activate flymake unless buffer is a tmp buffer for the interpreter
            (unless (eq buffer-file-name nil) (flymake-mode t))))

