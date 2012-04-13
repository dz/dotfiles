(require 'erc)
(require 'znc)
(require 'erc-nick-colors)

(defun growl (title message)
  (start-process "growl" " growl"
                 "/usr/local/bin/growlnotify"
                 title
                 "-a" "Emacs" "-m" message))

(defun my-erc-hook (match-type nick message)
  "Shows a growl notification, when user's nick was mentioned. If the buffer is currently not visible, makes it sticky."
  (unless (posix-string-match "^\\** *Users on #" message)
    (growl
     (concat (buffer-name (current-buffer)) ":")
     message
     )))
(add-hook 'erc-text-matched-hook 'my-erc-hook)

(setq erc-keywords '(("dz" (:background "salmon"))
                     ("insertinanename" (:background "salmon"))))

(setq erc-current-nick-highlight-type "all")
(setq erc-keyword-highlight-type "all")
(setq erc-notice-highlight-type "all")
(setq erc-pal-highlight-type "all")

;; change header line face if disconnected
(defface erc-header-line-disconnected
  '((t (:foreground "black" :background "indianred")))
  "Face to use when ERC has been disconnected.")

(defun erc-update-header-line-show-disconnected ()
  "Use a different face in the header-line when disconnected."
  (erc-with-server-buffer
    (cond ((erc-server-process-alive) 'erc-header-line)
          (t 'erc-header-line-disconnected))))

(setq erc-header-line-face-method 'erc-update-header-line-show-disconnected)

