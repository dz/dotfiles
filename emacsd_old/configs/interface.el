;;set font
(setq my-font "Inconsolata-dz-15")
(set-frame-font my-font)
(add-hook 'after-make-frame-functions
          (lambda (cur-frame)
            (modify-frame-parameters cur-frame
                                     (list
                                      (cons 'font my-font)))))

(require 'color-theme)
(color-theme-initialize)
(load-file "~/.emacs.d/vendor/color-theme-irblack.el")
(color-theme-irblack)

(require 'framemove)
(setq framemove-hook-into-windmove t)

;; set larger line spacing
(setq-default line-spacing 2)

;; stop cursor blinking
(blink-cursor-mode 0)

;; always display cursor's column position
(column-number-mode 1)

;; change vertical border color
(set-face-attribute 'vertical-border nil :foreground "#666666")

;; no line highlighting
(global-hl-line-mode 0)

;; turn off fringe
(set-fringe-mode 0)

;; bold and italic and underline suck
(set-face-bold-p 'bold nil)
(set-face-italic-p 'italic nil)
(set-face-underline-p 'underline nil)


(defun smart-beginning-of-line ()
  "Move point to first non-whitespace character or beginning-of-line.

Move point to the first non-whitespace character on this line.
If point was already at that position, move point to beginning of line."
  (interactive)
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))

(global-set-key "\C-a" 'smart-beginning-of-line)

;; (global-set-key (kbd "<escape>") 'ignore)
;; disable because it interferes with vimpulse
