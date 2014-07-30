;;; helm-itunes-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "helm-itunes" "helm-itunes.el" (21465 20997
;;;;;;  0 0))
;;; Generated autoloads from helm-itunes.el

(defvar helm-source-itunes-search '((name . "iTunes Search") (volatile) (delayed . 1) (multiline) (requires-pattern . 2) (candidates-process . helm-itunes-helm-search) (action ("Play Track" . helm-itunes-play-track))))

(autoload 'helm-itunes "helm-itunes" "\
Bring up a Spotify search interface in helm.

\(fn)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; helm-itunes-autoloads.el ends here
