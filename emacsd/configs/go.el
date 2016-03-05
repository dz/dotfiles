(env-var-import '("GOROOT" "GOPATH"))

(defun go-mode-setup ()
  (setq compile-command "go build -v && go test -v && go vet")
  (define-key (current-local-map) "\C-c\C-c" 'compile)
  (go-eldoc-setup)
  (setq gofmt-command "goimports")
  (load-file "$GOPATH/src/golang.org/x/tools/cmd/oracle/oracle.el")
  (add-hook 'before-save-hook 'gofmt-before-save))

(add-hook 'go-mode-hook 'go-mode-setup)
