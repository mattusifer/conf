(add-hook 'go-mode-hook 'lsp-deferred)
(add-hook 'go-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'gofmt nil t)))


(provide 'setup-go)
