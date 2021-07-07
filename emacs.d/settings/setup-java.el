(require 'cc-mode)

;; enable for serious java development, gets in the way otherwise.
(require 'lsp-java)
(add-hook 'java-mode-hook #'lsp)

(provide 'setup-java)
