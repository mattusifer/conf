(require 'company-lsp)
(require 'company)
(push 'company-lsp company-backends)

(setq lsp-enable-file-watchers nil)
