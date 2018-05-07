(require 'repl-utils)

(defun eval-haskell-buffer ()
  "will evaluate entire buffer"
  (interactive)
  (send-buffer-region-to-repl "haskell-repl" '("ghci")))

(defun eval-haskell-region ()
  (interactive)
  (send-line-region-to-repl "haskell-repl" '("ghci")))

(add-hook 'haskell-mode-hook (lambda () (local-set-key (kbd "C-c C-e") 'eval-haskell-region)))
(add-hook 'haskell-mode-hook (lambda () (local-set-key (kbd "C-c C-k") 'eval-haskell-buffer)))

(provide 'setup-haskell)
