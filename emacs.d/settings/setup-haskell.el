(require 'repl-utils)

(setq haskell-exec-cmd "ghci"
      haskell-repl-buffer-name "*haskell-repl*")

(defun add-newline ()
  (send-custom-string-to-repl-buffer "putStrLn \"\"" haskell-repl-buffer-name haskell-exec-cmd))

(defun eval-haskell-buffer ()
  "will evaluate entire buffer"
  (interactive)
  (send-custom-string-to-repl-buffer ":{" haskell-repl-buffer-name haskell-exec-cmd)
  (send-buffer-region-to-repl-buffer haskell-repl-buffer-name haskell-exec-cmd)
  (send-custom-string-to-repl-buffer ":}" haskell-repl-buffer-name haskell-exec-cmd)
  (add-newline))

(defun eval-haskell-region ()
  (interactive)
  (send-custom-string-to-repl-buffer ":{" haskell-repl-buffer-name haskell-exec-cmd)
  (send-line-region-to-repl-buffer haskell-repl-buffer-name haskell-exec-cmd)
  (send-custom-string-to-repl-buffer ":}" haskell-repl-buffer-name haskell-exec-cmd)
  (add-newline))

(add-hook 'haskell-mode-hook (lambda () (local-set-key (kbd "C-c C-e") 'eval-haskell-region)))
(add-hook 'haskell-mode-hook (lambda () (local-set-key (kbd "C-c C-k") 'eval-haskell-buffer)))

(provide 'setup-haskell)
