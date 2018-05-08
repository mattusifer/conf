(require 'repl-utils)

(setq haskell-exec-fn '("ghci")
      haskell-repl-process-name "haskell-repl")

(defun add-newline ()
  (send-custom-string-to-repl "putStrLn \"\"" haskell-repl-process-name haskell-exec-fn) )

(defun eval-haskell-buffer ()
  "will evaluate entire buffer"
  (interactive)
  (send-custom-string-to-repl ":{" haskell-repl-process-name haskell-exec-fn)
  (send-buffer-region-to-repl haskell-repl-process-name haskell-exec-fn)
  (send-custom-string-to-repl ":}" haskell-repl-process-name haskell-exec-fn)
  (add-newline))

(defun eval-haskell-region ()
  (interactive)
  (send-custom-string-to-repl ":{" haskell-repl-process-name haskell-exec-fn)
  (send-line-region-to-repl haskell-repl-process-name haskell-exec-fn)
  (send-custom-string-to-repl ":}" haskell-repl-process-name haskell-exec-fn)
  (add-newline))

(add-hook 'haskell-mode-hook (lambda () (local-set-key (kbd "C-c C-e") 'eval-haskell-region)))
(add-hook 'haskell-mode-hook (lambda () (local-set-key (kbd "C-c C-k") 'eval-haskell-buffer)))

(provide 'setup-haskell)
