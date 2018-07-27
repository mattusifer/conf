(require 'repl-utils)

(defun eval-gremlin-buffer ()
  "will evaluate entire buffer"
  (interactive)
  (send-buffer-region-to-repl-buffer "*gremlin-repl*" "gremlin" 'groovy-mode))

(defun eval-gremlin-region ()
  (interactive)
  (send-line-region-to-repl-buffer "*gremlin-repl*" "gremlin" 'groovy-mode))

(defun open-gremlin-scratch-buffer ()
  (interactive)
  (if (one-window-p)
      (split-window-horizontally))
  (other-window 1)
  (switch-to-buffer "gremlin-scratch")
  (groovy-mode))

(add-hook 'groovy-mode-hook (lambda () (local-set-key (kbd "C-c C-e") 'eval-gremlin-region)))
(add-hook 'groovy-mode-hook (lambda () (local-set-key (kbd "C-c C-k") 'eval-gremlin-buffer)))
(add-hook 'groovy-mode-hook (lambda () (local-set-key (kbd "C-c C-s") 'open-scratch-gremlin-buffer)))

(provide 'setup-gremlin)
