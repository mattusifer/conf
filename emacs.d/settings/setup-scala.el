(require 'ensime)
(require 'repl-utils)
(setq ensime-startup-notification nil)

(defun eval-scala-buffer ()
  "will evaluate entire buffer"
  (interactive)
  (send-buffer-region-to-repl "scala-repl" '("sbt" "-J-Xms3G" "-J-Xmx6G" "compile" "console")
                              'scala-mode))

(defun eval-scala-region ()
  (interactive)
  (send-line-region-to-repl "scala-repl" '("sbt" "-J-Xms3G" "-J-Xmx6G" "compile" "console")
                            'scala-mode))

(add-hook 'scala-mode-hook (lambda () (local-set-key (kbd "C-c C-e") 'eval-scala-region)))
(add-hook 'scala-mode-hook (lambda () (local-set-key (kbd "C-c C-k") 'eval-scala-buffer)))
 
(provide 'setup-scala)
