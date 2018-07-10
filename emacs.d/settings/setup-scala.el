(require 'ensime)
(require 'repl-utils)
(setq ensime-startup-notification nil)
(setq ensime-startup-snapshot-notification nil)

(defun get-subproject (path)
  "See if the file in PATH is within a subproject. Return the
name of the subproject if true."
  (let ((base-dir (get-base-dir path)))
    (when (not (member ".git" (directory-files base-dir)))
      (car (last (split-string base-dir "/") 2)))))

(defun eval-scala-buffer ()
  "will evaluate entire buffer"
  (interactive)
  (let ((prefix
         (if-let ((subproject (get-subproject default-directory)))
             (concat subproject "/") "")))
    (send-buffer-region-to-repl "scala-repl" `("sbt" "-J-Xms3G" "-J-Xmx6G"
                                               ,(concat prefix "compile")
                                               ,(concat prefix "console")))))

(defun eval-scala-region ()
  (interactive)
  (let ((prefix
         (if-let ((subproject (get-subproject default-directory)))
             (concat subproject "/") "")))
    (send-line-region-to-repl "scala-repl" `("sbt" "-J-Xms3G" "-J-Xmx6G"
                                             ,(concat prefix "compile")
                                             ,(concat prefix "console")))))

(add-hook 'scala-mode-hook (lambda () (local-set-key (kbd "C-c C-e") 'eval-scala-region)))
(add-hook 'scala-mode-hook (lambda () (local-set-key (kbd "C-c C-k") 'eval-scala-buffer)))

(provide 'setup-scala)
