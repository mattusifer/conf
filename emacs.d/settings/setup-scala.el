(require 'ensime)
(require 'repl-utils)
(setq ensime-startup-notification nil)
(setq ensime-startup-snapshot-notification nil)

;; remove ensime highlighting for implicit conversion
(setq ensime-sem-high-faces
      '((implicitConversion nil)
        (implicitParams nil)))
(setq ensime-implicit-gutter-icons nil)

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
    (send-buffer-region-to-repl-buffer "*scala-repl*"
                                       (format "sbt -J-Xms3G -J-Xmx6G -J-XX:MaxMetaspaceSize=2G %s %s"
                                               (concat prefix "compile")
                                               (concat prefix "console")))))

(defun eval-scala-region ()
  (interactive)
  (let ((prefix
         (if-let ((subproject (get-subproject default-directory)))
             (concat subproject "/") "")))
    (send-line-region-to-repl-buffer "*scala-repl*"
                                     (format "sbt -J-Xms3G -J-Xmx6G -J-XX:MaxMetaspaceSize=2G %s %s"
                                             (concat prefix "compile")
                                             (concat prefix "console")))))

(defun open-sbt ()
  (interactive)
  (when (> (window-total-width) 200)
    (split-window-horizontally)
    (other-window 1))
  (sbt:run-sbt nil t))

(defun ensime-sbt-do-fmt ()
  (interactive)
  (sbt:command "fmt"))

(bind-key "C-c C-v F" 'ensime-sbt-do-fmt scala-mode-map)

(defun open-scala-scratch-buffer ()
  (interactive)
  (if (one-window-p)
      (split-window-horizontally))
  (other-window 1)
  (switch-to-buffer "scala-scratch")
  (scala-mode)
  (setq default-directory "/"))

(add-hook 'scala-mode-hook (lambda () (local-set-key (kbd "C-c C-e") 'eval-scala-region)))
(add-hook 'scala-mode-hook (lambda () (local-set-key (kbd "C-c C-k") 'eval-scala-buffer)))
(add-hook 'scala-mode-hook (lambda () (local-set-key (kbd "C-c C-p") 'open-sbt)))
(add-hook 'scala-mode-hook (lambda () (local-set-key (kbd "C-c C-s") 'open-scala-scratch-buffer)))

(provide 'setup-scala)
