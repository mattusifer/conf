(require 'repl-utils)
(require 'company-lsp)
(require 'lsp-scala)

(defun get-subproject (path)
  "Confirm that the file in PATH is within a subproject.
Return the name of the subproject if true."
  (let ((base-dir (get-base-dir path)))
    (when (not (member "build.sbt" (directory-files base-dir)))
      (car (last (split-string base-dir "/") 2)))))

(defun eval-scala-buffer ()
  "Evaluate entire buffer."
  (interactive)
  (let ((prefix
         (if-let ((subproject (get-subproject default-directory)))
             (concat subproject "/") "")))
    (send-buffer-region-to-repl-buffer "*scala-repl*"
                                       (format "sbt -J-Xms3G -J-Xmx6G -J-XX:MaxMetaspaceSize=2G %s %s"
                                               (concat prefix "test:compile")
                                               (concat prefix "test:console")) nil "build.sbt")))

(defun eval-scala-region ()
  (interactive)
  (let ((prefix
         (if-let ((subproject (get-subproject default-directory)))
             (concat subproject "/") "")))
    (send-line-region-to-repl-buffer "*scala-repl*"
                                     (format "sbt -J-Xms3G -J-Xmx6G -J-XX:MaxMetaspaceSize=2G %s %s"
                                             (concat prefix "test:compile")
                                             (concat prefix "test:console")) nil "build.sbt")))

(defun open-sbt ()
  (interactive)
  (when (> (window-total-width) 200)
    (split-window-horizontally)
    (other-window 1))
  (sbt:run-sbt nil t))

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

;; format buffer on save
(defun format-scala-buffer-with-saved-position
    ()
  (when (not (s-ends-with? ".sbt" buffer-file-name))
    (let ((w-start (window-start)))
      (lsp-format-buffer)
      (set-window-start (selected-window) w-start))))
(add-hook 'scala-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c f") #'format-scala-buffer-with-saved-position)))
(add-hook 'scala-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'format-scala-buffer-with-saved-position nil t)))

(add-hook 'scala-mode-hook 'lsp)
(add-hook 'scala-mode-hook 'company-mode)
(setq lsp-prefer-flymake nil)

(push 'company-lsp company-backends)

(provide 'setup-scala)
