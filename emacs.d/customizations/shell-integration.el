;; Sets up exec-path-from shell
;; https://github.com/purcell/exec-path-from-shell
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-envs
   '("PATH")))

;; copy important environment variables from the user shell
(exec-path-from-shell-initialize)
(exec-path-from-shell-copy-env "AWS_ACCESS_KEY_ID")
(exec-path-from-shell-copy-env "AWS_SECRET_ACCESS_KEY")
(exec-path-from-shell-copy-env "SPARK_HOME")
(exec-path-from-shell-copy-env "PYTHONPATH")

;; emacs terminal conf
(setq system-uses-terminfo nil)

(defun create-or-show-small-terminal ()
  (interactive)
  (split-window-right -100)
  (windmove-right)
  (if (or (not (boundp 'current-terminal-buffer))
          (not (get-buffer current-terminal-buffer)))
      (progn (multi-term)
             (linum-mode -1)
             (setq current-terminal-buffer (buffer-name)))
    (switch-to-buffer (get-buffer current-terminal-buffer))))
