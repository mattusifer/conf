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

;; emacs terminal conf
(setq system-uses-terminfo nil)

(defun create-small-terminal ()
  (interactive)
  (split-window-right -100)
  (windmove-right)
  (multi-term)
  (linum-mode -1))
