(setq shell-file-name "zsh")
(setenv "SHELL" shell-file-name)

(when (eq system-type 'darwin)
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-envs
   '("PATH" "AWS_ACCESS_KEY_ID" "AWS_SECRET_ACCESS_KEY" "SPARK_HOME" "PYTHONPATH" "BLACKFYNN_NEXUS_USER" "BLACKFYNN_NEXUS_PW")))

;; todo: setup SSH agent

(setq comint-process-echoes t)

(defun create-shell-buffer ()
  (vterm)
  (make-shell-buffer-current))

(defun make-shell-buffer-current ()
  (interactive)
  (setq current-terminal-buffer (buffer-name))
  (message (format "Current terminal set to %s" (buffer-name))))

(defun create-or-show-small-terminal-shell ()
  "Pop open a terminal. Will switch active buffer to current terminal buffer if it exists and is not visible."
  (interactive)
  (when (> (window-total-width) 200)
    (split-window-horizontally)
    (other-window 1))
  (if (or (not (boundp 'current-terminal-buffer))
          (not (get-buffer current-terminal-buffer))
          (and (boundp 'current-terminal-buffer)
               (get-buffer-window current-terminal-buffer)))
      (create-shell-buffer)
    (switch-to-buffer (get-buffer current-terminal-buffer))))

;; shell
(global-set-key (kbd "C-c t c") 'create-or-show-small-terminal-shell)
(global-set-key (kbd "C-c t m") 'make-shell-buffer-current)

(defun create-or-show-small-terminal-eshell ()
  "Pop open a terminal"
  (interactive)
  (if (> (window-total-width) 200)
      (progn
        (split-window-horizontally)
        (windmove-right)))
  (if (or (not (boundp 'current-terminal-buffer))
          (not (get-buffer current-terminal-buffer)))
      (progn (eshell)
             (make-shell-buffer-current))
    (switch-to-buffer (get-buffer current-terminal-buffer))))

(global-set-key (kbd "C-c t t") 'create-or-show-small-terminal-eshell)

;; colorize compilation buffer
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(require 'send-to-process)

(defun send-line-to-terminal ()
  (interactive)
  (if (or (not (boundp 'current-terminal-buffer))
          (not (get-buffer-window current-terminal-buffer)))
      (message "No current terminal open")
    (send-line-region-to-process current-terminal-buffer)))

(defun send-buffer-to-terminal ()
  (interactive)
  (if (or (not (boundp 'current-terminal-buffer))
          (not (get-buffer-window current-terminal-buffer)))
      (message "No current terminal open")
    (send-buffer-region-to-process current-terminal-buffer)))

(global-set-key (kbd "C-c t e") 'send-line-to-terminal)
(global-set-key (kbd "C-c t k") 'send-buffer-to-terminal)

(setq-default sh-basic-offset 2)
(setq-default sh-indentation 2)

(add-hook 'shell-mode-hook (lambda() (set (make-local-variable 'global-linum-mode) nil)))
(add-hook 'shell-mode-hook (lambda() (set (make-local-variable 'global-font-lock-mode) nil)))

(provide 'setup-shell)
