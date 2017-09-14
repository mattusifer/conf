(when (eq system-type 'darwin)
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-envs
   '("PATH" "AWS_ACCESS_KEY_ID" "AWS_SECRET_ACCESS_KEY" "SPARK_HOME" "PYTHONPATH")
   ))

;; emacs terminal conf
(setq system-uses-terminfo nil)

;; todo: setup SSH agent

;; eshell
(add-to-list 'eshell-visual-commands "htop")

(defun create-or-show-small-terminal ()
  "Pop open a terminal"
  (interactive)
  (if (> (window-total-width) 200)
      (progn
        (split-window-horizontally)
        (windmove-right)))
  (if (or (not (boundp 'current-terminal-buffer))
          (not (get-buffer current-terminal-buffer)))
      (progn (multi-term)
             (linum-mode -1)
             (setq comint-move-point-for-output nil)
             (setq comint-scroll-show-maximum-output nil)
             (setq current-terminal-buffer (buffer-name)))
    (switch-to-buffer (get-buffer current-terminal-buffer))))

(global-set-key (kbd "C-c C-j") 'term-line-mode)
(global-set-key (kbd "C-c t c") 'create-or-show-small-terminal)


(require 'send-to-process)

(defun send-line-to-terminal ()
  (interactive)
  (send-line-region-to-process "terminal<1>"))

(defun send-buffer-to-terminal ()
  (interactive)
  (send-buffer-region-to-process "terminal<1>"))

(global-set-key (kbd "C-c t e") 'send-line-to-terminal)
(global-set-key (kbd "C-c t k") 'send-buffer-to-terminal)

(setq-default sh-basic-offset 2)
(setq-default sh-indentation 2)

(provide 'setup-shell)
