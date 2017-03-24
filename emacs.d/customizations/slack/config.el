;; slack functions
(defun read-secret-key (path)
  (ignore-errors
    (string-trim
     (with-temp-buffer
       (insert-file-contents path)
       (buffer-string)))))

(defun close-buffer-and-window ()
  (interactive)
  (kill-buffer)
  (delete-window))

;; slack config
(use-package slack
  :commands (slack-start)
  :init
  (setq slack-buffer-emojify t)
  (setq slack-prefer-current-team t)
  :config
  (slack-register-team
   :name "healthverity"
   :default t
   :client-id (read-secret-key "~/.emacs.d/customizations/slack/client-id")
   :client-secret (read-secret-key "~/.emacs.d/customizations/slack/client-secret")
   )
  )
(use-package alert
  :commands (alert)
  :init
  (setq alert-default-style 'message))

;; key bindings
(global-set-key (kbd "C-c s g") 'slack-group-select)
(global-set-key (kbd "C-c s c") 'slack-channel-select)
(global-set-key (kbd "C-c s i") 'slack-im-select)

(add-hook 'slack-mode-hook (lambda () (local-set-key (kbd "C-c C-q") 'close-buffer-and-window)))
