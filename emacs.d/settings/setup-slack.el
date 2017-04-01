(require 'slack)

;; slack functions
(defun read-secret-key (path)
  (ignore-errors
    (string-trim
     (with-temp-buffer
       (insert-file-contents path)
       (buffer-string)))))

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
   :client-id (read-secret-key (expand-file-name "secrets/slack-client-id" user-emacs-directory))
   :client-secret (read-secret-key (expand-file-name "secrets/slack-client-secret" user-emacs-directory))
   )
  )
(use-package alert
  :commands (alert)
  :init
  (setq alert-default-style 'fringe))

;; key bindings
(global-set-key (kbd "C-c s g") 'slack-group-select)
(global-set-key (kbd "C-c s c") 'slack-channel-select)
(global-set-key (kbd "C-c s i") 'slack-im-select)
(global-set-key (kbd "C-c s a") 'slack-select-rooms)
(global-set-key (kbd "C-c s s") 'slack-select-rooms-with-unread-messages)

(provide 'setup-slack)
