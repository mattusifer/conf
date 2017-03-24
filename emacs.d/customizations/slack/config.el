(use-package slack
  :commands (slack-start)
  :init
  (setq slack-buffer-emojify t)
  (setq slack-prefer-current-team t)
  :config
  (slack-register-team
   :name "healthverity"
   :default t
   :client-id 
   (ignore-errors
     (string-trim (with-temp-buffer
             (insert-file-contents "~/.emacs.d/customizations/slack/client-id")
             (buffer-string))))
   :client-secret
   (ignore-errors
     (string-trim (with-temp-buffer
             (insert-file-contents "~/.emacs.d/customizations/slack/client-secret")
             (buffer-string))))

   ;; :token "159348907925.159378136869.76c288338c"
   ;; :subscribed-channels '(test-rename rrrrr)
   )
  )

(use-package alert
  :commands (alert)
  :init
  (setq alert-default-style 'message))

(global-set-key (kbd "C-c s g") 'slack-group-select)
(global-set-key (kbd "C-c s c") 'slack-channel-select)
(global-set-key (kbd "C-c s i") 'slack-im-select)

(defun close-buffer-and-window ()
  (interactive)
  (kill-buffer)
  (delete-window))

(add-hook 'slack-mode-hook (lambda () (local-set-key (kbd "C-c C-q") 'close-buffer-and-window)))
