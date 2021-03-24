(setq assume-role-buffer "*assume role*")

(defun set-aws-keys (process signal)
  (when (when (memq (process-status process) '(exit signal))
          (with-current-buffer (get-buffer assume-role-buffer)
            (-each (-map (lambda (s) (split-string s))
                         (-remove (lambda (s) (or (s-starts-with? "MFA" s) (s-blank? s)))
                                  (split-string (buffer-string) "\n")))
              (lambda (elements)
                (cl-destructuring-bind (name val) elements
                  (progn (setenv (s-chomp name) (s-chomp val))))))))))

(defun assume-admin-role (account)
  "Change the aws role to an admin in the given ACCOUNT"
  (let* ((output-buffer (generate-new-buffer assume-role-buffer))
         (proc (progn (async-shell-command (format "assume-role %s admin emacs" account) output-buffer)
                      (get-buffer-process output-buffer))))
    (switch-to-buffer output-buffer)
    (if (process-live-p proc)
        (set-process-sentinel proc #'set-aws-keys)
      (message "No process running."))))

(global-set-key (kbd "C-c b n") (lambda () (interactive) (assume-admin-role "non-prod")))

(provide 'setup-blackfynn)
