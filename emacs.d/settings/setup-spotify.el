(require 'spotify)

(defun read-secret-key (path)
  (ignore-errors
    (string-trim
     (with-temp-buffer
       (insert-file-contents path)
       (buffer-string)))))

(setq spotify-oauth2-client-id
      (read-secret-key (expand-file-name "secrets/spotify-client-id" user-emacs-directory)))
(setq spotify-oauth2-client-secret
      (read-secret-key (expand-file-name "secrets/spotify-client-secret" user-emacs-directory)))

(provide 'setup-spotify)
