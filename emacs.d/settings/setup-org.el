;; configure agenda files
(setq org-agenda-files (list "~/Dropbox/symlinks/emacs/org-mode/work.org"
                             "~/Dropbox/symlinks/emacs/org-mode/home.org"
                             "~/Dropbox/symlinks/emacs/org-mode/calendar.org"))

;; refresh google calendar
(defun read-secret-key (path)
  (ignore-errors
    (string-trim
     (with-temp-buffer
       (insert-file-contents path)
       (buffer-string)))))

(defun refresh-calendar (url-location)
  (ignore-errors
    (let ((ical2org (expand-file-name "bin/ical2org.awk" user-emacs-directory))
          (ics-file (expand-file-name "hv-cal.ics" user-emacs-directory))
          (org-file "~/Dropbox/symlinks/emacs/org-mode/calendar.org"))

      (when (file-exists-p ics-file)
        (delete-file ics-file))

      (url-copy-file (read-secret-key url-location) ics-file)
      (start-process-shell-command (concat url-location " calendar-refresh")  nil
                                   (concat ical2org " < " ics-file " > " org-file))
      (delete-file ics-file))))

;; refresh healthverity calendar every 30 minutes
(run-with-timer 0 (* 60 30) 'refresh-calendar
                (expand-file-name "secrets/hv-calendar-url" user-emacs-directory))

;; notify appts
(org-agenda-to-appt)

(provide 'setup-org)
