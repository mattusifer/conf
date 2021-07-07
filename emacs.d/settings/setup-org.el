;; configure agenda files
(defun list-agenda-files (loc) (directory-files-recursively loc ".*\.org"))

(setq org-agenda-files-location "~/Seafile/Org/"
      org-agenda-files (list-agenda-files org-agenda-files-location))

(when (equal (system-name) "matts-mbp")
  (setq org-agenda-files (append org-agenda-files (list-agenda-files "~/Seafile/Etsy/"))))

;; always store relative paths for links
(setq org-link-file-path-type 'relative)

;; notify appts
(org-agenda-to-appt)

;; load commands for code blocks
(org-babel-do-load-languages 'org-babel-load-languages
    '(
      (shell . t)
      (sql . t)
      (R . t)
      (python . t)
    )
)

(provide 'setup-org)
