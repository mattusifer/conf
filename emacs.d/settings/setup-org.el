;; configure agenda files
(defun list-agenda-files (loc)
  (mapcar (lambda (x)  (concat loc x))
              (-filter (lambda (x) (s-suffix? ".org" x)) (directory-files loc))))

(setq org-agenda-files-location "~/Seafile/Org/"
      org-agenda-files (list-agenda-files org-agenda-files-location))

(when (equal (system-name) "9281")
  (setq org-agenda-files (append org-agenda-files (list-agenda-files "~/Seafile/Etsy/Notes/"))))

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
