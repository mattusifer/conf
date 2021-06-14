;; configure agenda files
(setq org-agenda-files
      (mapcar (lambda (x)  (concat "~/Seafile/seafile/Org Agenda/Org Agenda/" x))
              (-filter (lambda (x) (s-suffix? ".org" x)) (directory-files "~/Seafile/seafile/Org Agenda/Org Agenda"))))

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
