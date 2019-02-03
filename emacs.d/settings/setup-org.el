;; configure agenda files
(setq org-agenda-files
      (mapcar (lambda (x)  (concat "~/Seafile/Org Agenda/" x))
              (-filter (lambda (x) (s-suffix? ".org" x)) (directory-files "~/Seafile/Org Agenda"))))

;; notify appts
(org-agenda-to-appt)

(provide 'setup-org)
