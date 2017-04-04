(dashboard-setup-startup-hook)

;; Set the title
(setq dashboard-banner-logo-title "EMACS!")
;; Set the banner
(setq dashboard-startup-banner 'official)

(setq dashboard-items '((recents  . 5)
                        (bookmarks . 5)
			(projects . 5)))

(provide 'setup-dashboard)
