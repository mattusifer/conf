(require 'magit)

;; ignore whitespace in diffs
(defun magit-toggle-whitespace ()
  (interactive)
  (if (member "-w" magit-diff-options)
      (magit-dont-ignore-whitespace)
    (magit-ignore-whitespace)))

(defun magit-ignore-whitespace ()
  (interactive)
  (add-to-list 'magit-diff-options "-w")
  (magit-refresh))

(defun magit-dont-ignore-whitespace ()
  (interactive)
  (setq magit-diff-options (remove "-w" magit-diff-options))
  (magit-refresh))

(define-key magit-status-mode-map (kbd "W") 'magit-toggle-whitespace)

;; full screen magit status
(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

;; pop open github 
(defun pop-open-github-repo ()
  (interactive)
  (let ((upstream-url (magit-get "remote" "upstream" "url")))
    (if (null upstream-url)
        (message "No 'upstream' git remote is set on this repo")
        (browse-url (concat "https://www.github.com/"
                            (car (split-string
                                  (car (cdr (split-string
                                             upstream-url ":"))) "\\\.")))))))

;; useful kbd's
(global-set-key (kbd "C-c m s") 'magit-status)
(global-set-key (kbd "C-c m p") 'magit-push)
(global-set-key (kbd "C-c m o") 'pop-open-github-repo)

(provide 'setup-magit)
