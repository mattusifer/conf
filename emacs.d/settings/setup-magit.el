(require 'magit)

;; ignore whitespace in diffs
(defun magit-toggle-whitespace ()
  (interactive)
  (if (member "-w" magit-diff-arguments)
      (magit-dont-ignore-whitespace)
    (magit-ignore-whitespace)))

(defun magit-ignore-whitespace ()
  (interactive)
  (add-to-list 'magit-diff-arguments "-w")
  (magit-refresh))

(defun magit-dont-ignore-whitespace ()
  (interactive)
  (setq magit-diff-arguments (remove "-w" magit-diff-arguments))
  (magit-refresh))

(define-key magit-status-mode-map (kbd "W") 'magit-toggle-whitespace)

;; full screen magit status
(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(defun get-github-url ()
  (let ((upstream-url (magit-get "remote" "upstream" "url"))
        (origin-url (magit-get "remote" "origin" "url")))
    (flet ((get-url (url)
                    (concat "https://www.github.com/"
                            (car (split-string
                                  (car (cdr (split-string
                                             url ":"))) "\\\.")))))
      (if (null upstream-url)
          (if (null origin-url)
              (message "No 'upstream' or 'origin' git remote is set on this repo")
            (get-url origin-url))
        (get-url upstream-url)))))

(defun pop-open-github-repo ()
  (interactive)
  (browse-url (get-github-url)))

(global-set-key (kbd "C-c m s") 'magit-status)
(global-set-key (kbd "C-c m p") 'magit-push)
(global-set-key (kbd "C-c m o") 'pop-open-github-repo)

(provide 'setup-magit)
