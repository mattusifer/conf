;; This file contains the following integrations for org agenda mode:
;; - Start and stop Toggl entries when clocking in and out (respectively)
;; - Navigate directly to a Zendesk url parsed out of the entry

(defun get-string-from-file (filePath)
  "Return filePath's file content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

;; returns the index of the first instance of char in str
(defun index-of (str substr)
  (setq iterator 0)
  (setq substr-counter 0)
  (setq final-index -1)
  (setq not-found t)
  (while (and (< iterator (length str))
              not-found)
    (if (equal (char-to-string (aref substr substr-counter)) 
               (char-to-string (aref str iterator)))
        (if (equal substr-counter (- (length substr) 1))
            (if (equal (length substr) 1)
                (progn 
                  (setq final-index iterator)
                  (setq not-found nil))
              (setq not-found nil))
          (if (eq substr-counter 0)
              (progn
                (setq final-index iterator)
                (setq substr-counter (1+ substr-counter))
                (setq iterator (1+ iterator)))
            (progn
              (setq substr-counter (1+ substr-counter))
              (setq iterator (1+ iterator)))))
      (if (equal substr-counter 0)
          (progn
            (setq final-index -1)
            (setq iterator (1+ iterator)))
        (progn
          (setq final-index -1)
          (setq substr-counter 0)))))

  final-index)

(defun refresh-agenda ()
  (interactive)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'org-agenda-mode)
        (org-agenda-redo t)))))

(require 'real-auto-save)
(add-hook 'org-mode-hook 'real-auto-save-mode)
(setq real-auto-save-interval 1)

(global-auto-revert-mode 1)
(add-hook 'after-revert-hook 'refresh-agenda)

(defun toggl-start ()
  (interactive)
  (org-clock-goto)
  (let ((line (thing-at-point 'line t))
        (line-arr (split-string (thing-at-point 'line t) " :: "))
        (token (get-string-from-file "~/automations/toggl-logger/token")))
    (when (equal (buffer-name) "ticket.org")
      (setq toggl-entry-id 
            (replace-regexp-in-string 
             "\n" ""
             (shell-command-to-string (concat "java -jar ~/automations/toggl-logger/toggl-logger.jar --start " 
                                              (substring (car line-arr) (max (+ (index-of (car line-arr) "TODO") 5)
                                                                             (+ (index-of (car line-arr) "DONE") 5)
                                                                             (+ (index-of (car line-arr) "#") 4))) 
                                              " -d https://rjmetrics.zendesk.com/agent/tickets/" 
                                              (car (cdr line-arr))
                                              " -n '" (car (cdr (cdr line-arr)))
                                              "' -g 'Client support'"
                                              " -t " token))))
      (print (concat "New toggl entry id: " toggl-entry-id))))
  ;; switch to agenda
  (switch-to-prev-buffer))

(defun toggl-stop ()
  (interactive)
  (let ((token (get-string-from-file "~/automations/toggl-logger/token")))
    (when toggl-entry-id
      (shell-command (concat "java -jar ~/automations/toggl-logger/toggl-logger.jar --stop " toggl-entry-id " -t " token))
      (setq toggl-entry-id nil))))

(defun go-to-zendesk-url ()
  (interactive)
  (let ((line-arr (split-string (thing-at-point 'line t) " :: ")))
    (when (>= (length line-arr) 3)
      (browse-url (concat "https://rjmetrics.zendesk.com/agent/tickets/" (car (cdr line-arr)))))))

(defun go-to-admin-url ()
  (interactive)
  (let ((line-arr (split-string (thing-at-point 'line t) " :: ")))
    (when (>= (length line-arr) 3)
      (browse-url (concat "https://admin.rjmetrics.com/admin/v3b/client/" 
                          (substring (car line-arr) (max (+ (index-of (car line-arr) "TODO") 5)
                                                         (+ (index-of (car line-arr) "DONE") 5)
                                                         (+ (index-of (car line-arr) "#") 4))) "/table/")))))

(add-hook 'org-clock-in-hook 'toggl-start)
(add-hook 'org-clock-out-hook 'toggl-stop)

(defun map-org-mode-keys ()
  (local-set-key (kbd "C-c TAB") 'go-to-zendesk-url)
  (local-set-key (kbd "C-x TAB") 'go-to-admin-url))

(add-hook 'org-agenda-mode-hook 'map-org-mode-keys)

(provide 'setup-org-agenda-api-integrations)
