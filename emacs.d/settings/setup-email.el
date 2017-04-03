(require 'mu4e)

;; various settings
(setq mu4e-mu-binary (expand-file-name "vendor/mu/mu/mu" user-emacs-directory)
      mu4e-compose-dont-reply-to-self t

      ;; retrieval
      mu4e-get-mail-command "mbsync -V personal-gmail || [ $? -eq 1 ]"
      mu4e-update-interval  600 ;; seconds

      ;; speed
      ;; mu4e-index-cleanup nil      ;; don't do a full cleanup check
      ;; mu4e-index-lazy-check t     ;; don't consider up-to-date dirs

      ;; maildir
      mu4e-maildir       "~/.mbsyncmaildir/personal-gmail"  ;; top-level Maildir
      mu4e-inbox-folder   "/inbox"                          ;; folder for sent messages
      mu4e-sent-folder   "/sent"                            ;; folder for sent messages
      mu4e-drafts-folder "/drafts"                          ;; unfinished messages
      mu4e-trash-folder  "/trash"                           ;; trashed messages
      mu4e-refile-folder "/all"                             ;; saved messages

      user-mail-address "mattusifer@gmail.com"
      user-full-name "Matt Usifer"

      ;; sent mail
      mu4e-sent-messages-behavior 'delete
      message-send-mail-function 'smtpmail-send-it
      smtpmail-smtp-server "smtp.gmail.com"

      ;; attachments
      mu4e-attachment-dir  "~/Downloads"
      mu4e-view-show-images t

      ;; html
      mu4e-view-prefer-html t
      mu4e-html2text-command "textutil -stdin -format html -convert txt -stdout || html2text -utf8 -width 72"
      )

;; bookmarks example
;; (add-to-list 'mu4e-bookmarks
;;   (make-mu4e-bookmark
;;     :name  "Big messages"
;;     :query "size:5M..500M"
;;     :key ?b))

;; start email in the background
(mu4e t)

(provide 'setup-email)
