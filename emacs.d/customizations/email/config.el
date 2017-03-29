;; mu4e config
(add-to-list 'load-path "~/src/djcb/mu-0.9.9.5/mu4e")
(require 'mu4e)

(setq mu4e-mu-binary "/usr/local/bin/mu")
(setq mu4e-compose-dont-reply-to-self t)


(setq user-mail-address "mattusifer@gmail.com")
(setq user-full-name "Matt Usifer")

;; refresh mail dir every 2 minutes
(defun refresh-mail ()
  (start-process-shell-command
   "email-refresh" nil
   "mbsync personal-gmail && pkill -2 -u $UID mu && sleep 1 && mu index"))
(run-with-timer 0 (* 2 60) 'refresh-mail)
