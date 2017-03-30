;; mu4e config
(add-to-list 'load-path "~/src/djcb/mu-0.9.9.5/mu4e")
(require 'mu4e)

(setq mu4e-mu-binary "/usr/local/bin/mu")
(setq mu4e-compose-dont-reply-to-self t)

; (setq mu4e-contexts
;    `( ,(make-mu4e-context
;          :name "Personal Gmail"
;          :enter-func (lambda () (mu4e-message "Entering Personal Gmail context"))
;          :leave-func (lambda () (mu4e-message "Leaving Personal Gmail context"))
;          ;; we match based on the contact-fields of the message
;          :match-func (lambda (msg)
;                        (when msg 
;                          (mu4e-message-contact-field-matches msg 
;                            :to "mattusifer@gmail.com")))
;          :vars '( ( user-mail-address      . "mattusifer@gmail.com"  )
;                   ( user-full-name         . "Matt Usifer" )
;                   ( mu4e-compose-signature . "- Matt")))
;       ))

;; set `mu4e-context-policy` and `mu4e-compose-policy` to tweak when mu4e should
;; guess or ask the correct context, e.g.

;; start with the first (default) context; 
;; default is to ask-if-none (ask when there's no context yet, and none match)
;; (setq mu4e-context-policy 'pick-first)

;; compose with the current context is no context matches;
;; default is to ask 
;; (setq mu4e-compose-context-policy nil)

;; refresh mail dir every 2 minutes
(defun refresh-mail ()
  (start-process-shell-command
   "email-refresh" nil
   "mbsync personal-gmail && pkill -2 -u $UID mu && sleep 1 && mu index"))
(run-with-timer 0 (* 2 60) 'refresh-mail)

;; keybindings

