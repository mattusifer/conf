;; set up dependencies
(setq vendor-dir
      (expand-file-name "vendor" user-emacs-directory))

(setq settings-dir
      (expand-file-name "settings" user-emacs-directory))

(add-to-list 'load-path settings-dir)
(add-to-list 'load-path vendor-dir)

;; configure
(require 'setup-ui)
(require 'setup-package)

;; projectile -- enable caching
(setq projectile-enable-caching t)


(require 'setup-email)

(require 'setup-shell)

;; I wrote these integrations when working in tech support to allow
;; emacs to communicate directly with Zendesk for ticket management
;; and Toggl for time logging. I don't use them anymore, but they
;; could be useful at some point. Who knows.
;; (require 'setup-org-agenda-api-integrations)

(require 'setup-magit)

;; enable piping into emacs via:
;; $ cmd | esink
(require 'tty-format)
(require 'e-sink)

;; language-specific configs
(require 'setup-elisp.el)
(require 'setup-clojure.el)
(require 'setup-javascript.el)
(require 'setup-sql.el)
(require 'setup-scala.el)
(require 'setup-java.el)
(require 'setup-python.el)

;; slack
(add-to-list 'load-path "~/.emacs.d/settings/slack/emacs-slack/")
(load "slack/config.el")

;; backup files - save them elsewhere
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))
(setq vc-make-backup-files t)

;;;;
;; Custom Key Mappings
;;;;

;; ace jump
(global-set-key (kbd "C-c M-SPC") 'ace-jump-mode)

;; multi cursor
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-S-c C->") 'mc/mark-all-like-this)

;; killing
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-d") 'kill-word)
(global-set-key (kbd "C-x C-k") 'kill-region)
(global-set-key (kbd "C-c C-k") 'kill-region)
(global-set-key (kbd "C-c C-q")
               (lambda () (interactive) (kill-buffer) (delete-window)))
(global-set-key (kbd "C-c u") 'undo-tree-visualize)

;; agenda
(global-set-key (kbd "C-c a") 'org-agenda) 

;; terminal
(global-set-key (kbd "C-c t") 'create-or-show-small-terminal)

;; parediting
(global-set-key (kbd "C-}") 'paredit-forward-barf-sexp)
(global-set-key (kbd "C-{") 'paredit-backward-barf-sexp)

;; quicker movement
(global-set-key (kbd "C-S-n")
                (lambda ()
                  (interactive)
                  (ignore-errors (next-line 5))))
(global-set-key (kbd "C-S-p")
                (lambda ()
                  (interactive)
                  (ignore-errors (previous-line 5))))
(global-set-key (kbd "C-S-f")
                (lambda ()
                  (interactive)
                  (ignore-errors (forward-char 5))))
(global-set-key (kbd "C-S-b")
                (lambda ()
                  (interactive)
                  (ignore-errors (backward-char 5))))

;; webjump
(global-set-key (kbd "C-x g") 'webjump)


;; swap super and meta on OSX
(setq mac-option-modifier 'super)
(setq mac-command-modifier 'meta)

;; scrolling line-by-line
(setq scroll-step            1
      scroll-conservatively  10000)

;; show useless whitespace
(setq show-trailing-whitespace t)

;; agenda config
(setq org-agenda-files (remove-if-not 'file-exists-p
                                      (list "~/Dropbox/symlinks/emacs/org-mode/work.org"
                                            "~/Dropbox/symlinks/emacs/org-mode/home.org")))

;; undo tree
(global-undo-tree-mode)

;; notifications
(org-agenda-to-appt)

;; show agenda
(org-agenda-list)
(delete-other-windows)

(server-start)

(setq frame-resize-pixelwise t)

;;;;
;; Auto-generated stuff
;;;;

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'narrow-to-page 'disabled nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(coffee-tab-width 2)
 '(custom-safe-themes
   (quote
    ("6077e0de8ac8f10c8be7578c209bcfb6c5bbf0bd2be93a24cd74efae6aca520a" default)))
 '(eclim-eclipse-dirs (quote (eclipse-installation-dir)))
 '(eclim-executable (concat eclipse-installation-dir "eclim"))
 '(package-selected-packages
   (quote
    (use-package tagedit sql-indent smex scss-mode real-auto-save rainbow-delimiters python-mode projectile php-mode paredit neotree multiple-cursors multi-term markdown-preview-mode magit js2-mode ido-ubiquitous expand-region exec-path-from-shell ensime elpy company-emacs-eclim column-marker coffee-mode clojure-mode-extra-font-locking cider better-defaults ace-jump-mode ac-emacs-eclim)))
 '(send-mail-function (quote smtpmail-send-it))
 '(smtpmail-smtp-server "smtp.gmail.com")
 '(smtpmail-smtp-service 25))
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
