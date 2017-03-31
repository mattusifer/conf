;; before anything else - lose the UI 
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(global-linum-mode 0)

;; no splash
(setq inhibit-startup-message t)

(require 'package)
(package-initialize)

(setq vendor-dir
      (expand-file-name "vendor" user-emacs-directory))

(setq settings-dir
      (expand-file-name "settings" user-emacs-directory))

(add-to-list 'load-path settings-dir)
(add-to-list 'load-path vendor-dir)

;; configure
(require 'setup-package)
(require 'setup-ui)

;; projectile -- enable caching
(setq projectile-enable-caching t)

(require 'setup-email)
(require 'setup-slack)

(require 'setup-shell)

;; I wrote these integrations when working in tech support to allow
;; emacs to communicate directly with Zendesk for ticket management
;; and Toggl for time logging. I don't use them anymore, but they
;; could be useful at some point. Who knows.
;; (require 'setup-org-agenda-api-integrations)

(require 'setup-magit)

;; enable piping into emacs via:
;; $ <cmd> | esink
(require 'tty-format)
(require 'e-sink)

;; language-specific configs
(require 'setup-elisp)
(require 'setup-clojure)
(require 'setup-javascript)
(require 'setup-sql)
(require 'setup-scala)
(require 'setup-java)
(require 'setup-python)

(server-start)

;; auto-generated stuff

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
