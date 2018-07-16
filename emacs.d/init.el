;; lose the UI
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(global-linum-mode 0)

;; no splash screen
(setq inhibit-startup-message t)

;; let me handle (package-initialize)
(setq package--init-file-ensured t)

(setq vendor-dir
      (expand-file-name "vendor" user-emacs-directory))

(setq settings-dir
      (expand-file-name "settings" user-emacs-directory))

(setq settings-common-dir
      (expand-file-name "settings/common" user-emacs-directory))

(add-to-list 'load-path settings-dir)
(add-to-list 'load-path settings-common-dir)
(add-to-list 'load-path vendor-dir)

;; configure
(require 'setup-package)
(require 'setup-ui)

(require 'setup-yasnippet)

(require 'setup-shell)

;; I wrote these integrations when working in tech support to allow
;; emacs to communicate directly with Zendesk for ticket management
;; and Toggl for time logging. I don't use them anymore, but they
;; could be useful at some point. Who knows.
;; (require 'setup-org-agenda-api-integrations)

;; Though I no longer use emacs as a client for email or slack, I'm
;; keeping these here for reference.
;; (require 'setup-email)
;; (require 'setup-org)
;; (require 'setup-slack)

(require 'setup-magit)

;; language-specific configs
(require 'setup-elisp)
(require 'setup-clojure)
(require 'setup-javascript)
(require 'setup-sql)
(require 'setup-haskell)
(require 'setup-scala)
(require 'setup-java)
(require 'setup-rust)
(require 'setup-python)

;; Keep custom settings out of init.el
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; opening screen

;; dashboard
(require 'setup-dashboard)

;; ;; org agenda
;; (org-agenda-list)
;; (delete-other-windows)
