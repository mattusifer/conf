;;;;
;; Packages
;;;;

;; Define package repositories
(require 'package)
(package-initialize)

(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("tromey" . "http://tromey.com/elpa/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; refresh if needed
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages
  '(better-defaults
    use-package
    ido-ubiquitous
    smex
    magit
    auto-complete
    company

    ;; clojure/lisp
    paredit
    clojure-mode
    clojure-mode-extra-font-locking
    cider
    rainbow-delimiters

    ;; project navigation
    projectile
    neotree
    dired-details

    ;; html
    tagedit

    ;; misc editing
    ace-jump-mode
    undo-tree
    real-auto-save
    multiple-cursors
    expand-region
    column-marker

    ;; python
    python-mode
    elpy

    ;; coffeescript
    coffee-mode

    ;; java
    eclim
    ac-emacs-eclim
    company-emacs-eclim

    ;; sql
    sql-indent

    ;; php
    php-mode
    
    ;; javascript
    js2-mode

    ;; scss
    scss-mode

    ;; scala
    scala-mode
    ensime
    
    ;; markdown
    markdown-mode
    markdown-preview-mode

    ;; shell
    exec-path-from-shell
    multi-term

    ;; emacs-slack deps
    oauth2
    lui
    request
    alert
    websocket
    circe
    emojify
    ))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; source from any downloaded elisp files found in ~/.emacs.d/vendor
(add-to-list 'load-path "~/.emacs.d/vendor")

;; projectile -- enable caching
(setq projectile-enable-caching t)

;;;;
;; Customization
;;;;

;; set load path
(add-to-list 'load-path "~/.emacs.d/customizations")

;; email
;; requires 'mu' to be installed on the system
(load "email/config.el")

;; setup exec-path-from-shell so that Emacs will use the correct
;; environment variables
(load "shell-integration.el")

;; agenda api integrations
(load "org-agenda-api-integrations.el")

;; git
(load "magit/config.el")

;; configure emacs
(load "navigation.el")
(load "ui.el")
(load "editing.el")
(load "windows.el")
(load "misc.el")

;; pipe into emacs
(load "tty-format.el")
(load "e-sink.el")

;; auto-complete
(require 'company)
(require 'cl)
(require 'auto-complete-config)
(ac-config-default)
(global-company-mode t)

;; clean up dired
(setq-default dired-details-hidden-string "--- ")

;; Langauage-specific
(load "language/elisp.el")
(load "language/clojure.el")
(load "language/javascript.el")
(load "language/sql.el")
(load "language/scala.el")
(load "language/java.el")
(load "language/python.el")

;; slack
(add-to-list 'load-path "~/.emacs.d/customizations/slack/emacs-slack/")
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

; lose the UI
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(global-linum-mode 0)

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
