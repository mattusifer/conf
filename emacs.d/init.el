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

    ;; makes handling lisp expressions much, much easier
    ;; Cheatsheet: http://www.emacswiki.org/emacs/PareditCheatsheet
    paredit

    ;; key bindings and code colorization for Clojure
    ;; https://github.com/clojure-emacs/clojure-mode
    clojure-mode

    ;; extra syntax highlighting for clojure
    clojure-mode-extra-font-locking

    ;; integration with a Clojure REPL
    ;; https://github.com/clojure-emacs/cider
    cider

    ;; allow ido usage in as many contexts as possible. see
    ;; customizations/navigation.el line 23 for a description
    ;; of ido
    ido-ubiquitous

    ;; Enhances M-x to allow easier execution of commands. Provides
    ;; a filterable list of possible commands in the minibuffer
    ;; http://www.emacswiki.org/emacs/Smex
    smex

    ;; project navigation
    projectile
    neotree

    ;; auto save
    real-auto-save

    ;; colorful parenthesis matching
    rainbow-delimiters

    ;; edit html tags like sexps
    tagedit

    ;; git integration
    magit

    ;; ace-jump-mode
    ace-jump-mode

    ;; multiple-cursors
    multiple-cursors

    ;; expand region
    expand-region

    ;; column marker
    column-marker

    ;; python
    python-mode
    elpy

    ;; coffeescript
    coffee-mode

    ;; java
    emacs-eclim

    ;; auto-complete
    auto-complete
    company

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
    ))

;; copy important environment variables from the user shell
(exec-path-from-shell-initialize)
(exec-path-from-shell-copy-env "AWS_ACCESS_KEY_ID")
(exec-path-from-shell-copy-env "AWS_SECRET_ACCESS_KEY")

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

;; setup exec-path-from-shell so that Emacs will use the correct
;; environment variables
(load "shell-integration.el")

;; agenda api integrations
(load "org-agenda-api-integrations.el")

;; configure emacs
(load "navigation.el")
(load "ui.el")
(load "editing.el")
(load "misc.el")

;; pipe into emacs
(server-start)
(load "tty-format.el")
(load "e-sink.el")

;; auto-complete
(require 'company)
(require 'cl)
(require 'auto-complete-config)
(ac-config-default)
(global-company-mode t)

;; Langauage-specific
(load "language/elisp.el")
(load "language/clojure.el")
(load "language/javascript.el")
(load "language/sql.el")
(load "language/scala.el")
(load "language/java.el")
(load "language/python.el")

;;;;
;; Custom Key Mappings
;;;;

(global-set-key (kbd "C-x C-m") 'execute-extended-command)
(global-set-key (kbd "C-c C-m") 'execute-extended-command)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C-c M-SPC") 'ace-jump-mode)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-S-c C->") 'mc/mark-all-like-this)
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-d") 'kill-word)
(global-set-key (kbd "C-x C-k") 'kill-region)
(global-set-key (kbd "C-c C-k") 'kill-region)
(global-set-key (kbd "C-c a") 'org-agenda) 
(global-set-key (kbd "C-c f") 'neotree)
(global-set-key (kbd "C-c m s") 'magit-status)
(global-set-key (kbd "C-c m p") 'magit-push)
(global-set-key (kbd "C-}") 'paredit-forward-barf-sexp)
(global-set-key (kbd "C-{") 'paredit-backward-barf-sexp)

;; swap super and meta on OSX
(setq mac-option-modifier 'super)
(setq mac-command-modifier 'meta)

;; scrolling line-by-line
(setq scroll-step            1
      scroll-conservatively  10000)

;;;;
;; On-load Customizations
;;;;

; lose the UI
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; agenda config
(setq org-agenda-files (remove-if-not 'file-exists-p
                                      (list "~/Dropbox/symlinks/emacs/org-mode/work.org"
                                            "~/Dropbox/symlinks/emacs/org-mode/home.org")))
;; notifications
(org-agenda-to-appt)

;; show agenda
(org-agenda-list)
(delete-other-windows)

(setq frame-resize-pixelwise t)

;; show neotree
;; (neotree)

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
 '(coffee-tab-width 2))
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
