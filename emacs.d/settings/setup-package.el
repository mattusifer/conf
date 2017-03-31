;; build repo
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("tromey" . "http://tromey.com/elpa/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(defun packages-install (packages)
  "Install all packages in the list that haven't already been installed"
  (dolist (it packages)
    (when (not (package-installed-p it))
      (package-install it)))
  (delete-other-windows))

(defun install-custom-packages ()
  (packages-install
   '(
     ;; obvious ones
     better-defaults
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
     )))

;; install any packages that haven't been installed yet
(condition-case nil
    (install-custom-packages)
  (error
   (package-refresh-contents)
   (install-custom-packages)))

;; Add external projects to load path
(dolist (project (directory-files vendor-dir t "\\w+"))
  (when (file-directory-p project)
    (if (string= (car (last (split-string project "/"))) "mu")
        (add-to-list 'load-path (concat project "/mu4e"))
      (add-to-list 'load-path project))))

(provide 'setup-package)
