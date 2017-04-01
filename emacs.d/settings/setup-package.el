(require 'package)
(package-initialize)

;; build repo
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("tromey" . "http://tromey.com/elpa/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(setq load-prefer-newer t)

(defun packages-install (packages)
  "Install all packages in the list that haven't already been installed"
  (dolist (it packages)
    (when (not (package-installed-p it))
      (package-install it)))
  (delete-other-windows))

(defun install-custom-packages ()
  (packages-install
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
     helm
     ag

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

     ;; elisp
     multi

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

     ;; slack
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
(defun search-for-elisp-dir (dir level)
  (dolist (project (directory-files dir t "\\w+"))
    (when (file-directory-p project)
      (let ((project-lisp-files
             (delq nil
                   (mapcar (lambda (subdir) (string-match ".el$" subdir))
                           (directory-files project t "\\w+"))))
            (project-directories
             (delq nil
                   (mapcar (lambda (subdir)
                             (when (file-directory-p subdir)
                               subdir))
                           (directory-files project t "\\w+")))))
        (if (and (null project-lisp-files) (not (null project-directories)))
            (when (< level 2)
              (search-for-elisp-dir project (+ level 1)))
          (add-to-list 'load-path project))))))

;; begin search at vendor dir
(search-for-elisp-dir vendor-dir 1)

(provide 'setup-package)
