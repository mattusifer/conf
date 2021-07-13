(require 'package)
(package-initialize)

;; build repo
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("elpa" . "https://elpa.gnu.org/packages/") t)

(setq load-prefer-newer t)

(defun packages-install (packages)
  "Install all packages in the list that haven't already been installed"
  (dolist (it packages)
    (when (not (package-installed-p it))
      (package-install it)))
  (delete-other-windows))

(defun install-custom-packages ()
  (packages-install
   '(use-package

     better-defaults
     ido-completing-read+
     smex
     company
     dashboard
     package-lint
     dash
     s
     async
     eyebrowse
     window-purpose
     all-the-icons
     dumb-jump

     ;; language server support
     lsp-mode

     ;; jsonnet
     jsonnet-mode

     ;; bazel
     bazel

     ;; docker
     dockerfile-mode

     ;; yaml
     yaml-mode

     ;; protobuf
     protobuf-mode

     ;; go
     go-mode

     ;; git
     magit
     magit-todos
     diff-hl

     ;; clojure/lisp
     paredit
     clojure-mode
     clojure-mode-extra-font-locking
     cider
     rainbow-delimiters

     ;; project navigation
     projectile
     helm
     ag

     ;; html
     tagedit
     web-mode

     ;; terraform
     terraform-mode

     ;; misc editing
     avy
     ace-window
     undo-tree
     real-auto-save
     expand-region
     ripgrep

     ;; python
     python-mode
     virtualenvwrapper
     elpy
     python-pytest

     ;; rust
     rust-mode
     toml-mode
     cargo

     ;; groovy
     groovy-mode

     ;; ruby
     ruby-mode

     ;; java
     lsp-java

     ;; sql
     sql-indent

     ;; s3
     s3ed

     ;; elisp
     multi

     ;; php
     php-mode

     ;; haskell
     haskell-mode

     ;; go
     go-mode

     ;; javascript
     js2-mode
     elm-mode
     coffee-mode
     vue-mode
     tide
     rjsx-mode

     ;; scss
     scss-mode

     ;; scala
     scala-mode
     sbt-mode
     lsp-metals

     ;; markdown
     markdown-mode
     markdown-preview-mode

     ;; shell
     exec-path-from-shell
     vterm

     ;; themes
     monokai-theme
     base16-theme
     solarized-theme
     gruvbox-theme
     ujelly-theme
     kaolin-themes
     material-theme
     challenger-deep-theme
     doom-themes
     moe-theme
     zenburn-theme
     nord-theme
     dracula-theme
     )))

;; install any packages that haven't been installed yet
(condition-case nil
    (install-custom-packages)
  (error
   (package-refresh-contents)
   (install-custom-packages)))

(use-package evil
  :ensure t
  :init
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1)
  (evil-set-undo-system 'undo-tree)
  (define-key evil-ex-map "e " 'ido-find-file))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package evil-mc
  :after evil
  :ensure t
  :config
  (global-evil-mc-mode 1))


;; use-package installs
(use-package multi-vterm
    :config
    (add-hook 'vterm-mode-hook
            (lambda ()
            (setq-local evil-insert-state-cursor 'box)
            (evil-insert-state)))
    (define-key vterm-mode-map [return]                      #'vterm-send-return)

    (setq vterm-keymap-exceptions nil)
    (evil-define-key 'insert vterm-mode-map (kbd "C-e")      #'vterm--self-insert)
    (evil-define-key 'insert vterm-mode-map (kbd "C-f")      #'vterm--self-insert)
    (evil-define-key 'insert vterm-mode-map (kbd "C-a")      #'vterm--self-insert)
    (evil-define-key 'insert vterm-mode-map (kbd "C-v")      #'vterm--self-insert)
    (evil-define-key 'insert vterm-mode-map (kbd "C-b")      #'vterm--self-insert)
    (evil-define-key 'insert vterm-mode-map (kbd "C-w")      #'vterm--self-insert)
    (evil-define-key 'insert vterm-mode-map (kbd "C-u")      #'vterm--self-insert)
    (evil-define-key 'insert vterm-mode-map (kbd "C-d")      #'vterm--self-insert)
    (evil-define-key 'insert vterm-mode-map (kbd "C-n")      #'vterm--self-insert)
    (evil-define-key 'insert vterm-mode-map (kbd "C-m")      #'vterm--self-insert)
    (evil-define-key 'insert vterm-mode-map (kbd "C-p")      #'vterm--self-insert)
    (evil-define-key 'insert vterm-mode-map (kbd "C-j")      #'vterm--self-insert)
    (evil-define-key 'insert vterm-mode-map (kbd "C-k")      #'vterm--self-insert)
    (evil-define-key 'insert vterm-mode-map (kbd "C-r")      #'vterm--self-insert)
    (evil-define-key 'insert vterm-mode-map (kbd "C-t")      #'vterm--self-insert)
    (evil-define-key 'insert vterm-mode-map (kbd "C-g")      #'vterm--self-insert)
    (evil-define-key 'insert vterm-mode-map (kbd "C-c")      #'vterm--self-insert)
    (evil-define-key 'insert vterm-mode-map (kbd "C-SPC")    #'vterm--self-insert)
    (evil-define-key 'normal vterm-mode-map (kbd "C-d")      #'vterm--self-insert)
    (evil-define-key 'normal vterm-mode-map (kbd ",c")       #'multi-vterm)
    (evil-define-key 'normal vterm-mode-map (kbd ",n")       #'multi-vterm-next)
    (evil-define-key 'normal vterm-mode-map (kbd ",p")       #'multi-vterm-prev)
    (evil-define-key 'normal vterm-mode-map (kbd "i")        #'evil-insert-resume)
    (evil-define-key 'normal vterm-mode-map (kbd "o")        #'evil-insert-resume)
    (evil-define-key 'normal vterm-mode-map (kbd "<return>") #'evil-insert-resume))

(provide 'setup-package)
