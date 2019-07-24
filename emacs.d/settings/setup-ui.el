;;;;;;;;;;
;; theme

;; load themes from submodules
(let ((custom-theme-paths '("~/.emacs.d/vendor/themes/tomorrow-theme/GNU Emacs")))
  (dolist (element custom-theme-paths)
    (add-to-list 'custom-theme-load-path element)
    (add-to-list 'load-path element)))

(setq mu/current-theme
      (if (eq system-type 'darwin)
          'sanityinc-tomorrow-eighties 'doom-tomorrow-night))

;; apply theme to new frames
(defun apply-color-theme (frame)
  (select-frame frame)
  (load-theme mu/current-theme t))
(setq color-theme-is-global nil)
(add-hook 'after-make-frame-functions 'apply-color-theme)

;; load theme in current window
(load-theme mu/current-theme t)

;;;;;;;;;;
;; ui

;; font
(add-to-list 'default-frame-alist '(font . "Hack"))
(set-face-attribute 'default nil
            :family "Hack")

;; font size
(if (eq system-type 'darwin)

    ; OS X
    (set-face-attribute 'default nil :height 130)

  ; linux
  (set-face-attribute 'default nil :height 105))

;; various settings
(setq ;; killing/yanking interacts with the clipboard
      x-select-enable-clipboard t

      ;; Save clipboard strings into kill ring before replacing them.
      save-interprogram-paste-before-kill t

      ;; Shows all options when running apropos
      apropos-do-all t

      ;; Mouse yank commands yank at point instead of at click.
      mouse-yank-at-point t)

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

;; time
(setq display-time-day-and-date t)
(setq display-time-format "%Y-%m-%d %k:%M %p")
(display-time-mode 1)

;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

;; title format
(setq-default frame-title-format "%b")

;; mode line format
;; (require 'zerodark-modeline)
;; (zerodark-setup-modeline-format)

;; diminish modes in the modeline
(defun modeline-set-lighter (minor-mode lighter)
  (when (assq minor-mode minor-mode-alist)
    (setcar (cdr (assq minor-mode minor-mode-alist)) lighter)))

(defun modeline-remove-lighter (minor-mode)
  (modeline-set-lighter minor-mode ""))

(eval-after-load "projectile" '(modeline-remove-lighter 'projectile-mode))
(eval-after-load "undo-tree" '(modeline-remove-lighter 'undo-tree-mode))
(eval-after-load "paredit" '(modeline-remove-lighter 'paredit-mode))
(modeline-remove-lighter 'auto-revert-mode)

;; no bell
(setq ring-bell-function 'ignore)

;; pixelwise resizing
(setq frame-resize-pixelwise t)

;; include filepath when visiting two identically named files
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Turn on recent file mode
(setq recentf-save-file (concat user-emacs-directory ".recentf"))
(require 'recentf)
(setq recentf-auto-cleanup 'never) ;; don't try to stat files - because TRAMP
(recentf-mode 1)
(setq recentf-max-menu-items 40)

;; ido
(ido-mode t)
(setq ido-enable-flex-matching t)
(setq ido-use-filename-at-point nil)
(setq ido-auto-merge-work-directories-length -1)
(setq ido-use-virtual-buffers t)
(ido-ubiquitous-mode 1)

(defadvice ido-find-file (after find-file-sudo activate)
  "Attempt to open file as root if we don't have write permissions."
  (unless (and (not (eq major-mode 'dired-mode))
               buffer-file-name
               (file-writable-p buffer-file-name))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

;; Show a list of buffers
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; smex
(setq smex-save-file (concat user-emacs-directory ".smex-items"))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)

;; projectile
(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; async operations
(require 'async-bytecomp)
(async-bytecomp-package-mode 1)
(autoload 'dired-async-mode "dired-async.el" nil t)
(dired-async-mode 1)

;; winner
(winner-mode t)

;; eyebrowse workspaces
(eyebrowse-mode t)
(setq eyebrowse-new-workspace t)

;;;;;;;;;;
;; editing

(require 'god-mode)
(global-set-key (kbd "<escape>") 'god-mode-all)
(god-mode)

(define-key god-local-mode-map (kbd "i") 'god-mode-all)
(define-key god-local-mode-map (kbd ".") 'repeat)

(setq god-exempt-major-modes nil)
(setq god-exempt-predicates nil)

(require 'god-mode-isearch)
(define-key isearch-mode-map (kbd "<escape>") 'god-mode-isearch-activate)
(define-key god-mode-isearch-map (kbd "<escape>") 'god-mode-isearch-disable)

;; more convenient window functions for god mode
(global-set-key (kbd "C-x C-1") 'delete-other-windows)
(global-set-key (kbd "C-x C-2") 'split-window-below)
(global-set-key (kbd "C-x C-3") 'split-window-right)
(global-set-key (kbd "C-x C-0") 'delete-window)

;; change cursor style for god mode
(defun god-mode-update-cursor ()
  (setq cursor-type (if (or god-local-mode buffer-read-only)
                        'box
                      'bar)))

(add-hook 'god-mode-enabled-hook 'god-mode-update-cursor)
(add-hook 'god-mode-disabled-hook 'god-mode-update-cursor)

;; No need for ~ files when editing
(setq create-lockfiles nil)

;; save backups in the backups dir
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))
(setq vc-make-backup-files t)

;; highlight matching parens
(show-paren-mode 1)

;; highlight current line
(global-hl-line-mode 1)

;; swap super and meta on OSX
(setq mac-option-modifier 'super)
(setq mac-command-modifier 'meta)

;; fix weird clipboard error on OSX
(defun ns-get-pasteboard ()
  "Returns the value of the pasteboard, or nil for unsupported formats."
  (condition-case nil
      (ns-get-selection-interval 'CLIPBOARD)
    (quit nil)))

;; scrolling line-by-line
(setq scroll-step            1
      scroll-conservatively  10000)

;; undo tree
(global-undo-tree-mode)

;; run regexp searches by default
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; no hard tabs
(setq-default indent-tabs-mode nil)

;; comments
(defun toggle-comment-on-line ()
  "comment or uncomment current line"
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))
(global-set-key (kbd "C-;") 'toggle-comment-on-line)

;; rainbow delims
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; use 2 spaces for tabs
(defun die-tabs ()
  (interactive)
  (set-variable 'tab-width 2)
  (mark-whole-buffer)
  (untabify (region-beginning) (region-end))
  (keyboard-quit))

;; clean up file on save
(setq require-final-newline t)
(add-hook 'before-save-hook 'whitespace-cleanup)

;; DUMB jump
(dumb-jump-mode)

;;;;;;;;;;
;; window functions

(defun toggle-window-split ()
  "transpose window configuration"
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(global-set-key (kbd "C-c C-t") 'toggle-window-split)

(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file)

;; windmove
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;;;;;;;;;;
;; misc keybindings
(require 'key-chord)
(key-chord-mode +1)

;; ace window
(global-set-key (kbd "M-o") 'ace-window)
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

;; avy (ace jump)
(global-set-key (kbd "C-c M-SPC") 'avy-goto-char)

;; multi cursor
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-S-c C->") 'mc/mark-all-like-this)

;; killing
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-c C-w") 'kill-word)
(global-set-key (kbd "C-x C-k") 'kill-region)
(global-set-key (kbd "C-c C-k") 'kill-region)
(global-set-key (kbd "C-c C-q")
               (lambda () (interactive) (kill-buffer) (delete-window)))
(global-set-key (kbd "C-c u") 'undo-tree-visualize)

;; kill emacs and server
(global-set-key (kbd "C-x c") (lambda () (interactive)
                                (save-some-buffers)
                                (kill-emacs)))

;; frames
(global-set-key (kbd "C-c w 0") 'delete-frame)
(global-set-key (kbd "C-c w c") 'make-frame-command)
(global-set-key (kbd "C-c w o") 'other-frame)

;; agenda
(global-set-key (kbd "C-c a") 'org-agenda)

;; parediting
(global-set-key (kbd "C-}") 'paredit-forward-barf-sexp)
(global-set-key (kbd "C-{") 'paredit-backward-barf-sexp)

;; expand region
(global-set-key (kbd "C-*") 'er/expand-region)

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

;; iy-go-to-char
(require 'iy-go-to-char)
(key-chord-define-global "jk" 'iy-go-to-char)
(key-chord-define-global "hj" 'iy-go-to-char-backward)

;; s3ed
(require 's3ed)
(global-set-key (kbd "C-c s f") (lambda () (interactive) (s3ed-find-file)))
;; (s3ed-mode)

(provide 'setup-ui)
