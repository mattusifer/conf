;;;;;;;;;;
;; ui

;; load theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'load-path "~/.emacs.d/themes")
(load-theme 'tomorrow-night-bright t)

;; font size
(set-face-attribute 'default nil :height 130)

;; maximize frame
(setq initial-frame-alist '((top . 0) (left . -1)))
(toggle-frame-maximized)

;; various settings
(setq ;; killing/yanking interacts with the clipboard
      x-select-enable-clipboard t

      ;; Save clipboard strings into kill ring before replacing them.
      save-interprogram-paste-before-kill t

      ;; Shows all options when running apropos
      apropos-do-all t

      ;; Mouse yank commands yank at point instead of at click.
      mouse-yank-at-point t)

;; cursor
(blink-cursor-mode nil)

;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

;; title format
(setq-default frame-title-format "%b (%f)")

;; no bell
(setq ring-bell-function 'ignore)

(setq frame-resize-pixelwise t)

;; include filepath when visiting two identically named files
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Turn on recent file mode
(setq recentf-save-file (concat user-emacs-directory ".recentf"))
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 40)

;; ido
(ido-mode t)
(setq ido-enable-flex-matching t)
(setq ido-use-filename-at-point nil)
(setq ido-auto-merge-work-directories-length -1)
(setq ido-use-virtual-buffers t)
(ido-ubiquitous-mode 1)

;; Show a list of buffers
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; smex
(setq smex-save-file (concat user-emacs-directory ".smex-items"))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)

;; projectile
(projectile-global-mode)
(setq projectile-enable-caching t)

;; async operations
(require 'async-bytecomp)
(async-bytecomp-package-mode 1)
(autoload 'dired-async-mode "dired-async.el" nil t)
(dired-async-mode 1)


;;;;;;;;;;
;; editing

;; No need for ~ files when editing
(setq create-lockfiles nil)

;; save backups in the backups dir
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))
(setq vc-make-backup-files t)

;; auto-complete
(require 'company)
(require 'cl)
(require 'auto-complete-config)
(ac-config-default)
(global-company-mode t)

;; unbind <tab> everywhere in favor of yasnippet
(define-key ac-mode-map (kbd "TAB") nil)
(define-key ac-completing-map (kbd "TAB") nil)
(define-key ac-completing-map [tab] nil)
(define-key company-active-map [tab] nil)
(define-key company-active-map (kbd "TAB") nil)

;; hippie-expand
(global-set-key (kbd "M-/") 'hippie-expand)
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

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

;; show useless whitespace
(setq show-trailing-whitespace t)

;; undo tree
(global-undo-tree-mode)

;; run regexp searches by default
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; no hard tabs
(setq-default indent-tabs-mode nil)

;; saveplace
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (concat user-emacs-directory "places"))

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

;; fix weird os x kill error
(defun ns-get-pasteboard ()
  "Returns the value of the pasteboard, or nil for unsupported formats."
  (condition-case nil
      (ns-get-selection-internal 'CLIPBOARD)
    (quit nil)))

(setq electric-indent-mode nil)
 
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

(defun delete-current-buffer-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

(global-set-key (kbd "C-x C-k") 'delete-current-buffer-file)

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

;;;;;;;;;;
;; misc keybindings

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


(provide 'setup-ui)
