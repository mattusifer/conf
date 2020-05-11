;; javascript / html
(add-to-list 'auto-mode-alist '("components\\/.*\\.js\\'" . rjsx-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-hook 'js-mode-hook 'subword-mode)
(add-hook 'js-mode-hook 'js2-minor-mode)
(add-hook 'html-mode-hook 'subword-mode)
(setq js-indent-level 2)
(setq js2-highlight-level 3)
(eval-after-load "sgml-mode"
  '(progn
     (require 'tagedit)
     (tagedit-add-paredit-like-keybindings)
     (add-hook 'html-mode-hook (lambda () (tagedit-mode 1)))))


;; coffeescript
(add-to-list 'auto-mode-alist '("\\.coffee.erb$" . coffee-mode))
(add-hook 'coffee-mode-hook 'subword-mode)
(add-hook 'coffee-mode-hook 'highlight-indentation-current-column-mode)
(add-hook 'coffee-mode-hook
          (defun coffee-mode-newline-and-indent ()
            (define-key coffee-mode-map "\C-j" 'coffee-newline-and-indent)
            (setq coffee-cleanup-whitespace nil)))
(custom-set-variables
 '(coffee-tab-width 2))

;; ember
(add-to-list 'auto-mode-alist '("\\.hbs\\'" . html-mode))

;; elm
(setq elm-format-on-save t)

;; svelte
(add-to-list 'auto-mode-alist '("\\.svelte\\'" . web-mode))

;; typescript
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

(add-hook 'typescript-mode-hook #'setup-tide-mode)
(add-hook 'rjsx-mode-hook #'setup-tide-mode)

(add-hook 'typescript-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'tide-format-before-save nil t)))
(add-hook 'rsjx-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'tide-format-before-save nil t)))

(provide 'setup-javascript)
