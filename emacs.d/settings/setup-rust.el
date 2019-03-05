;; format buffer
(add-hook 'rust-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c f") #'rust-format-buffer)))

;; racer setup
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'company-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)

(define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
(setq company-tooltip-align-annotations t)

;; cargo setup
(add-hook 'rust-mode-hook 'cargo-minor-mode)

;; flycheck setup
(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
(add-hook 'rust-mode-hook #'flycheck-mode)

(provide 'setup-rust)
