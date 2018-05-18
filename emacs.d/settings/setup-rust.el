;; cargo mode setup
(add-hook 'rust-mode-hook 'cargo-minor-mode)

;; auto formatting
(add-hook 'rust-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c <tab>") #'rust-format-buffer)))

;; racer setup
(setq racer-cmd "~/.cargo/bin/racer")
(setq racer-rust-src-path "~/src/rust-lang/rust/src")

(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)

;; flycheck setup
(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
(add-hook 'rust-mode-hook #'flycheck-mode)

(provide 'setup-rust)
