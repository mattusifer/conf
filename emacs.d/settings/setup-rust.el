;; format buffer on save
(defun format-rust-buffer-with-saved-position
    ()
  (let ((w-start (window-start)))
    (rust-format-buffer)
    (set-window-start (selected-window) w-start)))
(add-hook 'rust-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c f") #'format-rust-buffer-with-saved-position)))
(add-hook 'rust-mode-hook
          (lambda ()
             (add-hook 'before-save-hook 'format-rust-buffer-with-saved-position nil t)))

;; lsp
(add-hook 'rust-mode-hook 'lsp)

;; cargo setup
(add-hook 'rust-mode-hook 'cargo-minor-mode)

(provide 'setup-rust)
