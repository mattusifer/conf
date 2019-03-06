;; format buffer
(defun format-with-saved-position
    ()
  (let ((w-start (window-start)))
    (rust-format-buffer)
    (set-window-start (selected-window) w-start)))
(add-hook 'rust-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c f") #'format-with-saved-position)))
(add-hook 'rust-mode-hook
          (lambda ()
             (add-hook 'before-save-hook 'format-with-saved-position nil t)))

;; racer setup
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'company-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)

;; racer describe
(add-hook 'racer-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c d") #'racer-describe)))

;; kbds
(add-hook 'rust-mode-hook
          (lambda () (local-set-key (kbd "TAB") 'company-indent-or-complete-common)))
(add-hook 'rust-mode-hook
          (lambda () (local-set-key (kbd "<tab>") 'company-indent-or-complete-common)))

(setq company-tooltip-align-annotations t)

;; cargo setup
(add-hook 'rust-mode-hook 'cargo-minor-mode)

;; flycheck setup
(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
(add-hook 'rust-mode-hook #'flycheck-mode)

(provide 'setup-rust)
