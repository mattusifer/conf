(elpy-enable)
(require 'python-pytest)

(setq elpy-rpc-python-command "python3")

(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "--simple-prompt -c exec('__import__(\\'readline\\')') -i"
      python-shell-prompt-detect-failure-warning nil)

(setq-default py-shell-name "ipython3")
(setq-default py-which-bufname "ipython3")
(setq py-python-command-args
      '("--gui=wx" "--pylab=wx" "-colors" "Linux"))
(setq python-shell-prompt-detect-failure-warning nil)

(setq python-shell-completion-native nil)
(setq python-shell-completion-native-enable nil)

(setq py-shell-switch-buffers-on-execute-p t)
(setq elpy-interactive-python-command t)
(setq py-switch-buffers-on-execute-p t)
(setq py-split-windows-on-execute-p nil)
(setq py-smart-indentation t)

(setq elpy-shell-echo-input nil)

(add-hook 'python-mode-hook 'highlight-indentation-mode)
(add-hook 'python-mode-hook 'elpy-mode)

;; format buffer on save
(defun format-python-buffer-with-saved-position
    ()
  (let ((w-start (window-start)))
    (elpy-format-code)
    (set-window-start (selected-window) w-start)))
(add-hook 'python-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c f") #'format-python-buffer-with-saved-position)))
(add-hook 'python-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'format-python-buffer-with-saved-position nil t)))

;; proper syntax highlighting for Pipfile/Pipfile.lock
(add-to-list 'auto-mode-alist '("Pipfile\\'" . toml-mode))
(add-to-list 'auto-mode-alist '("Pipfile\\.lock\\'" . js2-mode))

;; (defun python-setup-current-project ()
;;   (with-temp-buffer
;;     (let ((python-set-proj-dir-code
;;            "
;; import os
;; home = os.path.expanduser('~')
;; while os.path.isfile('__init__.py') and (os.getcwd() != home): os.chdir('..')
;; del os

;; "))
;;       (insert python-set-proj-dir-code)
;;       (elpy-shell-send-region-or-buffer)
;;       (message "Setup project path"))))

;; (add-hook 'inferior-python-mode-hook 'python-setup-current-project)

(provide 'setup-python)
