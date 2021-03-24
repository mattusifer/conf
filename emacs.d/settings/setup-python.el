(require 'elpy)
(elpy-enable)

(require 'python-pytest)

(setq elpy-shell-echo-input nil
      elpy-shell-echo-output nil
      elpy-shell-starting-directory 'project-root
      elpy-rpc-timeout 10
      elpy-rpc-python-command "python"
      elpy-rpc-virtualenv-path 'current
      elpy-syntax-check-command "flake8 --ignore=T499"
      elpy-interactive-python-command t
      )

(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "--simple-prompt -c exec('__import__(\\'readline\\')') -i"
      python-shell-prompt-detect-failure-warning nil)

(setq-default py-shell-name "ipython")
(setq-default py-which-bufname "ipython")
(setq py-python-command-args
      '("--gui=wx" "--pylab=wx" "-colors" "Linux"))
(setq python-shell-prompt-detect-failure-warning nil)

(setq python-shell-completion-native nil)
(setq python-shell-completion-native-enable nil)

(setq py-shell-switch-buffers-on-execute-p t)
(setq py-switch-buffers-on-execute-p t)
(setq py-split-windows-on-execute-p nil)
(setq py-smart-indentation t)

(add-hook 'python-mode-hook 'highlight-indentation-mode)
(add-hook 'python-mode-hook 'elpy-mode)

;; company completion
(push 'elpy-company-backend company-backends)
(add-hook 'elpy-mode-hook 'company-mode)

;; format buffer on save
(defun format-python-buffer-with-saved-position
    ()
  (let ((w-start (window-start)))
    (elpy-black-fix-code)
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

(provide 'setup-python)
