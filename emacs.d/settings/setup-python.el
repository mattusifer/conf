(elpy-enable)
(require 'python-pytest)

;; this is not backwards compatible to ipython versions <5!
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "--simple-prompt")

(setq-default py-shell-name "ipython")
(setq-default py-which-bufname "ipython")
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
(add-hook 'python-mode-hook 'flycheck-mode)

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
