(elpy-enable)
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "--simple-prompt --pprint")
(setq-default py-shell-name "ipython")
(setq-default py-which-bufname "ipython")
(setq py-python-command-args
      '("--gui=wx" "--pylab=wx" "-colors" "Linux"))
(setq python-shell-completion-native nil)
(setq py-shell-switch-buffers-on-execute-p t)
(setq elpy-interactive-python-command t)
(setq py-switch-buffers-on-execute-p t)
(setq py-split-windows-on-execute-p nil)
(setq py-smart-indentation t)

(add-hook 'python-mode-hook 'highlight-indentation-mode)
(add-hook 'python-mode-hook 'elpy-mode)

(provide 'setup-python)
