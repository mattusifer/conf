(elpy-enable)
(setq python-shell-interpreter "ipython"
       python-shell-interpreter-args "-i --simple-prompt")
(setq-default py-shell-name "ipython")
(setq-default py-which-bufname "IPython")
(setq py-python-command-args
      '("--gui=wx" "--pylab=wx" "-colors" "Linux"))
(setq py-shell-switch-buffers-on-execute-p t)
(setq elpy-interactive-python-command t)
(setq py-switch-buffers-on-execute-p t)
(setq py-split-windows-on-execute-p nil)
(setq py-smart-indentation t)

(add-hook 'python-mode-hook 'highlight-indentation-mode)
