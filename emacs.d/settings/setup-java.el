(require 'eclim)
(require 'eclimd)
(add-hook 'java-mode-hook 'global-eclim-mode)
(setq help-at-pt-display-when-idle t)
(setq help-at-pt-timer-delay 0.1)
(setq eclim-problems-show-pos t)
(help-at-pt-set-timer)
(require 'ac-emacs-eclim)
(add-hook 'java-mode-hook 'ac-emacs-eclim-config)
(require 'company-emacs-eclim)
(company-emacs-eclim-setup)

(setq eclipse-installation-dir "~/eclipse/java-neon/Eclipse.app/Contents/Eclipse/")

(custom-set-variables
 '(eclim-eclipse-dirs '(eclipse-installation-dir))
 '(eclim-executable (concat eclipse-installation-dir "eclim")))

(provide 'setup-java)
