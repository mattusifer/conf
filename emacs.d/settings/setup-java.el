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

(require 'repl-utils)

(defun eval-java-buffer ()
  "will evaluate entire buffer"
  (interactive)
  (send-buffer-region-to-repl "java-repl" '("java" "-jar" "/Users/musifer/src/mattusifer/java-repl/build/artifacts/javarepl-dev.build.jar")
                              'java-mode))

(defun eval-java-region ()
  (interactive)
  (send-line-region-to-repl "java-repl" '("java" "-jar" "/Users/musifer/src/mattusifer/java-repl/build/artifacts/javarepl-dev.build.jar")
                            'java-mode))

(add-hook 'java-mode-hook (lambda () (local-set-key (kbd "C-c j e") 'eval-java-region)))
(add-hook 'java-mode-hook (lambda () (local-set-key (kbd "C-c j k") 'eval-java-buffer)))

(provide 'setup-java)
