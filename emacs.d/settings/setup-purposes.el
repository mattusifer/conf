(require 'window-purpose)
(purpose-mode)

(add-to-list 'purpose-user-mode-purposes '(scala-mode . sc))
(add-to-list 'purpose-user-mode-purposes '(sbt-mode . sbt))
(purpose-compile-user-configuration)

(require 'window-purpose-x)
(purpose-x-kill-setup)
(purpose-x-magit-single-on)

(provide 'setup-purposes)
