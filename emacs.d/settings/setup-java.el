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


(defun get-last-occurrence (substr string start)
  (let ((match-index (string-match substr string start)))
    (if (and match-index (string-match substr string (match-end 0)))
        (get-last-occurrence substr string (+ match-index (length substr)))
      match-index)))

(defun get-base-dir (str)
  (substring str 0 (get-last-occurrence "src" str 0)))

(defun eval-java (str dir)
  ;; start java if it hasn't started yet
  (unless (get-process "java-repl")

    ;; look for a 'src' folder in the current dir
    ;; if none exists, get the base dir
    (if (not (member "src" (directory-files default-directory)))
        (setq default-directory (get-base-dir dir)))
    
    (let ((process-connection-type nil))  ; use a pipe
      (start-process "java-repl" "*java*"  "java" "-jar" "/Users/musifer/src/mattusifer/java-repl/build/artifacts/javarepl-dev.build.jar"))
    (set-buffer "*java*")
    (special-mode)
    (java-mode))  
  ;; execute
  (process-send-string "java-repl" str)
  ;;display buffer
  (unless (get-buffer-window "*java*")
    (display-buffer 
     (get-buffer "*java*")
     '(display-buffer-pop-up-window
       (reusable-frames . 0)
       (window-height . 20) (window-width . nil)))))

(defun eval-java-buffer ()
  "will evaluate entire buffer"
  (interactive)
  (if (use-region-p)
      (progn (eval-java
              (replace-regexp-in-string "\n\s*\n" "\n" (buffer-substring-no-properties (region-beginning) (region-end))) default-directory)
             (eval-java "\n" default-directory))
    (eval-java (replace-regexp-in-string "\n\s*\n" "\n" (buffer-string)) default-directory)))

(defun eval-java-region ()
  "will evaluate a single line or a region, depending on whether it's highlighted"
  (interactive)
  (if (use-region-p)
      (progn (eval-java 
              (replace-regexp-in-string "\n\s*" "" (buffer-substring-no-properties (region-beginning) (region-end))) default-directory) 
             (eval-java "\n" default-directory))
      (eval-java (thing-at-point 'line) default-directory)))

(add-hook 'java-mode-hook (lambda () (local-set-key (kbd "C-c j e") 'eval-java-region)))
(add-hook 'java-mode-hook (lambda () (local-set-key (kbd "C-c j k") 'eval-java-buffer)))

(provide 'setup-java)
