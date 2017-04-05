(require 'ensime)
(require 'send-to-process)
(setq ensime-startup-notification nil)

(defun get-last-occurrence (substr string start)
  "Get the last occurrence of a substring within a string"
  (let ((match-index (string-match substr string start)))
    (if (and match-index (string-match substr string (match-end 0)))
        (get-last-occurrence substr string (+ match-index (length substr)))
      match-index)))

(defun get-base-dir (str)
  "Get the base directory of the current scala project"
  (substring str 0 (get-last-occurrence "src" str 0)))

(defun eval-scala (execution-fn)
  "
Evaluate a scala string in a repl - will pop one open if it
doesn't already exist
"
  ;; start scala if it hasn't started yet
  (unless (get-process "scala-repl")

    ;; look for a 'src' folder in the current dir
    ;; if none exists, get the base dir
    (if (not (member "src" (directory-files default-directory)))
        (setq default-directory (get-base-dir default-directory)))
    
    (let ((process-connection-type nil))  ; use a pipe
      (start-process "scala-repl" "*scala*"  "sbt" "-J-Xms3G" "-J-Xmx6G" "compile" "console"))
    (set-buffer "*scala*")
    (linum-mode -1)
    (special-mode)
    (scala-mode))

  ;; execute
  (funcall execution-fn)

  ;;display buffer
  (unless (get-buffer-window "*scala*")
    (display-buffer 
     (get-buffer "*scala*")
     '(display-buffer-pop-up-window
       (reusable-frames . 0)
       (window-height . 20) (window-width . nil)))))

(defun eval-scala-buffer ()
  "will evaluate entire buffer"
  (interactive)
  (eval-scala (lambda () (send-buffer-region-to-process "scala-repl"))))

(defun eval-scala-region ()
  (interactive)
  (eval-scala (lambda () (send-line-region-to-process "scala-repl"))))

(add-hook 'scala-mode-hook (lambda () (local-set-key (kbd "C-c C-e") 'eval-scala-region)))
(add-hook 'scala-mode-hook (lambda () (local-set-key (kbd "C-c C-k") 'eval-scala-buffer)))
 
(provide 'setup-scala)
