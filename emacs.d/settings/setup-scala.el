(require 'ensime)
(setq ensime-startup-notification nil)
;; (add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

(define-key scala-mode-map (kbd "TAB") nil)

(defun get-last-occurrence (substr string start)
  (let ((match-index (string-match substr string start)))
    (if (and match-index (string-match substr string (match-end 0)))
        (get-last-occurrence substr string (+ match-index (length substr)))
      match-index)))

(defun get-base-dir (str)
  (substring str 0 (get-last-occurrence "src" str 0)))

(defun eval-scala (str dir)
  ;; start scala if it hasn't started yet
  (unless (get-process "scala-repl")

    ;; look for a 'src' folder in the current dir
    ;; if none exists, get the base dir
    (if (not (member "src" (directory-files default-directory)))
        (setq default-directory (get-base-dir dir)))
    
    (let ((process-connection-type nil))  ; use a pipe
      (start-process "scala-repl" "*scala*"  "sbt" "-J-Xms3G" "-J-Xmx6G" "compile" "console"))
    (set-buffer "*scala*")
    (linum-mode -1)
    (special-mode)
    (scala-mode))  
  ;; execute
  (process-send-string "scala-repl" str)
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
  (if (use-region-p)
      (progn (eval-scala
              (replace-regexp-in-string "\n\s*\n" "\n" (buffer-substring-no-properties (region-beginning) (region-end))) default-directory)
             (eval-scala "\n" default-directory))
    (eval-scala (replace-regexp-in-string "\n\s*\n" "\n" (buffer-string)) default-directory)))

(defun eval-scala-region ()
  "will evaluate a single line or a region, depending on whether it's highlighted"
  (interactive)
  (if (use-region-p)
      (progn (eval-scala 
              (replace-regexp-in-string "\n\s*" "" (buffer-substring-no-properties (region-beginning) (region-end))) default-directory) 
             (eval-scala "\n" default-directory))
      (eval-scala (thing-at-point 'line) default-directory)))

(add-hook 'scala-mode-hook (lambda () (local-set-key (kbd "C-c C-e") 'eval-scala-region)))
(add-hook 'scala-mode-hook (lambda () (local-set-key (kbd "C-c C-k") 'eval-scala-buffer)))
 
(provide 'setup-scala)
