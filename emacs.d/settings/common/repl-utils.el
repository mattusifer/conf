;; utilities for starting a repl process and sending commands to it
;; from external buffers

;; used primarily for projects using JVM languages, it will currently
;; find the base of a project based on the presense of a 'src'
;; directory

(require 'send-to-process)
(setq ensime-startup-notification nil)

(defun get-last-occurrence (substr string start)
  "Get the last occurrence of a substring within a string"
  (let ((match-index (string-match substr string start)))
    (if (and match-index (string-match substr string (match-end 0)))
        (get-last-occurrence substr string (+ match-index (length substr)))
      match-index)))

(defun get-base-dir (str)
  "Get the base directory of the current project"
  (substring str 0 (get-last-occurrence "src" str 0)))

(defun send-string-to-repl (execution-fn
                            repl-process-name
                            repl-create-cmd-args
                            repl-mode)
  "
Evaluate a string in a repl - will pop one open if it
doesn't already exist
"
  ;; start process if it hasn't started yet
  (unless (get-process repl-process-name)

    ;; look for a 'src' folder in the current dir
    ;; if none exists, get the base dir
    (if (not (member "src" (directory-files default-directory)))
        (setq default-directory (get-base-dir default-directory)))
    
    ;; use a pipe
    (let ((process-connection-type nil)
          (repl-buffer-name (concat "*" repl-process-name "*")))
      (apply 'start-process (append (list repl-process-name repl-buffer-name)
                                    repl-create-cmd-args))
      (set-buffer repl-buffer-name))
    (linum-mode -1)
    (special-mode)
    (if (not (null 'repl-mode)) (repl-mode)))

  ;; execute
  (funcall execution-fn)

  ;;display buffer
  (unless (get-buffer-window repl-buffer-name)
    (display-buffer 
     (get-buffer repl-buffer-name)
     '(display-buffer-pop-up-window
       (reusable-frames . 0)
       (window-height . 20) (window-width . nil)))))

(defun send-buffer-region-to-repl (repl-process-name
                                   repl-create-cmd-args
                                   repl-mode)
  (send-string-to-repl (lambda () (send-buffer-region-to-process repl-process-name))
                       repl-process-name
                       repl-create-cmd-args
                       repl-mode))

(defun send-line-region-to-repl (repl-process-name
                                 repl-create-cmd-args
                                 repl-mode)
  (send-string-to-repl (lambda () (send-line-region-to-process repl-process-name))
                       repl-process-name
                       repl-create-cmd-args
                       repl-mode))

(provide 'repl-utils)
