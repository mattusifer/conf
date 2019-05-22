;; utilities for starting a repl process and sending commands to it
;; from external buffers

;; used primarily for projects using JVM languages, it will currently
;; find the base of a project based on the presence of a 'src'
;; directory

(require 'send-to-buffer)

(defun get-last-occurrence (substr string start)
  "Get the last occurrence of a substring within a string"
  (let ((match-index (string-match substr string start)))
    (if (and match-index (string-match substr string (match-end 0)))
        (get-last-occurrence substr string (+ match-index (length substr)))
      match-index)))

(defun get-base-dir-or-parent (str &optional indicator)
  "Get the base directory of the current project. If that base
directory isn't itself a git repo, assume it's a subproject and
return the parent of the base directory."
  (or indicator (setq indicator ".git"))

  (if (member indicator (directory-files str)) str
    (let ((potential-dir (get-base-dir str)))
      (if (member indicator (directory-files potential-dir))
          potential-dir
        (mapconcat 'identity (butlast (split-string potential-dir "/") 2) "/") ))))

(defun get-base-dir (str)
  "Get the base directory of the current project"
  (substring str 0 (get-last-occurrence "src" str 0)))

(defun send-string-to-repl-buffer (execution-fn
                                   repl-buffer-name
                                   repl-create-cmd
                                   &optional repl-mode indicator)
  (when (not (get-buffer repl-buffer-name))
    ;; look for a 'src' folder in the current dir
    ;; if none exists, get the base dir
    (let ((default-directory
            (if (not (member "src" (directory-files default-directory)))
                (get-base-dir-or-parent default-directory indicator) default-directory)))
      (async-shell-command repl-create-cmd repl-buffer-name)))

  (funcall execution-fn)

  (unless (get-buffer-window repl-buffer-name)
    (display-buffer
     (get-buffer repl-buffer-name)
     '(display-buffer-pop-up-window
       (reusable-frames . 0)
       (window-height . 20) (window-width . nil)))))

(defun send-buffer-region-to-repl-buffer (repl-buffer-name
                                          repl-create-cmd
                                          &optional repl-mode indicator)
  (send-string-to-repl-buffer (lambda () (send-buffer-region-to-buffer repl-buffer-name))
                       repl-buffer-name
                       repl-create-cmd
                       repl-mode
                       indicator))

(defun send-line-region-to-repl-buffer (repl-buffer-name
                                        repl-create-cmd
                                        &optional repl-mode indicator)
  (send-string-to-repl-buffer (lambda () (send-line-region-to-buffer repl-buffer-name))
                              repl-buffer-name
                              repl-create-cmd
                              repl-mode
                              indicator))

(defun send-custom-string-to-repl-buffer (custom-string
                                          repl-buffer-name
                                          repl-create-cmd
                                          &optional repl-mode indicator)
  (send-string-to-repl-buffer (lambda () (send-string-to-buffer repl-buffer-name (concat custom-string "\n")))
                              repl-buffer-name
                              repl-create-cmd
                              repl-mode
                              indicator))

(provide 'repl-utils)
