(require 'repl-utils)
(require 'company-lsp)
(require 'lsp-scala)

(defun get-subproject (path)
  "Confirm that the file in PATH is within a subproject.
Return the name of the subproject if true."
  (let ((base-dir (get-base-dir path)))
    (when (not (member "build.sbt" (directory-files base-dir)))
      (car (last (split-string base-dir "/") 2)))))

(defun eval-scala-buffer ()
  "Evaluate entire buffer."
  (interactive)
  (let ((prefix
         (if-let ((subproject (get-subproject default-directory)))
             (concat subproject "/") "")))
    (send-buffer-region-to-repl-buffer "*scala-repl*"
                                       (format "sbt -J-Xms3G -J-Xmx6G -J-XX:MaxMetaspaceSize=2G %s %s"
                                               (concat prefix "test:compile")
                                               (concat prefix "test:console")) nil "build.sbt")))

(defun eval-scala-region ()
  (interactive)
  (let ((prefix
         (if-let ((subproject (get-subproject default-directory)))
             (concat subproject "/") "")))
    (send-line-region-to-repl-buffer "*scala-repl*"
                                     (format "sbt -J-Xms3G -J-Xmx6G -J-XX:MaxMetaspaceSize=2G %s %s"
                                             (concat prefix "test:compile")
                                             (concat prefix "test:console")) nil "build.sbt")))

(defun open-sbt ()
  (interactive)
  (when (> (window-total-width) 200)
    (split-window-horizontally)
    (other-window 1))
  (sbt:run-sbt nil t))

(defun open-scala-scratch-buffer ()
  (interactive)
  (if (one-window-p)
      (split-window-horizontally))
  (other-window 1)
  (switch-to-buffer "scala-scratch")
  (scala-mode)
  (setq default-directory "/"))

(add-hook 'scala-mode-hook (lambda () (local-set-key (kbd "C-c C-e") 'eval-scala-region)))
(add-hook 'scala-mode-hook (lambda () (local-set-key (kbd "C-c C-k") 'eval-scala-buffer)))
(add-hook 'scala-mode-hook (lambda () (local-set-key (kbd "C-c C-p") 'open-sbt)))
(add-hook 'scala-mode-hook (lambda () (local-set-key (kbd "C-c C-s") 'open-scala-scratch-buffer)))

;; format buffer on save
(defun format-scala-buffer-with-saved-position
    ()
  (when (not (s-ends-with? ".sbt" buffer-file-name))
    (let ((w-start (window-start)))
      (lsp-format-buffer)
      (set-window-start (selected-window) w-start))))
(add-hook 'scala-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c f") #'format-scala-buffer-with-saved-position)))
(add-hook 'scala-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'format-scala-buffer-with-saved-position nil t)))

(defmacro make-coalescing (fn default-timeout) ()
          (let* ((fn-name (intern (symbol-name fn)))
                 (timer-sym (intern (format "coalescing-%s-timer-id" fn-name)))
                 (timeout-var-sym (intern (format "coalescing-%s-timeout" fn-name)))
                 (cls-fn (intern (format "coalescing-%s" fn-name)))
                 (timer-docstring (format "Timeout to invoke %s" fn-name))
                 (cls-fn-docstring (format "Execute %s when idle" fn-name)))
            `(progn
               (defcustom ,timeout-var-sym ,default-timeout ,timer-docstring :type 'number)
               (defvar ,timer-sym nil)
               (defun ,cls-fn (&rest args) ,cls-fn-docstring
                      (-some->> ,timer-sym (cancel-timer))
                      (setq ,timer-sym
                            (run-with-idle-timer ,timeout-var-sym nil
                                                 ;; don't know if we have to
                                                 ;; pass around args in this
                                                 ;; clumsy manner
                                                 (lambda (args)
                                                   (apply #',fn args)
                                                   (setq ,timer-sym nil)) args))))))

;; defines a function named coalescing-some-function that (I hope) accepts the
;; same arguments as some-function and executes after 0.2 seconds of idle time
;; (user-configurable via coalescing-some-function-timeout)
;; Also, something like this might already exist in Emacs's standard library?
(make-coalescing lsp-on-change 0.2)

(add-hook 'scala-mode-hook 'lsp)
(add-hook 'scala-mode-hook 'company-mode)
(setq lsp-prefer-flymake nil)

(push 'company-lsp company-backends)

(provide 'setup-scala)
