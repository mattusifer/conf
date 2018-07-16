(when (eq system-type 'darwin)
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-envs
   '("PATH" "AWS_ACCESS_KEY_ID" "AWS_SECRET_ACCESS_KEY" "SPARK_HOME" "PYTHONPATH"
     "BLACKFYNN_API_KEY" "BLACKFYNN_SECRET_KEY" "BLACKFYNN_NEXUS_USER" "BLACKFYNN_NEXUS_PW")))

;; emacs terminal conf
(setq system-uses-terminfo nil)

;; todo: setup SSH agent

(require 'multi-term)
(defun create-multiterm-buffer ()
  (multi-term)
  (linum-mode -1)
  (setq comint-move-point-for-output nil)
  (setq comint-scroll-show-maximum-output nil)
  (setq current-terminal-buffer (buffer-name)))

(defun create-or-show-small-terminal-multiterm ()
  "Pop open a terminal. Will switch active buffer to current terminal buffer if it exists and is not visible."
  (interactive)
  (if (> (window-total-width) 200)
      (progn
        (split-window-horizontally)
        (windmove-right)))
  (if (or (not (boundp 'current-terminal-buffer))
          (not (get-buffer current-terminal-buffer))
          (and (boundp 'current-terminal-buffer)
               (get-buffer-window current-terminal-buffer)))
      (create-multiterm-buffer)
    (switch-to-buffer (get-buffer current-terminal-buffer))))

(defun next-multiterm-and-set-current ()
  "Swap to different multi term buffer and set new current"
  (interactive)
  (multi-term-next)
  (setq current-terminal-buffer (buffer-name)))

(defun new-multiterm-and-htop ()
  "Create new multiterm buffer and run htop"
  (interactive)
  (create-multiterm-buffer)
  (process-send-string current-terminal-buffer "htop\n"))

(defun backward-terminal-search ()
  "Use backward search in terminal"
  (interactive)
  (term-send-raw-string "\C-r"))

;; multiterm
(global-set-key (kbd "C-c t c") 'create-or-show-small-terminal-multiterm)
(global-set-key (kbd "C-c t h") 'new-multiterm-and-htop)
(add-to-list 'term-bind-key-alist '("C-c C-j" . term-line-mode))
(add-to-list 'term-bind-key-alist '("C-c C-n" . next-multiterm-and-set-current))
(add-to-list 'term-bind-key-alist '("C-c C-r" . backward-terminal-search))

(defun create-or-show-small-terminal-eshell ()
  "Pop open a terminal"
  (interactive)
  (if (> (window-total-width) 200)
      (progn
        (split-window-horizontally)
        (windmove-right)))
  (if (or (not (boundp 'current-terminal-buffer))
          (not (get-buffer current-terminal-buffer)))
      (progn (eshell)
             (setq current-terminal-buffer (buffer-name)))
    (switch-to-buffer (get-buffer current-terminal-buffer))))

(global-set-key (kbd "C-c t t") 'create-or-show-small-terminal-eshell)

;; colorize compilation buffer
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(require 'send-to-process)

(defun send-line-to-terminal ()
  (interactive)
  (if (or (not (boundp 'current-terminal-buffer))
          (not (get-buffer-window current-terminal-buffer)))
      (message "No current terminal open")
    (send-line-region-to-process current-terminal-buffer)))

(defun send-buffer-to-terminal ()
  (interactive)
  (if (or (not (boundp 'current-terminal-buffer))
          (not (get-buffer-window current-terminal-buffer)))
      (message "No current terminal open")
    (send-buffer-region-to-process current-terminal-buffer)))

(global-set-key (kbd "C-c t e") 'send-line-to-terminal)
(global-set-key (kbd "C-c t k") 'send-buffer-to-terminal)

(setq-default sh-basic-offset 2)
(setq-default sh-indentation 2)

(provide 'setup-shell)
