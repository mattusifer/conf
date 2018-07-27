(defun remove-newlines
    (str)
  (replace-regexp-in-string "\n\s*" "" str))

(defun dedupe-newlines
    (str)
  (replace-regexp-in-string "\n\s*\n" "\n" str))

(defun send-string-to-buffer (buffer-name str-to-send)
  (with-current-buffer buffer-name
    (let ((process (get-buffer-process (current-buffer))))
      (dolist (substring-to-eval (butlast (split-string str-to-send "\n")))
        (goto-char (process-mark process))
        (insert (remove-newlines substring-to-eval))
        (comint-send-input nil t)
        (sit-for 0.05)))))

(defun send-region-to-buffer (buffer-name remove-newline)
  "Send selected region to buffer"
  (when (use-region-p)
    (let*  ((region-str (buffer-substring-no-properties (region-beginning)
                                                        (region-end)))
            (str-to-eval (if remove-newline
                             (remove-newlines region-str)
                           (dedupe-newlines region-str))))
      (send-string-to-buffer buffer-name str-to-eval))))

(defun send-buffer-region-to-buffer (buffer-name)
  "will evaluate entire buffer or a region (with newlines) if it is selected"
  (if (use-region-p)
      (send-region-to-buffer buffer-name nil)
    (send-string-to-buffer buffer-name (dedupe-newlines (buffer-string)))))

(defun send-line-region-to-buffer (buffer-name)
  "will evaluate a single line or a region (without newlines) if it is highlighted"
  (if (use-region-p)
      (send-region-to-buffer buffer-name t)
    (send-string-to-buffer buffer-name (thing-at-point 'line))))

(provide 'send-to-buffer)
