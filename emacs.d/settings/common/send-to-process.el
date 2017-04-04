(defun remove-newlines
    (str)
  (replace-regexp-in-string "\n\s*" "" str))

(defun dedupe-newlines
    (str)
  (replace-regexp-in-string "\n\s*\n" "\n" str))

(defun send-region-to-process (process-name remove-newline)
  "Send selected region to process"
  (when (use-region-p)
    (let*  ((region-str (buffer-substring-no-properties (region-beginning)
                                                       (region-end)))
            (str-to-eval (if remove-newline
                             (remove-newlines region-str)
                           (dedupe-newlines region-str))))
      (progn (process-send-string process-name str-to-eval)
             (process-send-string process-name "\n")))))

(defun send-buffer-region-to-process (process-name)
  "will evaluate entire buffer or a region (with newlines) if it is selected"
  (if (use-region-p)
      (send-region-to-process process-name nil)
    (process-send-string
     process-name
     (dedupe-newlines (buffer-string)))))

(defun send-line-region-to-process (process-name)
  "will evaluate a single line or a region (without newlines) if it is highlighted"
  (if (use-region-p)
      (send-region-to-process process-name t)
    (process-send-string process-name (thing-at-point 'line))))

(provide 'send-to-process)
