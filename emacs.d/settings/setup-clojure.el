;; enable paredit for Clojure
(add-hook 'clojure-mode-hook 'paredit-mode)

;; working with camel-case tokens
(add-hook 'clojure-mode-hook 'subword-mode)

;; extra syntax highlighting
(require 'clojure-mode-extra-font-locking)

;; syntax highlighting for midje
(add-hook 'clojure-mode-hook
          (lambda ()
            (setq inferior-lisp-program "lein repl")
            (font-lock-add-keywords
             nil
             '(("(\\(facts?\\)"
                (1 font-lock-keyword-face))
               ("(\\(background?\\)"
                (1 font-lock-keyword-face))))
            (define-clojure-indent (fact 1))
            (define-clojure-indent (facts 1))))

;;;;;;;;;;
;; cider

;; provide minibuffer documentation for code in the repl
(add-hook 'cider-mode-hook 'eldoc-mode)

;; show cider error buffer and switch to it
(setq cider-show-error-buffer t)
(setq cider-auto-select-error-buffer t)

;; cider history
(setq cider-repl-history-file (expand-file-name "cider-history" user-emacs-directory))
(setq cider-repl-wrap-history t)

;; enable paredit in the REPL
(add-hook 'cider-repl-mode-hook 'paredit-mode)

;; clojure mode for other extensions
(add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.boot$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.cljs.*$" . clojure-mode))
(add-to-list 'auto-mode-alist '("lein-env" . enh-ruby-mode))

;; fix 'Protocol Family' issue in cider
;; https://github.com/clojure-emacs/cider/issues/1960
(setq cider-lein-parameters "repl :headless :host localhost")

(provide 'setup-clojure)
