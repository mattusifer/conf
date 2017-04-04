(setq yas-snippet-dirs
      '("~/.emacs.d/snippets"                  ;; personal snippets
        "~/.emacs.d/vendor/yasnippet-snippets" ;; the default collection
        ))

(yas-global-mode 1)

(provide 'setup-yasnippet)
