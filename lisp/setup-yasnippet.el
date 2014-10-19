;; Use only own snippets, do not use bundled ones
(setq yas-snippet-dirs '("~/.emacs.d/snippets"))

;; Don't mess with the indentation
(setq yas-indent-line 'fixed)

;; Snippets everywhere
(yas-global-mode 1)

;; No need to be so verbose
(setq yas-verbosity 1)

(add-hook 'snippet-mode-hook (lambda ()
    ;; Temporarily disable required newline at the end of file
    ;; This fixes the problem with an extra newline when expanding snippets
    (set (make-local-variable 'require-final-newline) nil)
))

(provide 'setup-yasnippet)
