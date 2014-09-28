(setq-default js2-global-externs
              '("module" "export" "require" "describe" "it" "before" "after"))

;; Let flycheck handle parse errors
(setq-default js2-mode-show-parse-errors nil)
(setq-default js2-strict-missing-semi-warning nil)
(setq-default js2-strict-trailing-comma-warning t) ;; jshint does not warn about this now for some reason

(add-hook 'js2-mode-hook (lambda () (flycheck-mode 1)))

;; Use lambda for anonymous functions
(font-lock-add-keywords
 'js2-mode `(("\\(function\\) *("
              (0 (progn (compose-region (match-beginning 1)
                                        (match-end 1) "\u0192") ; f
                                        ;; (match-end 1) "\u03bb") ; lambda
                        nil)))))


(provide 'setup-javascript)
