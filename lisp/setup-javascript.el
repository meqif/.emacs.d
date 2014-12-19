;; Load js2-refactor
(require 'js2-refactor)

(setq-default js2-global-externs
              '("module" "export" "require" "describe" "it" "before" "after"))

;; Let flycheck handle parse errors
(setq-default js2-mode-show-parse-errors nil)
(setq-default js2-strict-missing-semi-warning nil)
(setq-default js2-strict-trailing-comma-warning t) ;; jshint does not warn about this now for some reason

(add-hook 'js2-mode-hook (lambda () (flycheck-mode 1)))

;; Use lambda for anonymous functions
;; (font-lock-add-keywords
;;  'js2-mode `(("\\(function\\) *("
;;               (0 (progn (compose-region (match-beginning 1)
;;                                         (match-end 1) "\u0192") ; f
;;                                         ;; (match-end 1) "\u03bb") ; lambda
;;                         nil)))))
(add-hook 'js2-mode-hook
          (lambda ()
            ;; Use symbol for anonymous functions
            (push '("function" . ?ƒ) prettify-symbols-alist)
            ;; Enable ternjs (requires tern to be installed through npm)
            (tern-mode t)))

;; Make JSON files indent with two spaces, Node Style
;; (add-hook 'js2-mode-hook
;;           (lambda ()
;;             (when (and (stringp buffer-file-name)
;;                        (string-match "\\.json\\'" buffer-file-name)))
;;             (setq js2-basic-offset 2)
;;             (setq tab-width 2)))

(provide 'setup-javascript)
