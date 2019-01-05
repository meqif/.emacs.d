;; Use normal tabs in makefiles
(add-hook 'makefile-mode-hook 'indent-tabs-mode)

;; Enable electric indent in all programming modes
(add-hook 'prog-mode-hook 'electric-indent-mode)

;; Don't hide the output of evaluated code
(setq eval-expression-print-level nil
      eval-expression-print-length nil)

;; Recenter to top first when calling `recenter-top-bottom'
(setq recenter-positions '(top middle bottom))

;; Use smaller steps for text scaling
(setq text-scale-mode-step 1.1)

;; Start with a clean scratch buffer
(setq initial-major-mode 'fundamental-mode
      initial-scratch-message nil)

(provide 'my-misc)
