;; Use normal tabs in makefiles
(add-hook 'makefile-mode-hook 'indent-tabs-mode)

;; Enable electric indent in all programming modes
(add-hook 'prog-mode-hook 'electric-indent-mode)

;; Enable rainbow delimiters in all programming modes
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; Tweaks for modes that show search results
(--each
    '(occur-mode-hook ag-mode-hook)
  (progn
    ;; Use visual line mode
    (add-hook it #'visual-line-mode)))

(defmacro version>= (a b)
  `(not (version< ,a ,b)))

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
