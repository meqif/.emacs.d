;; Enable diff indication on the fringe
(add-hook 'prog-mode-hook 'turn-on-diff-hl-mode)
(add-hook 'vc-dir-mode-hook 'turn-on-diff-hl-mode)

;; Use normal tabs in makefiles
(add-hook 'makefile-mode-hook 'indent-tabs-mode)

;; Enable electric indent in all programming modes
(add-hook 'prog-mode-hook 'electric-indent-mode)

;; Enable rainbow delimiters in all programming modes
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; Highlight ag's search results
(setq ag-highlight-search t)

(defun meqif/cycle-windows ()
  "Cycle between windows in the same frame"
  (interactive)
  (other-window 1 t))
(global-set-key (kbd "C-x C-\\") 'meqif/cycle-windows)
(global-set-key (kbd "H-\\") 'meqif/cycle-windows)

(defmacro version>= (a b)
  `(not (version< ,a ,b)))

;; Don't hide the output of evaluated code
(setq eval-expression-print-level nil
      eval-expression-print-length nil)

(provide 'my-misc)
