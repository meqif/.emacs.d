;; Highlight the current entry in browse-kill-ring
(eval-after-load "browse-kill-ring"
  '(setq browse-kill-ring-highlight-current-entry t))

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

;; Enable smex
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)

;; Make imenu rescan automatically
(setq imenu-auto-rescan t)

;; Make imenu work on larger files
(setq imenu-auto-rescan-maxout 120000)

;; Split undo-tree side-by-side, like decent people do.
(defadvice undo-tree-visualize (around undo-tree-split-side-by-side activate)
  "Split undo-tree side-by-side"
  (let ((split-height-threshold nil)
        (split-width-threshold 0))
  ad-do-it))

(defun meqif/cycle-windows ()
  "Cycle between windows in the same frame"
  (interactive)
  (other-window 1 t))
(global-set-key (kbd "C-x C-\\") 'meqif/cycle-windows)
(global-set-key (kbd "H-\\") 'meqif/cycle-windows)

;; Stop org-mode from replacing my window-moving keys
;; Has to be defined before loading org-mode
(setq org-replace-disputed-keys t)

(defmacro version>= (a b)
  `(not (version< ,a ,b)))

;; Don't hide the output of evaluated code
(setq eval-expression-print-level nil
      eval-expression-print-length nil)

(provide 'my-misc)
