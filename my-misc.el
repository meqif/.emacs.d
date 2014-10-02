;; Highlight the current entry in browse-kill-ring
(eval-after-load "browse-kill-ring"
  '(setq browse-kill-ring-highlight-current-entry t))

;; Enable diff indication on the fringe
(add-hook 'prog-mode-hook 'turn-on-diff-hl-mode)
(add-hook 'vc-dir-mode-hook 'turn-on-diff-hl-mode)

;; Use normal tabs in makefiles
(add-hook 'makefile-mode-hook 'indent-tabs-mode)

(provide 'my-misc)
