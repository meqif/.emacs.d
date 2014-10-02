;; Highlight the current entry in browse-kill-ring
(eval-after-load "browse-kill-ring"
  '(setq browse-kill-ring-highlight-current-entry t))

;; Use normal tabs in makefiles
(add-hook 'makefile-mode-hook 'indent-tabs-mode)

(provide 'my-misc)
