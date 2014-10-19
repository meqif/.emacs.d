(add-hook 'c-mode-hook (lambda ()
  (setq c-basic-offset 4 c-default-style "linux")
  (setq tab-width 4 indent-tabs-mode t)
  ;; Automatically indent on RET
  (define-key c-mode-base-map (kbd "RET") 'newline-and-indent)
  ;; Backspace also deletes a whole indentation level
  (setq c-backspace-function 'backward-delete-char)
  
  ;; Enable auto-complete
  (auto-complete-mode)
))

(provide 'setup-c)
