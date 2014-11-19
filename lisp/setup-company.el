;; Misery loves this
(require 'company)
(add-hook 'prog-mode-hook 'company-mode)
(set 'company-idle-delay 0.1)

;; Add tern.js backend
(add-to-list 'company-backends 'company-tern)

(provide 'setup-company)
