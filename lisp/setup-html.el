(eval-after-load "sgml-mode"
  #'(progn
      ;; Enable tagedit
      (require 'tagedit)
      ;; Auto-close tags and other goodies
      (tagedit-add-experimental-features)
      (add-hook 'html-mode-hook (lambda () (tagedit-mode 1)))
      (add-hook 'mustache-mode (lambda () (tagedit-mode 1)))))

;; Enable Emmet in tag soup modes
(add-hook 'sgml-mode-hook 'emmet-mode)
(add-hook 'mustache-mode-hook 'emmet-mode)

(add-to-list 'auto-mode-alist '("\\.hjs\\'" . mustache-mode))

;; Don't autocompile SCSS, I usually have task runners doing that
(add-hook 'scss-mode-hook #'(lambda () (setq scss-compile-at-save nil)))

(provide 'setup-html)
