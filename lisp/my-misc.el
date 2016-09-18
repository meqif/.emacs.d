;; Use normal tabs in makefiles
(add-hook 'makefile-mode-hook 'indent-tabs-mode)

;; Enable electric indent in all programming modes
(add-hook 'prog-mode-hook 'electric-indent-mode)

;; Enable rainbow delimiters in all programming modes
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; Follow highlighted occur results in original buffer
(add-hook 'occur-mode-hook #'next-error-follow-minor-mode)

;; Adjust the font settings of frame so Emacs can display emoji properly.
(defun darwin-set-emoji-font (frame)
  (when (eq system-type 'darwin)
    (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") frame 'prepend)))
(darwin-set-emoji-font nil)

(defmacro version>= (a b)
  `(not (version< ,a ,b)))

;; Don't hide the output of evaluated code
(setq eval-expression-print-level nil
      eval-expression-print-length nil)

(provide 'my-misc)
