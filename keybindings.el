;; Window switching
(windmove-default-keybindings) ;; Shift+direction

;; change command to meta, and ignore option to use weird Norwegian keyboard
;(setq mac-option-modifier 'super)
;(setq mac-command-modifier 'meta)
;(setq ns-function-modifier 'hyper)

;; Norwegian mac-keyboard alt-keys)
;(define-key key-translation-map (kbd "s-8") (kbd "["))
;(define-key key-translation-map (kbd "s-(") (kbd "{"))
;(define-key key-translation-map (kbd "s-9") (kbd "]"))
;(define-key key-translation-map (kbd "s-)") (kbd "}"))

(setq ns-function-modifier 'hyper)

; Let right alt work as usual
(setq ns-right-option-modifier 'none)

;; Portuguese mac-keyboard alt-keys)
(define-key key-translation-map (kbd "H-8") (kbd "["))
(define-key key-translation-map (kbd "H-(") (kbd "{"))
(define-key key-translation-map (kbd "H-9") (kbd "]"))
(define-key key-translation-map (kbd "H-)") (kbd "}"))

;; Fix keyboard in OSX
(define-key key-translation-map (kbd "M-8") (kbd "["))
(define-key key-translation-map (kbd "M-9") (kbd "]"))
(define-key key-translation-map (kbd "M-(") (kbd "{"))
(define-key key-translation-map (kbd "M-)") (kbd "}"))

;; Misc

;; Kill current buffer without prompting
(global-set-key (kbd "H-k") 'kill-this-buffer)

;; Comment or uncomment current line or region
(global-set-key (kbd "s-/") 'comment-or-uncomment-region-or-line)

(provide 'keybindings)
