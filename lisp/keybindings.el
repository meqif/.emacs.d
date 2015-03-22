;; Window switching
(windmove-default-keybindings) ;; Shift+direction

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

;; Make C-a and evil's "0" jump to the indentation point at first, since that's
;; what I usually mean by "jump to beginning of the line". Doing it again does
;; jump to the beginning of the line.
(global-set-key (kbd "C-a") 'meqif/move-to-beginning-of-indentation)
(eval-after-load "evil"
  '(define-key evil-normal-state-map "0" 'meqif/move-to-beginning-of-indentation))

(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Quickly jump to scratch buffer
(global-set-key
 (kbd "s-t")
 #'(lambda ()
     (interactive)
     (switch-to-buffer (get-buffer-create "*scratch*"))))

(provide 'keybindings)
