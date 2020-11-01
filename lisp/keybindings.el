;;; keybindings.el -*- lexical-binding: t; -*-

;; Window switching
(windmove-default-keybindings) ;; Shift+direction

(setq ns-function-modifier 'hyper)

; Let right alt work as usual
(setq ns-right-option-modifier 'none)

;; For emacs-mac
(when (eq window-system 'mac)
  (progn
    (setq mac-pass-command-to-system nil
          mac-option-modifier 'meta
          mac-command-modifier 'super
          mac-right-option-modifier 'none)
    (global-set-key (kbd "s-h") 'meqif/hide-window)
    (global-set-key (kbd "s-w") 'delete-frame)
    (global-set-key (kbd "s-s") 'save-buffer)
    (global-set-key (kbd "s-v") 'yank)
    (global-set-key (kbd "s-c") 'kill-ring-save)
    (global-set-key (kbd "s-l") 'goto-line)
    (global-set-key (kbd "s-z") 'undo)
    (global-set-key (kbd "s-a") 'mark-whole-buffer)))

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

;; Comment or uncomment current line or region
(global-set-key (kbd "s-/") 'comment-or-uncomment-region-or-line)

;; Make C-a and evil's "0" jump to the indentation point at first, since that's
;; what I usually mean by "jump to beginning of the line". Doing it again does
;; jump to the beginning of the line.
(global-set-key (kbd "C-a") 'meqif/move-to-beginning-of-indentation)
(eval-after-load "evil"
  '(define-key evil-normal-state-map "0" 'meqif/move-to-beginning-of-indentation))

;; Quickly jump to scratch buffer
(global-set-key
 (kbd "s-t")
 #'(lambda ()
     (interactive)
     (switch-to-buffer (get-buffer-create "*scratch*"))))

;; Get nicer output when evaluating expressions
(global-set-key (kbd "M-:") 'pp-eval-expression)

(provide 'keybindings)
;;; keybindings.el ends here
