(require 'evil)

;; We need more evil leadership, your wickedness, Sir.
(global-evil-leader-mode)

;; Quick shortcut to projectile-find-file
(evil-leader/set-key "f" 'projectile-find-file)

;; Evil surround is a must
(global-evil-surround-mode 1)

;; Enable evil mode
(evil-mode 1)

;; Make Esc quit
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

;; Disable evil mode in terminal
(add-hook 'term-mode-hook 'evil-emacs-state)

(provide 'setup-evil)
