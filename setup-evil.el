(require 'evil)

;; We need more evil leadership, your wickedness, Sir.
(global-evil-leader-mode)

;; Quick shortcut to projectile-find-file
(evil-leader/set-key "f" 'projectile-find-file)

;; Evil surround is a must
(global-evil-surround-mode 1)

;; Enable evil mode
(evil-mode 1)

; Map escape to cancel (like C-g)...
;; (define-key isearch-mode-map [escape] 'isearch-abort)   ;; isearch
;; (global-set-key [escape] 'keyboard-escape-quit)         ;; everywhere else

;; esc quits
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

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
