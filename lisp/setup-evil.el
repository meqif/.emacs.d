(require 'evil)

;; We need more evil leadership, your wickedness, Sir.
(global-evil-leader-mode)

;; Quick shortcut to projectile-find-file
(evil-leader/set-key "f" 'projectile-find-file)

;; Quick shortcut to browse-kill-ring
(evil-leader/set-key "p" 'browse-kill-ring)

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

;; Make ':bd' kill the buffer but not close the window.
;;
;; This function is exactly the same as `evil-delete-buffer` with
;; the code that closes the windows deleted.
;;
;; Source: https://lists.ourproject.org/pipermail/implementations-list/2013-March/001807.html
;; Credit: Frank Fischer
(evil-define-command evil-delete-buffer-keep-windows
  (buffer &optional bang)
  (interactive "<b><!>")
  (with-current-buffer (or buffer (current-buffer))
    (when bang
      (set-buffer-modified-p nil)
      (dolist (process (process-list))
        (when (eq (process-buffer process)
                  (current-buffer))
          (set-process-query-on-exit-flag process nil))))
    (if (and (fboundp 'server-edit)
             (boundp 'server-buffer-clients)
             server-buffer-clients)
        (server-edit)
      (kill-buffer nil))))
(evil-ex-define-cmd "bd[elete]" 'evil-delete-buffer-keep-windows)

;; Work-around for broken indentation in 24.4
(require 'my-misc)
(when (version>= emacs-version "24.4")
  (define-key evil-insert-state-map [remap newline] 'newline)
  (define-key evil-insert-state-map [remap newline-and-indent] 'newline-and-indent))

(provide 'setup-evil)
