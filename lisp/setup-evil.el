;;; setup-evil.el -*- lexical-binding: t; -*-

(use-package evil)
(require 'my-misc)
(require 'dash)

;; Improve symbol search to consider snake case symbols
(setq-default evil-symbol-word-search 'symbol)

;; Evil surround is a must
(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

;; Enable evil mode
(evil-mode 1)

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

;; Make Esc quit everywhere
(--each (list evil-normal-state-map evil-visual-state-map)
  (define-key it [escape] 'keyboard-quit))
(--each
    (list minibuffer-local-map
          minibuffer-local-ns-map
          minibuffer-local-completion-map
          minibuffer-local-must-match-map
          minibuffer-local-isearch-map
          minibuffer-local-filename-completion-map
          minibuffer-local-shell-command-map)
  (define-key it [escape] 'minibuffer-keyboard-quit))

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
(when (version>= emacs-version "24.4")
  (define-key evil-insert-state-map [remap newline] 'newline)
  (define-key evil-insert-state-map [remap newline-and-indent] 'newline-and-indent))

;; Some more modes that should be in emacs mode
(--each
    '(cider-stacktrace-mode
      compilation-mode
      finder-mode
      flycheck-error-list-mode
      neotree-mode
      paradox-menu-mode
      paradox-commit-list-mode
      profiler-report-mode
      racer-help-mode
      special-mode
      term-mode
      TeX-error-overview-mode)
  (add-to-list 'evil-emacs-state-modes it))

;; Better navigation for modes
(evil-add-hjkl-bindings occur-mode-map 'emacs)
(evil-add-hjkl-bindings profiler-report-mode-map 'emacs)
(evil-add-hjkl-bindings paradox-commit-list-mode-map 'emacs)

(eval-after-load 'ibuffer
  '(progn
     ;; Remove ibuffer from evil emacs state modes
     (delete 'ibuffer-mode evil-emacs-state-modes)
     ;; Use the standard ibuffer bindings as a base
     (set-keymap-parent
      (evil-get-auxiliary-keymap ibuffer-mode-map 'normal t)
      (assq-delete-all 'menu-bar (copy-keymap ibuffer-mode-map)))
     (evil-define-key 'normal ibuffer-mode-map "j" 'ibuffer-forward-line)
     (evil-define-key 'normal ibuffer-mode-map "k" 'ibuffer-backward-line)
     (evil-define-key 'normal ibuffer-mode-map "J" 'ibuffer-jump-to-buffer)))
(evil-define-key 'normal messages-buffer-mode-map "q" 'quit-window)
(evil-define-key 'normal dired-mode-map "gr" 'revert-buffer)

;; Undo like Vim: everything done during an insert session will be undone in one
;; step.
;; (setq evil-want-fine-undo 'no)

;; Let emacs bindings for M-. and M-, take over
(define-key evil-normal-state-map (kbd "M-.") nil)
(define-key evil-normal-state-map (kbd "M-,") nil)

(provide 'setup-evil)
;;; setup-evil.el ends here
