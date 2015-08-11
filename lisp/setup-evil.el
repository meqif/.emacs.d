(require 'evil)
(require 'my-misc)
(require 'dash)

;; We need more evil leadership, your wickedness, Sir.
(global-evil-leader-mode)

(evil-leader/set-key
  "f" 'projectile-find-file
  "p" 'browse-kill-ring
  "b" 'ibuffer
  "r" 'helm-recentf
  "g" 'magit-status)

(evil-leader/set-key-for-mode 'latex-mode
  "s" 'flyspell-buffer
  "t" #'(lambda () (interactive) (TeX-insert-macro "todo"))
  "cc" 'TeX-command-master
  "cv" 'TeX-view)

(defhydra hydra-cargo (:color blue)
  "cargo"
  ("c" (lambda () (interactive)
         (save-buffer)
         (compile "cargo build"))
   "build")
  ("tt" (lambda () (interactive)
          (save-buffer)
          (compile "cargo test"))
   "test all")
  ("tf" (lambda () (interactive)
          (save-buffer)
          (compile (concat "cargo test " (meqif/which-function))))
   "test current function")
  ("b" (lambda () (interactive)
         (save-buffer)
         (compile "cargo benchmark"))
   "benchmark all")
  ("C" (lambda () (interactive)
         (compile "cargo clean"))
   "clean"))
(evil-leader/set-key-for-mode 'rust-mode "c" #'hydra-cargo/body)
(evil-leader/set-key-for-mode 'toml-mode "c" #'hydra-cargo/body)

(evil-leader/set-key-for-mode 'org-mode
  "ce" #'org-export-dispatch)

;; Easy mark popping
(defun meqif/pop-mark ()
  (interactive)
  (set-mark-command '(4)))
(evil-leader/set-key
  "\\" #'meqif/pop-mark)

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
    '(compilation-mode
      finder-mode
      flycheck-error-list-mode
      messages-buffer-mode
      neotree-mode
      paradox-menu-mode
      special-mode
      term-mode
      ;; Magit modes
      git-rebase-mode
      magit-auto-revert-mode
      magit-blame-mode
      magit-branch-manager-mode
      magit-cherry-mode
      magit-commit-mode
      magit-diff-mode
      magit-key-mode
      magit-log-mode
      magit-popup-mode
      magit-pre-key-mode
      magit-process-mode
      magit-process-set-mode
      magit-process-unset-mode
      magit-reflog-mode
      magit-status-mode
      magit-topgit-mode
      magit-wazzup-mode
      magit-wip-mode
      magit-wip-save-mode)
  (add-to-list 'evil-emacs-state-modes it))

(provide 'setup-evil)
