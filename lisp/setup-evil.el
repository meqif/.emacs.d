(use-package evil)
(require 'my-misc)
(require 'dash)

;; We need more evil leadership, your wickedness, Sir.
(use-package evil-leader
  :config
  (global-evil-leader-mode)

  ;; Global evil leader shortcuts
  (evil-leader/set-key
    "f" 'projectile-or-counsel-find-file
    "p" 'counsel-yank-pop
    "b" 'ibuffer
    "r" 'counsel-recentf
    "l" 'avy-goto-line
    "g" 'magit-status
    "\\" 'meqif/pop-mark)

  ;; LaTeX leader shortcuts
  (evil-leader/set-key-for-mode 'latex-mode
    "s" 'flyspell-buffer
    "t" #'(lambda () (interactive) (TeX-insert-macro "todo"))
    "cc" 'TeX-command-master
    "cv" 'TeX-view)

  ;; Hydra for rust's cargo
  (defhydra hydra-cargo (:color blue :columns 4)
    "cargo"
    ("c" (lambda () (interactive)
           (save-buffer)
           (compile "rustc --version && cargo build"))
     "build")
    ("tt" (lambda () (interactive)
            (save-buffer)
            (compile "rustc --version && cargo test"))
     "test all")
    ("tf" (lambda () (interactive)
            (save-buffer)
            (compile (concat "rustc --version && cargo test " (meqif/which-function))))
     "test current function")
    ("b" (lambda () (interactive)
           (save-buffer)
           (compile "rustc --version && cargo benchmark"))
     "benchmark all")
    ("C" (lambda () (interactive)
           (compile "cargo clean"))
     "clean")
    ("d" (lambda () (interactive)
           (save-buffer) (compile "rustc --version && cargo doc"))
     "build documentation")
    ("r" (lambda () (interactive)
           (save-buffer) (compile "rustc --version && cargo run"))
     "run"))
  (evil-leader/set-key-for-mode 'rust-mode "c" #'hydra-cargo/body)
  (evil-leader/set-key-for-mode 'toml-mode "c" #'hydra-cargo/body)

  ;; Org-mode leader shortcuts
  (evil-leader/set-key-for-mode 'org-mode
    "ce" #'org-export-dispatch))

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
          minibuffer-local-must-match-filename-map
          minibuffer-local-isearch-map
          minibuffer-local-filename-completion-map
          minibuffer-local-filename-must-match-map
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
    '(compilation-mode
      finder-mode
      flycheck-error-list-mode
      messages-buffer-mode
      neotree-mode
      paradox-menu-mode
      paradox-commit-list-mode
      profiler-report-mode
      racer-help-mode
      special-mode
      term-mode
      TeX-error-overview-mode
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

;; Better navigation for modes
(evil-add-hjkl-bindings occur-mode-map 'emacs)
(evil-add-hjkl-bindings profiler-report-mode-map 'emacs)
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

;; Undo like Vim: everything done during an insert session will be undone in one
;; step.
;; (setq evil-want-fine-undo 'no)

(provide 'setup-evil)
