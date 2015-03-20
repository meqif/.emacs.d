;; Refuse to work with old Emacsen
(when (version< emacs-version "24.4")
  (error "This emacs is too old!"))

;; Disable mouse interface
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Disable splash screen
(setq inhibit-startup-message t)

;; Set path to dependencies
(setq lisp-dir (expand-file-name "lisp" user-emacs-directory))

;; Set up load path
(add-to-list 'load-path lisp-dir)

;; Keep emacs Custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" lisp-dir))
(load custom-file)

;; Bring some sanity in
(require 'sane-defaults)

;; Packages
(require 'cask "~/homebrew/opt/cask/cask.el")
(cask-initialize)
(require 'pallet)
(pallet-mode t)
(require 'use-package)

(use-package dash
  :config (dash-enable-font-lock))

;; Fix path
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; Functions
(require 'defuns)

;; Keybindings
(require 'keybindings)

;; Appearance
(require 'appearance)

;; Awesome project navigation
(use-package projectile
  :init
  (projectile-global-mode)
  (add-to-list 'projectile-globally-ignored-directories "node_modules"))

;; Load default smartparens configuration
(require 'smartparens-config)
(smartparens-global-mode 1)

;; Setup extensions
(require 'setup-evil)
(require 'setup-ido)
(require 'expand-region)
(require 'multiple-cursors)
(require 'ace-jump-mode)
(require 'helm-imenu)
(require 'setup-html)

;; Save a list of recent files visited
(use-package recentf
  :init
  (recentf-mode t)
  (setq recentf-max-saved-items 100)) ;; just 20 is too recent

;; Misery loves this
(use-package company
  :config
  (progn
    ;; Enable company mode for every programming major mode
    (add-hook 'prog-mode-hook 'company-mode)
    ;; Offer completions quickly
    (set 'company-idle-delay 0.1)

    ;; Add tern.js backend
    (add-to-list 'company-backends 'company-tern)

    ;; Fix lowercase candidates
    (use-package company-dabbrev-code
      :config
      (add-to-list 'company-dabbrev-code-modes 'rust-mode))))

;; Unique buffer names
(use-package uniquify
  ;; Make uniquify rename buffers like in path name notation
  :config (setq uniquify-buffer-name-style 'forward))

(use-package wgrep
  :config
  (progn
    (autoload 'wgrep-ag-setup "wgrep-ag")
    (add-hook 'ag-mode-hook 'wgrep-ag-setup)))

(use-package flycheck
  :config
  (setq-default flycheck-display-errors-delay 0.5))

(use-package org
  :init
  (progn
    ;; Don't truncate lines
    (setq org-startup-truncated nil
          org-capture-templates
          '(("t" "Todo" entry (file+headline "~/org/tasks.org" "Tasks")
             "* TODO %?\n  %i\n  %a")
            ("j" "Dissertation" entry (file+datetree "~/org/dissertation.org")
             "* %?\nEntered on %U\n  %i\n  %a")
            ("b" "Blog post idea" entry (file+headline "~/org/blog/post_ideas.org" "Ideas")
             "* %?")))
    ;; Enable wordwrap
    (add-hook 'org-mode-hook 'visual-line-mode)))

(use-package yasnippet
  :config
  (progn
    ;; Use only own snippets, do not use bundled ones
    (setq yas-snippet-dirs '("~/.emacs.d/snippets"))

    ;; Don't mess with the indentation
    (setq yas-indent-line 'fixed)

    ;; No need to be so verbose
    (setq yas-verbosity 1)

    ;; Snippets everywhere
    (yas-global-mode 1)

    (add-hook 'snippet-mode-hook
              (lambda ()
                ;; Temporarily disable required newline at the end of file
                ;; This fixes the problem with an extra newline when expanding snippets
                (set (make-local-variable 'require-final-newline) nil)))))

;; Language-specific setup files
(load-config 'c-mode 'setup-c
  'tex-mode      'setup-latex
  'markdown-mode 'setup-markdown
  'js2-mode      'setup-javascript
  'rust-mode     'setup-rust
  'helm-bibtex   'setup-helm-bibtex
  'flyspell      'setup-flyspell)

;; Some files need their modes explicitly set
(require 'mode-mappings)

;; Misc
(require 'my-misc)

(use-package aggressive-indent
  :config
  (progn
    ;; Enable agressive indent mode globally
    (global-aggressive-indent-mode)
    ;; Aggressive indentation doesn't work very well in rust
    (add-to-list 'aggressive-indent-excluded-modes 'rust-mode)))

;; Highlight excessively long lines
(require 'whitespace)
(setq whitespace-line-column 80)
(setq whitespace-style '(face lines-tail))
(add-hook 'prog-mode-hook 'whitespace-mode)

;; emacs-refactor
(add-hook 'prog-mode-hook 'emr-initialize)
(define-key prog-mode-map (kbd "M-RET") 'emr-show-refactor-menu)
