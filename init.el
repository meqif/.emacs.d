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
(require 'setup-yasnippet)
(require 'setup-ido)
(require 'setup-recentf)
(require 'expand-region)
(require 'multiple-cursors)
(require 'ace-jump-mode)
(require 'helm-imenu)
(require 'setup-html)
(require 'setup-company)

;; Unique buffer names
(use-package uniquify
  ;; Make uniquify rename buffers like in path name notation
  :config (setq uniquify-buffer-name-style 'forward))

(use-package wgrep
  :config
  (progn
    (autoload 'wgrep-ag-setup "wgrep-ag")
    (add-hook 'ag-mode-hook 'wgrep-ag-setup)))

;; Language-specific setup files
(load-config 'c-mode 'setup-c
  'tex-mode      'setup-latex
  'markdown-mode 'setup-markdown
  'js2-mode      'setup-javascript
  'rust-mode     'setup-rust
  'org           'setup-org-mode
  'helm-bibtex   'setup-helm-bibtex
  'flycheck      'setup-flycheck
  'flyspell      'setup-flyspell)

;; Some files need their modes explicitly set
(require 'mode-mappings)

;; Misc
(require 'my-misc)

(global-aggressive-indent-mode)

;; Aggressive indentation doesn't work very well in rust
(add-to-list 'aggressive-indent-excluded-modes 'rust-mode)

;; Highlight excessively long lines
(require 'whitespace)
(setq whitespace-line-column 80)
(setq whitespace-style '(face lines-tail))
(add-hook 'prog-mode-hook 'whitespace-mode)

;; emacs-refactor
(add-hook 'prog-mode-hook 'emr-initialize)
(define-key prog-mode-map (kbd "M-RET") 'emr-show-refactor-menu)
