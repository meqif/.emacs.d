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

(use-package smartparens
  :diminish smartparens-mode
  :init
  (progn
    (smartparens-global-mode t)
    ;; Load default smartparens configuration
    (use-package smartparens-config)))

;; Setup extensions
(require 'setup-evil)

;; Enable IDO mode
(when (> emacs-major-version 21)
  (ido-mode t)
  (setq ido-enable-prefix nil
        ido-enable-flex-matching t
        ido-create-new-buffer 'always
        ido-use-filename-at-point nil
        ido-max-prospects 10))

(use-package helm-imenu
  :bind ("s-r" . helm-imenu))

(use-package ace-jump-mode
  :bind ("C-c SPC" . ace-jump-mode)
  :config
  (progn
    (define-key global-map (kbd "C-c SPC") 'ace-jump-mode)))

(use-package expand-region
  :bind ("C-ç" . er/expand-region))

(use-package multiple-cursors
  :bind (("C-º" . mc/mark-next-like-this)
         ("C-ª" . mc/mark-previous-like-this)
         ("C-c C-º" . mc/mark-all-like-this)))

;; Save a list of recent files visited
(use-package recentf
  :init
  (recentf-mode t)
  (setq recentf-max-saved-items 100)) ;; just 20 is too recent

;; Misery loves this
(use-package company
  :diminish company-mode
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
(load-config
  'tex-mode      'setup-latex
  'markdown-mode 'setup-markdown
  'js2-mode      'setup-javascript
  'helm-bibtex   'setup-helm-bibtex)

(use-package cc-mode
  :init
  (add-hook 'c-mode-hook
            #'(lambda ()
                (setq c-basic-offset 4
                      c-default-style "linux")
                ;; Automatically indent on RET
                (define-key c-mode-base-map (kbd "RET") 'newline-and-indent)
                ;; Backspace also deletes a whole indentation level
                (setq c-backspace-function 'backward-delete-char))))

(use-package sgml-mode
  :config
  (progn
    ;; Enable tagedit
    (use-package tagedit)
    ;; Auto-close tags and other goodies
    (tagedit-add-experimental-features)
    (add-hook 'html-mode-hook (lambda () (tagedit-mode 1)))
    (add-hook 'mustache-mode (lambda () (tagedit-mode 1)))))

(use-package emmet-mode
  :config
  (progn
    ;; Enable Emmet in tag soup modes
    (add-hook 'sgml-mode-hook 'emmet-mode)
    (add-hook 'mustache-mode-hook 'emmet-mode)))

(use-package mustache-mode
  :mode "\\.hjs\\'")

(use-package scss-mode
  ;; Don't autocompile SCSS, I usually have task runners doing that
  :config (setq scss-compile-at-save nil))

(use-package rust-mode
  :config
  (progn
    (add-hook 'rust-mode-hook
              (lambda ()
                ;; Enable on-the-fly syntax checking
                (flycheck-mode 1)
                ;; Fix paths where to look for libraries
                (add-to-list 'flycheck-rust-library-path
                             (concat (projectile-project-root) "target"))
                (add-to-list 'flycheck-rust-library-path
                             (concat (projectile-project-root) "target/release"))
                ;; Rust has different rules for too long lines
                (setq-local fill-column 101)
                (setq-local whitespace-line-column 100)
                ;; Reload whitespace mode to make the previous change effective
                (whitespace-mode -1)
                (whitespace-mode 1)))

    ;; Add brackets to smartparens pair list
    (sp-local-pair 'rust-mode "<" ">")))

(use-package flyspell
  :init
  (progn
    ;; Use Aspell for spellcheck
    (setq ispell-program-name "~/homebrew/bin/aspell")
    (setq ispell-list-command "--list")

    ;; Default language is Portuguese.
    (setq ispell-dictionary "pt_PT")

    ;; Flyspell messages slow down the spellchecking process
    (setq flyspell-issue-message-flag nil)))

(use-package js2-mode
  :mode ("\\.js$" "\\.json$"))

;; Easier kill-ring viewing
(use-package browse-kill-ring
  ;; Highlight the current entry in browse-kill-ring
  :config (setq browse-kill-ring-highlight-current-entry t))

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
(use-package whitespace
  :config
  (progn
    (setq whitespace-line-column 80
          whitespace-style '(face lines-tail))
    (add-hook 'prog-mode-hook 'whitespace-mode)))

(use-package undo-tree
  ;; Show timestamps
  :config (setq undo-tree-visualizer-timestamps t))

(use-package enh-ruby-mode
  ;; Don't deep indent arrays and hashes
  :config (setq enh-ruby-deep-ident-paren nil))

;; Better package management
(use-package paradox
  ;; Always update in background
  :config (setq paradox-execute-asynchronously t))

;; NERDtree clone
(use-package neotree
  :config (setq neo-theme 'nerd))

;; Better interactive search
(use-package swiper
  :init (global-set-key (kbd "C-s") 'swiper))

;; Enable paredit for Emacs Lisp
(add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
