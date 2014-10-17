;; Disable mouse interface
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Disable splash screen
(setq inhibit-startup-message t)

;; Dependencies
(add-to-list 'load-path user-emacs-directory)

;; Keep emacs Custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; Bring some sanity in
(require 'sane-defaults)

;; Default directory
(setq default-directory "~/")

;; Packages
(require 'cask "~/homebrew/opt/cask/cask.el")
(cask-initialize)
(require 'pallet)

;; Fix path
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; Functions
(require 'defuns)

;; Keybindings
(require 'keybindings)

;; Appearance
(require 'appearance)

; (require 'setup-package)
; no longer needed because I'm using cask & pallet

;; Awesome project navigation
(require 'setup-projectile)

;; Load default smartparens configuration
(require 'smartparens-config)
(smartparens-global-mode 1)

;; Setup extensions
(require 'setup-evil)
(require 'setup-yasnippet)
(require 'setup-ido)
(require 'setup-recentf)
(require 'setup-wgrep)
(require 'uniquify)
(require 'expand-region)
(require 'multiple-cursors)

;; Language-specific setup files
(eval-after-load 'c-mode '(require 'setup-c))
(eval-after-load 'LaTeX-mode '(require 'setup-latex))
(eval-after-load 'markdown-mode '(require 'setup-markdown))
(eval-after-load 'js2-mode '(require 'setup-javascript))
(eval-after-load 'rust-mode '(require 'setup-rust))
(eval-after-load 'org-mode '(require 'setup-org-mode))

;; Load stuff on demand
(autoload 'flycheck-mode "setup-flycheck" nil t)
(autoload 'flyspell-mode "setup-flyspell" nil t)

;; Some files need their modes explicitly set
(require 'mode-mappings)

;; Misc
(require 'my-misc)

;; Some more modes that should be in emacs mode
(add-to-list 'evil-emacs-state-modes 'flycheck-error-list-mode)
