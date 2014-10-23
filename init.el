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

;; Default directory
(setq default-directory "~/")

;; Packages
(require 'cask "~/homebrew/opt/cask/cask.el")
(cask-initialize)
(require 'pallet)

(require 'dash)
(eval-after-load "dash" '(dash-enable-font-lock))

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
(eval-after-load 'tex-mode '(require 'setup-latex))
(eval-after-load 'markdown-mode '(require 'setup-markdown))
(eval-after-load 'js2-mode '(require 'setup-javascript))
(eval-after-load 'rust-mode '(require 'setup-rust))
(eval-after-load 'org-mode '(require 'setup-org-mode))
(eval-after-load 'helm-bibtex '(require 'setup-helm-bibtex))

;; Load stuff on demand
(autoload 'flycheck-mode "setup-flycheck" nil t)
(autoload 'flyspell-mode "setup-flyspell" nil t)

;; Some files need their modes explicitly set
(require 'mode-mappings)

;; Misc
(require 'my-misc)

(when (version>= emacs-version "24.4")
  ;; Enable prettify symbols mode
  (global-prettify-symbols-mode +1)
  (add-hook 'js2-mode-hook
            (lambda ()
              (push '("function" . ?ƒ) prettify-symbols-alist)))
  )
