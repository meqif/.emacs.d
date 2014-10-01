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

;; Keybindings
(require 'keybindings)

;; Appearance
(require 'appearance)

;; Plugins

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
(require 'uniquify)

;; Language-specific setup files
(eval-after-load 'c-mode '(require 'setup-c))
(eval-after-load 'LaTeX-mode '(require 'setup-latex))
(eval-after-load 'markdown-mode '(require 'setup-markdown))
(eval-after-load 'js2-mode '(require 'setup-javascript))
(eval-after-load 'rust-mode '(require 'setup-rust))

;; Load stuff on demand
(autoload 'flycheck-mode "setup-flycheck" nil t)
(autoload 'flyspell-mode "setup-flyspell" nil t)

;; Highlight ag's search results
(setq ag-highlight-search t)

;; Enable smex
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)

;; Make imenu rescan automatically
(setq imenu-auto-rescan t)

;; Make imenu work on larger files
(setq imenu-auto-rescan-maxout 120000)

;; Split undo-tree side-by-side, like decent people do.
(defadvice undo-tree-visualize (around undo-tree-split-side-by-side activate)
  "Split undo-tree side-by-side"
  (let ((split-height-threshold nil)
        (split-width-threshold 0))
  ad-do-it))

;; Some files need their modes explicitly set
(require 'mode-mappings)

;; Enable electric indent in all programming modes
(add-hook 'prog-mode-hook 'electric-indent-mode)

;; Enable rainbow delimiters in all programming modes
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; Enable Emmet in tag soup modes
(add-hook 'sgml-mode-hook 'emmet-mode)

;; Utils
(require 'setup-utils)

;; Misc

;; Highlight the current entry in browse-kill-ring
(eval-after-load "browse-kill-ring"
  '(setq browse-kill-ring-highlight-current-entry t))
