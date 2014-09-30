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

;; Show keystrokes in progress
(setq echo-keystrokes 0.1)

;; Tabs
(setq-default tab-width 4)

;; Disable backup
(setq backup-inhibited t)

;; Disable auto save
(setq auto-save-default nil)

;; Auto refresh buffers
(global-auto-revert-mode 1)

;; Answering just 'y' or 'n' will do
(defalias 'yes-or-no-p 'y-or-n-p)

;; UTF-8 please
(setq locale-coding-system 'utf-8) ; pretty
(set-terminal-coding-system 'utf-8) ; pretty
(set-keyboard-coding-system 'utf-8) ; pretty
(set-selection-coding-system 'utf-8) ; please
(prefer-coding-system 'utf-8) ; with sugar on top

;; Always display line and column numbers
(setq line-number-mode t)
(setq column-number-mode t)

;; Lines should be 80 characters wide, not 72
;; Make fill-paragraph (Alt-Q and gq) respect the line width
(set-default 'fill-column 80)

;; Never insert tabs
(set-default 'indent-tabs-mode nil)

;; Default directory
(setq default-directory "~/")

;; Use Aspell for spellcheck
(setq ispell-program-name "~/homebrew/bin/aspell")
(setq ispell-list-command "--list")

;; Default language is Portuguese.
(setq ispell-dictionary "pt_PT")

;; Flyspell messages slow down the spellchecking process
(setq flyspell-issue-message-flag nil)

;; Save a list of recent files visited. (open recent file with C-x f)
(recentf-mode 1)
(setq recentf-max-saved-items 100) ;; just 20 is too recent

;; Save minibuffer history
(savehist-mode 1)
(setq history-length 1000)

;; Ensure there's an empty line at the end of the file
(setq-default require-final-newline t)

;; Destroy trailing whitespace on exit
(add-hook 'before-save-hook 'delete-trailing-whitespace)

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
(projectile-global-mode)
(add-to-list 'projectile-globally-ignored-directories "node_modules")

;; Load default smartparens configuration
(require 'smartparens-config)
(smartparens-global-mode 1)

(require 'setup-evil)

;; Enable electric indent in all programming modes
(add-hook 'prog-mode-hook 'electric-indent-mode)

;; Enable rainbow delimiters in all programming modes
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; Setup extensions
(require 'setup-yasnippet)
(require 'setup-ido)

;; Language-specific setup files
(eval-after-load 'c-mode '(require 'setup-c))
(eval-after-load 'LaTeX-mode '(require 'setup-latex))
(eval-after-load 'markdown-mode '(require 'setup-markdown))
(eval-after-load 'js2-mode '(require 'setup-javascript))
(eval-after-load 'rust-mode '(require 'setup-rust))

;; Load stuff on demand
(autoload 'flycheck-mode "setup-flycheck" nil t)

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

(require 'mode-mappings)

;; Emmet
(add-hook 'sgml-mode-hook 'emmet-mode)

;; Utils
(require 'setup-utils)
