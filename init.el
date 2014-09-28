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

;; Evil mode
(global-evil-leader-mode)
(evil-leader/set-key "f" 'projectile-find-file)

(global-evil-surround-mode 1)

;; Enable evil mode
(evil-mode 1)

;;; esc quits

(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
(add-hook 'slime-repl-mode-hook 'rainbow-delimiters-mode)
(add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
;; (let ((depth 0))
;;   (dolist (color
;;            ;; These are nicked from VIMClojure:
;;            ;; vimclojure/vim/syntax/clojure.vim
;;            '("orange1"
;;              "yellow1"
;;              "greenyellow"
;;              "green1"
;;              "springgreen1"
;;              "cyan1"
;;              "slateblue1"
;;              "magenta1"
;;              "purple1"))
;;     (set-face-foreground (intern (format "rainbow-delimiters-depth-%d-face"
;;                                          (incf depth)))
;;                          color)))

;; Enable rainbow delimiters globally
(global-rainbow-delimiters-mode)

(require 'setup-c)
(require 'setup-latex)
(require 'setup-markdown)

;; Highlight ag's search results
(setq ag-highlight-search t)

;; Enable IDO mode
(when (> emacs-major-version 21)
  (ido-mode t)
  (setq ido-enable-prefix nil
        ido-enable-flex-matching t
        ido-create-new-buffer 'always
        ido-use-filename-at-point nil
        ido-max-prospects 10))

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

;; Enable smex
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)


;; Comment or uncomment current line or region
(defun comment-or-uncomment-region-or-line ()
    "Comments or uncomments the region or the current line if there's no active region."
    (interactive)
    (let (beg end)
        (if (region-active-p)
            (setq beg (region-beginning) end (region-end))
            (setq beg (line-beginning-position) end (line-end-position)))
        (comment-or-uncomment-region beg end)))
(global-set-key (kbd "s-/") 'comment-or-uncomment-region-or-line)

;; Use only own snippets, do not use bundled ones
(setq yas-snippet-dirs '("~/.emacs.d/snippets"))

;; Don't mess with the indentation
(setq yas-indent-line 'fixed)

;; Snippets everywhere
(yas-global-mode 1)

;; No need to be so verbose
(setq yas-verbosity 1)

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

(eval-after-load 'js2-mode '(require 'setup-javascript))

(require 'mode-mappings)
