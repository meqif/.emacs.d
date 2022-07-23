;; Mark start point for load time measurement
(defconst emacs-start-time (current-time))
(unless noninteractive
  (message "Loading %s..." load-file-name))

(setq user-init-file (or load-file-name buffer-file-name))
(setq user-emacs-directory (file-name-directory user-init-file))

(setq package-archives nil)

;; straight package manager
(setq straight-use-package-by-default t)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Refuse to work with old Emacsen
(when (version< emacs-version "24.4")
  (error "This Emacs is too old!"))

;; Cargo culting stuff to make new windows open faster
(modify-frame-parameters nil '((wait-for-wm . nil)))

;; Disable mouse interface
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(defun garbage-collect-when-frame-is-unfocused ()
  "Run garbage collection when the frame is unfocused."
  (unless (frame-focus-state)
    (garbage-collect)))

(if (version< emacs-version "27.0")
    (add-hook 'focus-out-hook 'garbage-collect)
  (add-function :after
                after-focus-change-function
                #'garbage-collect-when-frame-is-unfocused))

;; Disable splash screen
(setq inhibit-startup-message t)

;; Disable startup message in minibuffer
(defun display-startup-echo-area-message ()
  (message ""))

;; Set path to dependencies
(defvar lisp-dir (expand-file-name "lisp" user-emacs-directory))

;; Set up load path
(add-to-list 'load-path lisp-dir)

;; Always load most recent version of required files
(setq load-prefer-newer t)

;; Packages
(setq use-package-enable-imenu-support t
      use-package-always-ensure t)
(straight-use-package 'use-package)
(setq use-package-compute-statistics t)

;; Answering just 'y' or 'n' will do
(if (version< emacs-version "28.1")
    (defalias 'yes-or-no-p 'y-or-n-p)
  (setq use-short-answers t))

;; Bring better defaults
(use-package better-defaults :straight nil :ensure nil)

;; Essential utility libraries!
(use-package f)
(use-package s)

(use-package dash
  :defer t
  :config (dash-enable-font-lock))

(use-package no-littering)

;; Fix path
(use-package exec-path-from-shell
  :config
  (setq exec-path-from-shell-arguments '("-l")
        exec-path-from-shell-shell-name (-first
                                         #'file-exists-p
                                         '("/usr/local/bin/fish" "/usr/bin/fish"))
        shell-file-name exec-path-from-shell-shell-name)
  (exec-path-from-shell-initialize))

;; Functions
(require 'defuns)

;; Keybindings
(require 'keybindings)

;; Appearance
(use-package appearance
  :straight nil
  :ensure nil)

;; Load local-only settings, not tracked by VCS
;; Makes it easy to customize settings for each machine that I don't want to persist in VCS
(when (f-exists? (f-join lisp-dir "local.el"))
  (require 'local))

(use-package doom-modeline
  :defer t
  :hook (after-init . doom-modeline-init)
  :config
  (setq doom-modeline-major-mode-icon nil
        doom-modeline-lsp nil))

(use-package rainbow-delimiters
  :defer
  :hook (prog-mode . rainbow-delimiters-mode))

;; Display clock in modeline
(use-package time
  :defer t
  :config
  (setq display-time-24hr-format t
        display-time-default-load-average nil))

(use-package prog-mode
  :straight nil
  :ensure nil
  :config
  ;; Enable prettify symbols mode globally
  (global-prettify-symbols-mode +1)
  ;; Unprettify when the cursor is either on the symbol or on its right edge
  (setq prettify-symbols-unprettify-at-point 'right-edge))

;; Fix emoji display
(defun --set-emoji-font (frame)
  "Adjust the font settings of FRAME so Emacs can display emoji properly."
  (when (member "Apple Color Emoji" (font-family-list))
    (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") frame 'prepend)))

;; For when Emacs is started in GUI mode:
(add-hook 'emacs-startup-hook #'(lambda () (--set-emoji-font nil)))
;; Hook for when a frame is created with emacsclient
;; see https://www.gnu.org/software/emacs/manual/html_node/elisp/Creating-Frames.html
(add-hook 'after-make-frame-functions '--set-emoji-font)

;; Unclutter modeline
(use-package delight)

(use-package autorevert
  :delight auto-revert-mode)

(use-package eldoc
  :defer t
  :delight
  :hook (eldoc-mode . visual-line-mode)
  :config
  (with-eval-after-load 'evil (evil-set-initial-state 'eldoc-mode 'emacs)))

(use-package smartparens
  :delight
  :config
  (smartparens-global-mode t)
  ;; Show matching parentheses
  (show-smartparens-global-mode t)
  ;; Remove matching parentheses delay
  (setq sp-show-pair-delay 0)
  ;; Load default smartparens configuration
  (require 'smartparens-config))

;; Setup extensions
(use-package setup-evil
  :straight nil
  :ensure nil
  :init (setq evil-want-keybinding nil))

(use-package undo-fu
  :after evil
  :config
  (evil-set-undo-system 'undo-fu))

;; Port of vim-surround
(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package imenu
  :config
  ;; Make imenu rescan automatically
  (setq imenu-auto-rescan t)

  ;; Make imenu work on larger files
  (setq imenu-auto-rescan-maxout 120000)

  ;; Extend the maximum length of entries
  (setq imenu-max-item-length 120)

  ;; Recenter buffer after jumping
  (add-hook 'imenu-after-jump-hook 'recenter))

;; compilation-mode
(use-package compile
  :defer
  :config
  (add-hook 'compilation-mode-hook 'visual-line-mode)
  ;; Scroll compile buffer automatically but stop on the first error
  (setq compilation-scroll-output 'first-error))

(use-package general
  :config
  (general-create-definer general-evil-leader-define-key
    :states '(normal visual)
    :prefix "\\")

  (general-define-key "<s-return>" #'promote-demote-window-dwim)

  ;; Global evil leader shortcuts
  (general-evil-leader-define-key
    "a" 'meqif/goto-alternate-file
    "A" 'meqif/goto-alternate-file-other-window
    "\\" 'meqif/pop-mark
    ;; (t)ake (t)odo items
    "tt" #'(lambda () (interactive) (find-file "~/todo.org"))
    ;; (t)ake (m)eeting notes
    "tm" 'meqif/create-meeting-note))

(use-package vertico
  :init
  ;; Use consult-completion-in-region to get completion in minibuffer a la selectrum-completion-in-region
  ;;
  ;; References:
  ;;   - https://github.com/minad/vertico/issues/127
  ;;   - https://github.com/minad/vertico/issues/24
  (setq completion-in-region-function
        (lambda (&rest args)
          (apply (if vertico-mode
                     #'consult-completion-in-region
                   #'completion--in-region)
                 args)))
  :config
  (vertico-mode)
  ;; Allow completion-at-point while in minibuffer
  (setq enable-recursive-minibuffers t))

(use-package savehist
  :init
  (savehist-mode))

(use-package prescient
  :config
  (setq prescient-persist-mode +1))

(use-package consult
  :bind (("s-r" . consult-imenu))
  :init
  (general-evil-leader-define-key
    "r" 'consult-recent-file
    "R" 'consult-recent-file-other-window
    "p" 'consult-yank-pop
    "s" 'consult-line
    "S" 'consult-ripgrep
    "b" 'consult-buffer
    "B" 'consult-buffer-other-window)
  :config
  (consult-customize
   consult-recent-file :preview-key nil
   consult-buffer :preview-key (kbd "s-p"))
  (setq consult-project-root-function #'(lambda () (-some-> (project-current) (project-root)))))

(use-package marginalia
  :bind (:map minibuffer-local-map ("C-M-a" . marginalia-cycle))
  :init
  (marginalia-mode +1)

  ;; Prefer richer, more heavy, annotations over the lighter default variant.
  (setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil)))

(use-package orderless
  :after vertico
  :config
  ;; The basic completion style is specified as fallback in addition to orderless in order to ensure that completion
  ;; commands which rely on dynamic completion tables, e.g., completion-table-dynamic or completion-table-in-turn, work
  ;; correctly.
  (setq completion-styles '(orderless basic)
        completion-category-overrides '((file (styles partial-completion))))

  (defun without-if-bang (pattern _index _total)
    (when (string-prefix-p "!" pattern)
      `(orderless-without-literal . ,(substring pattern 1))))

  (defun exact-match-if-equals-suffix (pattern _index _total)
    (when (string-suffix-p "=" pattern)
      `(orderless-literal . ,(s-chop-suffix "=" pattern))))

  (setq orderless-style-dispatchers '(without-if-bang exact-match-if-equals-suffix)
        orderless-matching-styles '(orderless-initialism orderless-prefixes orderless-regexp)))

(use-package embark
  :config
  (bind-key "C-c C-o" 'embark-collect-snapshot minibuffer-local-map)
  (bind-key "C-c C-c" 'embark-act minibuffer-local-map)

  (general-define-key :keymaps 'embark-collect-mode-map
                      "M-n" #'(lambda () (interactive) (forward-button 1) (push-button))
                      "M-p" #'(lambda () (interactive) (backward-button 1) (push-button)))

  ;; embarQue
  (general-evil-leader-define-key "q" 'embark-act))

(use-package embark-consult
 :after (embark consult)
 :hook (embark-collect-mode . embark-consult-preview-minor-mode))

(use-package avy
  :demand
  :bind ("C-c SPC" . avy-goto-char-timer)
  :config
  (general-evil-leader-define-key "<SPC>" 'avy-goto-char-timer))

(use-package ace-window
  :init
  (general-evil-leader-define-key "w" #'ace-window)
  :config
  (set-face-attribute 'aw-leading-char-face nil :height 10.0))

;; Save a list of recent files visited
(use-package recentf
  :after f
  :defer 1
  :init
  ;; Increase recent entries list from default (20)
  (setq recentf-max-saved-items 100)
  :config
  (recentf-mode +1)
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory))

;; Misery loves this
(use-package company
  :disabled
  :hook ((prog-mode comint-mode docker-compose-mode ess-mode cider-repl-mode) . company-mode)
  :delight
  :config
  (setq
   ;; Offer completions quickly
   company-idle-delay 0.3
   ;; Align tooltips
   company-tooltip-align-annotations t
   ;; Start completing after two chars
   company-minimum-prefix-length 2
   ;; Wrap around candidate list
   company-selection-wrap-around t
   ;; Fix lowercase candidates
   company-dabbrev-downcase nil
   ;; Ignore case in completion popup
   completion-ignore-case t)

  (setq company-format-margin-function #'company-vscode-light-icons-margin)

  (with-eval-after-load 'evil-collection
    (progn
      (evil-collection-company-setup)
      (define-key company-active-map (kbd "RET") 'company-complete))))

(use-package company-posframe
  :hook (company-mode . company-posframe-mode))

(use-package corfu
  :bind
  (:map corfu-map
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous))
  :config
  (global-corfu-mode)
  (setq corfu-auto t
        corfu-preselect-first nil
        corfu-cycle t))

(use-package corfu-doc
  :after corfu
  :hook (corfu-mode . corfu-doc-mode))

(use-package kind-icon
  :ensure t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  (kind-icon-default-style '(:padding 0 :stroke 0 :margin 0 :radius 0 :height 1.0 :scale 0.7))
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package docker-compose-mode
  :defer)

;; Unique buffer names
(use-package uniquify
  :straight nil
  :ensure nil
  ;; Make uniquify rename buffers like in path name notation
  :config (setq uniquify-buffer-name-style 'forward))

(use-package wgrep
  :commands wgrep-change-to-wgrep-mode
  :defer t)

(use-package flymake
  :defer
  :bind (:map flymake-mode-map
              ("C-c ! l" . flymake-show-diagnostics-buffer)
              ("C-c ! n" . flymake-goto-next-error)
              ("C-c ! p" . flymake-goto-prev-error)))

(use-package pos-tip :defer t)

(use-package flymake-diagnostic-at-point
  :after flymake
  :config
  (add-hook 'flymake-mode-hook #'flymake-diagnostic-at-point-mode))

(use-package org
  :defer
  :after hydra
  :init
  (progn
    ;; Don't truncate lines
    (setq org-startup-truncated nil
          ;; Fold all items in new buffers
          org-startup-folded t
          ;; Stop org-mode from replacing my window-moving keys
          ;; Has to be defined before loading org-mode
          org-replace-disputed-keys t
          ;; Don't export a table of content by default
          org-export-with-toc nil
          ;; Place footnotes at the end of the current outline node
          org-footnote-section nil
          ;; Add an intermediate "doing" state in todo keywords
          org-todo-keywords '((sequence "TODO" "DOING" "DONE")))
    ;; Enable wordwrap
    (add-hook 'org-mode-hook 'visual-line-mode)

    ;; Source block tweaks
    (setq
     ;; Enable syntax highlighting in source blocks
     org-src-fontify-natively t
     ;; Don't insert two spaces at the start of each line in source blocks
     org-src-preserve-indentation t
     ;; Make tabs act the same as in the source block's associated major mode
     org-src-tab-acts-natively t
     ;; Re-use the same window for source block edits
     org-src-window-setup 'current-window)

    ;; Organizing stuff
    ;; http://sachachua.com/blog/2015/02/learn-take-notes-efficiently-org-mode/

    ;; Set default notes file
    (setq org-default-notes-file "~/organizer.org")

    ;; Make it easier to change TODO status
    (evil-define-key 'motion org-mode-map "t" 'org-todo)

    ;; Hydra
    (defhydra hydra-org-mode-narrow (:color blue)
      "Narrow buffer to"
      ("s" org-narrow-to-subtree "Subtree")
      ("b" org-narrow-to-block "Block")
      ("w" widen "Widen"))

    ;; Shortcut to capture notes
    (global-set-key (kbd "C-c c") 'org-capture)

    ;; Easier navigation between headings
    (general-define-key :keymaps 'org-mode-map
                        :states 'normal
                        "C-j" #'org-next-visible-heading
                        "C-k" #'org-previous-visible-heading)))

;; Org-latex configuration
(use-package ox-latex
    :after org
    :straight org
    :config
    ;; Use latexmk and xelatex to generate PDFs
    (setq org-latex-pdf-process '("latexmk -pdflatex=xelatex -pdf -f %f"))
    ;; Default packages
    (add-to-list 'org-latex-packages-alist
                 '("" "MinionPro" nil))
    (add-to-list 'org-latex-packages-alist
                 '("" "microtype" nil)))

;; Add Github-Flavored Markdown exporter
(use-package ox-gfm
  :after org)

;; Allow editing html blocks
(use-package ox-html
  :after org
  :straight org)

;; Use org-mode tables in any mode
(use-package org-table
  :straight nil
  :ensure nil
  :commands orgtbl-mode
  :init
  (defalias 'org-table-mode 'orgtbl-mode))

(use-package evil-org
  :after org
  :config
  (add-hook 'org-mode-hook #'evil-org-mode)
  (add-hook 'evil-org-mode-hook #'evil-org-set-key-theme))

(use-package org-superstar
  :after org
  :hook (org-mode . org-superstar-mode)
  :config
  (setq org-superstar-leading-bullet ?\s
        org-superstar-headline-bullets-list '(?① ?② ?③ ?④ ?⑤ ?⑥)))

(use-package org-ql
  :after org)

(use-package yasnippet
  :hook ((org-mode rspec-mode ruby-mode enh-ruby-mode rust-mode) . yas-minor-mode)
  :defer
  :delight yas-minor-mode
  :config
  (general-define-key :keymaps 'yas-minor-mode-map "TAB" yas-maybe-expand)

  ;; Use only own snippets, do not use bundled ones
  (setq yas-snippet-dirs (list (expand-file-name "~/.emacs.d/snippets")))

  ;; Don't mess with the indentation
  (setq yas-indent-line 'fixed)

  ;; No need to be so verbose
  (setq yas-verbosity 1)

  (yas-reload-all)

  (add-hook 'snippet-mode-hook
            (lambda ()
              ;; Temporarily disable required newline at the end of file
              ;; This fixes the problem with an extra newline when expanding snippets
              (setq-local require-final-newline nil))))

;; Language-specific setup files
(use-package markdown-mode
  :mode ("\\.md\\'" . gfm-mode)
  :config
  (setq markdown-command "marked --gfm")
  (add-hook 'gfm-mode-hook #'visual-line-mode))

(use-package markdown-toc
  :after markdown-mode
  :config
  (setq markdown-toc-header-toc-title "## Table of contents"))

(use-package js2-mode
  :mode "\\.js\\'"
  :config
  (setq-default js2-global-externs
                '("module" "export" "require" "describe" "it" "before" "after"))

  ;; Let flycheck handle parse errors
  (setq-default js2-mode-show-parse-errors nil)
  (setq-default js2-strict-missing-semi-warning nil)

  (setq js-indent-level 2)

  (add-hook 'js2-mode-hook
            (lambda ()
              ;; Use symbol for anonymous functions
              (push '("function" . ?ƒ) prettify-symbols-alist))))

(use-package json-mode
  :mode "\\.json\\'"
  :config
  (setq-default js-indent-level 2))

(use-package rjsx-mode
  :defer)

(use-package dockerfile-mode
  :mode "Dockerfile\\'")

(use-package cc-mode
  :defer t
  :config
  (setq c-basic-offset 4
        c-default-style "linux"))

(use-package sgml-mode
  :defer t
  :config
  ;; Enable tagedit
  ;; Auto-close tags and other goodies
  (tagedit-add-experimental-features)
  (add-hook 'html-mode-hook #'tagedit-mode)
  (add-hook 'mustache-mode #'tagedit-mode))

(use-package tagedit
  :after sgml-mode)

(use-package emmet-mode
  :defer
  :init
  (add-hook 'sgml-mode-hook 'emmet-mode)
  (add-hook 'mustache-mode-hook 'emmet-mode))

(use-package scss-mode
  :defer t
  ;; Don't autocompile SCSS, I usually have task runners doing that
  :config (setq scss-compile-at-save nil))

(use-package rust-mode
  :defer t)

(use-package cargo
  :after rust-mode)

(use-package cargo-transient
  :bind (:map rust-mode-map
              ("C-c C-c" . cargo-transient)))

(use-package eglot
  :hook ((rust-mode kotlin-mode ruby-mode enh-ruby-mode) . eglot-ensure)
  :bind (:map eglot-mode-map
              ("M-RET" . eglot-code-actions))
  :config
  (add-to-list 'eglot-server-programs '(rust-mode "rust-analyzer"))
  (add-to-list 'eglot-server-programs '(enh-ruby-mode "solargraph" "socket" "--port" :autoport))
  (add-to-list 'eglot-server-programs '(terraform-mode "terraform-ls" "serve" "-port" :autoport))
  (add-to-list 'eglot-server-programs '(python-mode "pyright-langserver" "--stdio"))
  (setq eglot-autoshutdown t
        eglot-sync-connect nil
        eglot-autoreconnect nil)

  ;; Fix "proc macro Deserialize not expanded"
  (setq-default eglot-workspace-configuration
                `((:rust-analyzer . (:procMacro
                                     (:enable t)
                                     :cargo
                                     (:loadOutDirsFromCheck t)))))

  (general-define-key :keymap 'eglot-mode-map "C-h ." 'eldoc-doc-buffer))

(use-package flyspell
  :defer
  :init
  ;; Use Aspell for spellcheck
  (setq ispell-program-name "aspell")
  (setq ispell-list-command "--list")
  (setq ispell-dictionary "english")

  ;; Flyspell messages slow down the spellchecking process
  (setq flyspell-issue-message-flag nil)

  ;; Don't spell check embedded snippets in org-mode
  ;; Source: http://emacs.stackexchange.com/a/9347
  (defun org-mode-flyspell-verify-ignore-blocks (return-value)
    (let ((rlt return-value)
          (begin-regexp "^[ \t]*#\\+BEGIN_\\(SRC\\|HTML\\|LATEX\\)")
          (end-regexp "^[ \t]*#\\+END_\\(SRC\\|HTML\\|LATEX\\)")
          old-flag
          b e)
      (when return-value
        (save-excursion
          (setq old-flag case-fold-search)
          (setq case-fold-search t)
          (setq b (re-search-backward begin-regexp nil t))
          (if b (setq e (re-search-forward end-regexp nil t)))
          (setq case-fold-search old-flag))
        (if (and b e (< (point) e)) (setq rlt nil)))
      return-value))
  (advice-add 'org-mode-flyspell-verify :filter-return 'org-mode-flyspell-verify-ignore-blocks))

(use-package flyspell-correct
     :after flyspell)

;; Misc
(use-package my-misc :straight nil :ensure nil)

(use-package make-mode
  :defer
  ;; Use normal tabs in makefiles
  :hook (makefile-mode . (lambda () (setq indent-tabs-mode t))))

;; Highlight excessively long lines
(use-package whitespace
  :defer
  :delight
  :config
  (setq whitespace-style '(face lines-tail))
  (setq-default whitespace-line-column nil))

;; Enable diff indication on the fringe
(use-package diff-hl
  :commands diff-hl-magit-post-refresh
  :init
  (add-hook 'prog-mode-hook 'turn-on-diff-hl-mode)
  (add-hook 'vc-dir-mode-hook 'turn-on-diff-hl-mode)
  :config
  (set-face-attribute 'diff-hl-insert nil :foreground "#98971a" :background "#98971a")
  (set-face-attribute 'diff-hl-change nil :foreground "#458588" :background "#458588")
  (set-face-attribute 'diff-hl-delete nil :foreground "#cc241d" :background "#cc241d"))

(use-package magit
  :bind ("C-x g" . magit-status)
  :init
  (general-evil-leader-define-key "g" #'magit-status)
  ;; Mark setup instructions as read
  (setq magit-last-seen-setup-instructions "1.4.0"
        ;; Silence nag on push
        magit-push-always-verify nil)

  ;; Make fine-grained changes more obvious
  (setq magit-diff-refine-hunk 'all)
  :config

  ;; Use (more recent) git installed at /usr/local/bin/git if available
  ;; This should speed things up in macOS when Homebrew git is installed
  (when (file-exists-p "/usr/local/bin/git")
    (setq magit-git-executable "/usr/local/bin/git"))

  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

  (add-hook 'magit-status-mode-hook 'visual-line-mode)

  (add-to-list 'magit-repository-directories '("~/.emacs.d/" . 0))

  ;; Disable the new section indicators
  (setq magit-section-visibility-indicator nil)

  ;; Check excessively long summary lines in commit messages
  (add-to-list 'git-commit-style-convention-checks 'overlong-summary-line)

  (setq magit-set-upstream-on-push t)
  ;; Fix gravatars in commit log in OSX
  (when (eq system-type 'darwin)
    (setq magit-revision-use-gravatar-kludge t))

  ;; Verbose commits (show changes to be commited) by default
  (setq magit-commit-arguments '("--verbose")
        ;; No fast-forward by default for merge mode
        magit-merge-arguments '("--no-ff")
        ;; Prune removed branches by default when fetching from remote
        magit-fetch-arguments '("--prune"))

  ;; Make <SPC> insert dashes instead. Useful when creating new branches
  (define-key magit-minibuffer-local-ns-map "\s" "-")

  (defun meqif/open-pr ()
    (interactive)
    (-when-let* ((matches (s-match "git@github.com:\\(.+\\)\\(?:\\.git\\)"
                                   (substring-no-properties (magit-format-remote*url))))
                 (repository (cadr matches))
                 (pr-url (concat "https://github.com/" repository "/compare/" (magit-get-current-branch))))
      (browse-url pr-url)))

  (defun guess-jira-ticket-identifier ()
    "Attempt to extract JIRA identifier from git branch name.

It only really works if the branch follows the PROJECT-TICKETNUMBER-description
naming scheme."
    (-when-let* ((current-branch (magit-get-current-branch)))
      (-first-item (s-match "^[A-Z]+-[0-9]+" current-branch))))

  (defun append-jira-ticket-identifier ()
    (-when-let* ((jira-ticket-identifier (guess-jira-ticket-identifier)))
      (save-excursion
        (goto-char (point-min))
        (insert "\n\n" jira-ticket-identifier))))

  (defun maybe-append-jira-ticket-identifier ()
    "Add JIRA ticket identifier only when the commit message doesn't contain it.

When ammending commits, the existing commit message may already
contain the JIRA ticket identifier, in which case re-adding it is
unnecessary."
    (unless (save-excursion
              (s-matches-p "^[A-Z]+-[0-9]+$"
                           (buffer-substring-no-properties 1 (re-search-forward "\n\n#"))))
      (append-jira-ticket-identifier)))

  (add-hook 'git-commit-setup-hook 'maybe-append-jira-ticket-identifier)

  ;; The fill-column for git commits is 72 characters as per https://commit.style/
  (add-hook 'git-commit-setup-hook #'(lambda () (setq fill-column 72))))

(use-package evil-collection
  :after evil
  :config
  (with-eval-after-load 'evil-collection-magit
    (evil-define-key evil-collection-magit-state magit-mode-map "\\" nil))
  (setq evil-collection-mode-list
        '((occur replace)
          magit
          vterm
          deadgrep
          xref
          helpful))
  (evil-collection-init))

(use-package browse-at-remote
  :defer
  :init
  (general-evil-leader-define-key "o" #'browse-at-remote)
  :config
  (setq browse-at-remote-prefer-symbolic nil))

(use-package diff-mode
  :defer t
  :straight nil
  :ensure nil
  :config
  ;; Make fine grained changes more obvious
  (set-face-attribute 'diff-refine-added nil :bold t :background 'unspecified)
  (set-face-attribute 'diff-refine-changed nil :bold t :background 'unspecified)
  (set-face-attribute 'diff-refine-removed nil :bold t :background 'unspecified))

(use-package subword
  :hook ((rust-mode ruby-mode enh-ruby-mode kotlin-mode python-mode j2-mode) . subword-mode)
  :delight "_")

;; Ruby mode
(use-package ruby-mode
  :mode ("\\.rb\\'"
         "\\.rbi\\'"
         "\\.rake\\'"
         "Rakefile\\'"
         "\\.gemspec\\'"
         "\\.ru\\'"
         "Gemfile\\'"
         "Guardfile\\'"
         "Capfile\\'"
         "Vagrantfile\\'")
  :config
  (advice-add 'join-line :after
              #'meqif/ruby-delete-trailing-comma-before-closing-bracket)
  (setq ruby-deep-ident-paren nil
        ruby-insert-encoding-magic-comment nil)
  (add-hook 'ruby-mode-hook
            #'(lambda ()
                (setq mode-name "💎")
                (setq-local tab-width 2)
                (setq-local evil-shift-width 2)))

  (add-hook 'ruby-mode-hook #'meqif/set-fill-column-to-rubocop-max-line-length)

  (defun meqif/rename-rails-migration ()
    "Rename a Rails migration to the current moment."
    (interactive)
    (require 'magit-files)
    (-when-let* ((root (project-root (project-current)))
                 (migrations-directory (f-join root "db" "migrate"))
                 (filename (completing-read
                            "Migration to rename: "
                            (-map 'f-filename (f-files migrations-directory))))
                 (new-name (s-concat (format-time-string "%Y%m%d%H%M%S")
                                     (s-replace-regexp "^[0-9]+_" "_" filename))))
      (magit-file-rename (f-join migrations-directory filename)
                         (f-join migrations-directory new-name))))

  (defun guess-docker-cwd ()
    "Attempt to guess the value of APP_HOME in the project's Dockerfile."
    (-when-let* ((project-root (-some-> (project-current) (project-root)))
                 (dockerfile (concat project-root "Dockerfile"))
                 (_ (f-exists? dockerfile))
                 (command (concat "sed -En 's/ENV APP_HOME (.*)/\\1/p' " dockerfile))
                 (cwd (s-trim-right (shell-command-to-string command))))
      (file-name-as-directory cwd)))

  (defun maybe-set-docker-cwd ()
    (-when-let (cwd (guess-docker-cwd))
      (setq rspec-docker-cwd cwd)))

  (add-hook 'ruby-mode-hook #'maybe-set-docker-cwd))

(use-package rspec-mode
  :functions (rspec-verify
              rspec-verify-all
              rspec-verify-single
              rspec-toggle-spec-and-target-find-example
              rspec-run-test-subset
              hydra-rspec/body)
  :after (:any ruby-mode enh-ruby-mode)
  :config
  (setq rspec-command-options "--format progress"
        rspec-use-docker-when-possible t
        rspec-docker-container "tests")

  (defun rspec-run-test-subset (type)
    (-let* ((relative-path (pcase type
                             ('unit "spec/unit")
                             ('acceptance "spec/acceptance")
                             (_ (error "Unknown test subset type"))))
            (path (concat (-some-> (project-current) (project-root)) relative-path)))
      (rspec-run-single-file path (rspec-core-options)))))
;; Handy functions to run rubocop from Emacs
(use-package rubocop
  :after (:any ruby-mode enh-ruby-mode)
  :config
  (setq rubocop-check-command "rubocop --format emacs --parallel"))

;; Automatically expand # to #{} inside double-quoted strings
(use-package ruby-tools
  :after (:any ruby-mode enh-ruby-mode)
  :delight "🛠")

(use-package inf-ruby
  :defer
  :config
  (setq inf-ruby-default-implementation "ruby")
  (add-hook 'ruby-mode-hook #'inf-ruby-minor-mode)
  (general-define-key :keymap 'ruby-mode-map "C-c C-c" 'ruby-send-buffer))

(use-package blacken
  :after python
  :hook (python-mode . blacken-mode))

(use-package lispy
  :delight
  :defer t
  :init
  (add-hook 'emacs-lisp-mode-hook #'lispy-mode)
  (add-hook 'clojure-mode-hook #'lispy-mode))

(use-package lispyville
  :delight
  :defer t
  :init
  (add-hook 'lispy-mode-hook #'lispyville-mode))

(use-package default-text-scale
  :straight (default-text-scale :type git :host github :repo "purcell/default-text-scale"))

;; Group reusable keyboard bindings behind a common prefix
(use-package hydra
  :config
  ;; Easy error navigation
  (global-set-key
   (kbd "M-g")
   (defhydra hydra-error (:color red)
     "goto-error"
     ("h" first-error "first")
     ("n" next-error "next")
     ("p" previous-error "prev")
     ("v" recenter-top-bottom "recenter")
     ("q" nil "quit")))

  (defhydra hydra-zoom ()
    "zoom"
    ("i" default-text-scale-increase "in")
    ("o" default-text-scale-decrease "out")
    ("0" default-text-scale-reset "reset")
    ("q" nil "quit" :color blue))
  (general-evil-leader-define-key "z" #'hydra-zoom/body))

(use-package major-mode-hydra
  :after hydra
  :bind ("M-SPC" . major-mode-hydra)
  :config
  (major-mode-hydra-define rust-mode
    (:quit-key "q")
    ("Build"
     (("c" cargo-process-check "check")
      ("b" cargo-process-bench "benchmark all")
      ("C" cargo-process-build "build")
      ("r" cargo-process-run "run"))
     "Test"
     (("tt" cargo-process-test "test all")
      ("tf" cargo-process-current-test "test current function"))
     "Documentation"
     (("dd" cargo-process-doc "build documentation")
      ("do" cargo-process-doc-open "build and open documentation"))
     "Extra"
     (("y" cargo-process-clippy "clippy")
      ("f" cargo-process-fmt "format"))))

  (major-mode-hydra-define enh-ruby-mode
    (:quit-key "q")
    ("RSpec"
     (("a" rspec-verify-all "run all specs")
      ("s" rspec-verify-single "run specs for this context")
      ("v" rspec-verify "run specs for this buffer")
      ("t" rspec-toggle-spec-and-target-find-example "toggle between spec and class"))
     ""
     (("f" rspec-run-last-failed "rerun last failed specs")
      ("A" (lambda () (interactive) (rspec-run-test-subset 'acceptance)) "run acceptance tests")
      ("u" (lambda () (interactive) (rspec-run-test-subset 'unit)) "run unit tests")))))

(use-package which-key
  :config
  (which-key-mode +1))

;; Macro expansion for ease of debugging
(use-package macrostep
  :defer t
  :bind (:map emacs-lisp-mode-map ("C-c e" . macrostep-expand))
  :config
  ;; Make macrostep play well with evil
  (evil-define-key 'normal macrostep-keymap "q" 'macrostep-collapse-all))

(use-package dired
  :straight nil
  :ensure nil
  :defer t
  :config
  ;; Show human-friendly file sizes and sort numbers properly
  (setq-default dired-listing-switches "-alhv"))

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer))

(use-package ibuffer-vc
  :after ibuffer
  :config
  (add-hook 'ibuffer-hook
            (lambda ()
              (ibuffer-vc-set-filter-groups-by-vc-root)
              (unless (eq ibuffer-sorting-mode 'alphabetic)
                (ibuffer-do-sort-by-alphabetic)))))

(use-package bufler
  :bind ("C-x C-b" . bufler)
  :init
  ;; `bufler-list-mode' derives from `magit-section-mode', which has some magic
  ;; applied through `evil-collection-magit', so we have to put things back to
  ;; normal here
  (evil-set-initial-state 'bufler-list-mode 'normal)
  :config
  (setq bufler-filter-buffer-modes '(bufler-list-mode special-mode timer-list-mode))
  ;; Restore some keybindings in evil's normal state
  (general-define-key :states 'normal :keymaps 'bufler-list-mode-map
                      "C-k" #'bufler-list-buffer-kill
                      "RET" #'bufler-list-buffer-switch
                      "TAB" #'magit-section-toggle
                      "gr" #'bufler-list
                      "q" #'quit-window))

(use-package xref
  :init
  (evil-set-initial-state 'xref-mode 'normal)
  :config
  (setq xref-search-program 'ripgrep)
  (add-hook 'xref-after-return-hook #'recenter))

(use-package dumb-jump
  :defer t
  :config
  (add-hook 'dumb-jump-after-jump-hook #'recenter)
  (add-hook 'dumb-jump-after-jump-hook #'xref-pulse-momentarily)
  (setq dumb-jump-selector 'completing-read
        dumb-jump-force-searcher 'rg))

(use-package smart-jump
  :after eglot
  :config
  (smart-jump-setup-default-registers))

(use-package iedit
  :defer
  :init (setq iedit-toggle-key-default nil))

(use-package evil-multiedit
  :config
  ;; Use default bindings
  (evil-multiedit-default-keybinds)

  ;; Jump to new matches
  (setq evil-multiedit-follow-matches t))

;; Better % jumping
(use-package evil-matchit
  :init (global-evil-matchit-mode))

;; Convenient bindings for align functions
(use-package evil-lion
  :config
  (evil-lion-mode))

;; Unbind keys I accidentally hit too often
(unbind-key (kbd "s-&"))
(unbind-key (kbd "s-k"))
(unbind-key (kbd "s-p"))

(use-package faun-mode
  :straight nil
  :ensure nil
  :after 'org
  :load-path "lisp/"
  :delight "👹")

;; Wrap lines in visual-line-mode at the fill column
(use-package visual-fill-column
  :disabled
  :init
  (add-hook 'visual-line-mode-hook 'visual-fill-column-mode))

(use-package clojure-mode
  :defer t
  :config
  (add-hook 'clojure-mode-hook 'cider-mode))

(use-package cider
  :defer t
  :config
  (setq cider-prompt-for-symbol nil
        cider-repl-display-help-banner nil)
  (advice-add 'cider-find-var :after #'recenter))

;; Diffs like vimdiff
(use-package vdiff
  :defer
  :config
  (setq ediff-split-window-function 'split-window-horizontally))

(use-package smerge-mode
  :defer)

(use-package ess
  :defer t)

(use-package kotlin-mode
  :mode "\\.kt\\'")

(use-package groovy-mode
  :mode "\\.gradle\\'")

;; Improve readability of ELisp regular expressions
(use-package easy-escape
  :delight easy-escape-minor-mode
  :hook ((lisp-mode emacs-lisp-mode) . easy-escape-minor-mode))

(use-package autoinsert
  :hook ((prog-mode yaml-mode org-mode) . auto-insert-mode)
  :config
  (setq auto-insert-query nil)
  (assoc-delete-all '("\\.el\\'" . "Emacs Lisp header") auto-insert-alist)
  (setq auto-insert-alist
        (-cons*
         '("\\.rb\\'" nil "# frozen_string_literal: true\n")
         '("\\.ya?ml\\'" nil "---\n")
         '("/meeting-notes/.+\\.org\\'" . [(lambda () (yas-expand-snippet (yas-lookup-snippet "<meeting-note" 'org-mode)))])
         auto-insert-alist)))

(use-package fish-mode
  :defer t)

(use-package flymake-shellcheck
  :commands flymake-shellcheck-load
  :init
  (add-hook 'sh-mode-hook 'flymake-shellcheck-load)
  (add-hook 'sh-mode-hook 'flymake-mode))

;; Improve performance of compilation buffers with colors
(use-package xterm-color
  :after compile
  :config
  (setq compilation-environment '("TERM=xterm-256color"))

  (add-hook 'compilation-start-hook
            (lambda (proc)
              (when (eq (process-filter proc) 'compilation-filter)
                (set-process-filter
                 proc
                 (lambda (proc string)
                   (funcall 'compilation-filter proc
                            (xterm-color-filter string))))))))

(use-package vterm
  :commands vterm
  :config
  (add-hook 'vterm-mode-hook
            #'(lambda ()
                (setq-local
                 ;; Disable font height override in vterm because it messes up the displayed text area
                 default-text-properties nil
                 ;; Disable line highlight
                 global-hl-line-mode nil))))

(use-package olivetti
  :defer
  :hook ((org-mode zettelkasten-mode) . olivetti-mode)
  :config
  (setq-default olivetti-body-width 100))

(use-package go-mode
  :defer
  :config
  (add-hook 'before-save-hook 'gofmt-before-save t t))

(use-package terraform-mode)

(use-package deadgrep
  :defer
  :init
  (evil-set-initial-state 'deadgrep-mode 'normal))

;; Post initialization -- calculate loading time
;; Copied from jwiegley's configuration
(when (display-graphic-p)
  (let ((elapsed (float-time (time-subtract (current-time) emacs-start-time))))
    (message "Loading %s...done (%.3fs)" load-file-name elapsed))

  (add-hook 'after-init-hook
            `(lambda ()
               (let ((elapsed (float-time
                               (time-subtract (current-time) emacs-start-time))))
                 (message "Loading %s...done (%.3fs) [after-init]"
                          ,load-file-name elapsed)))
            t))

(use-package tramp
  :defer
  :config
  (setq tramp-default-method "ssh")
  (setq vc-ignore-dir-regexp
        (format "%s\\|%s" vc-ignore-dir-regexp tramp-file-name-regexp)))

(use-package project
  :commands project-root
  :init
  (general-evil-leader-define-key
    "f" #'project-find-file
    "x" #'project-switch-project)
  :config
  (defun meqif/clean-removed-projects ()
    "Clean removed directories from `project-switch-project'."
    (when (listp project--list)
      (setq project--list (--filter (f-directory-p (car it)) project--list))))
  (run-with-timer 1 300 'meqif/clean-removed-projects)

  (setq project-switch-commands
        '((?f "Find file" project-find-file)
          (?g "Find regexp" project-find-regexp)
          (?G "Find expression (deadgrep)" deadgrep)
          (?d "Dired" project-dired)
          (?m "Magit" (lambda () (interactive) (magit-status (project-root (project-current t)))))
          (?s "Vterm" vterm))))

(use-package replace
  :straight nil
  :ensure nil
  :config
  ;; Make jumping from occur results easier to follow visually
  (add-hook 'occur-mode-find-occurrence-hook 'recenter)
  (add-hook 'occur-mode-find-occurrence-hook 'xref-pulse-momentarily))

(use-package helpful
  :bind (("C-h f" . #'helpful-callable)
         ("C-h v" . #'helpful-variable)
         ("C-h k" . #'helpful-key))
  :hook (helpful-mode . visual-line-mode)
  :init
  (evil-set-initial-state 'helpful-mode 'normal))

(use-package eldoc-eval
  :config
  (eldoc-in-minibuffer-mode +1))

(use-package ctrlf
  :config
  (ctrlf-mode +1)
  (setq ctrlf-auto-recenter t))

(use-package origami
  :config
  (global-origami-mode +1))

(use-package bookmark
  :ensure nil
  :straight nil
  :config
  ;; Persist bookmarks to file after every change
  (setq bookmark-save-flag 1))

(use-package burly)

(use-package ts :defer)
(use-package ghub :defer)
(use-package dipper
  :defer
  :commands display-pending-pull-requests filtered-pending-pull-requests
  ;; :after (:all ghub dash s ts)
  :ensure nil
  ;; :straight (dipper :host github :repo "meqif/.emacs.d" :files ("lisp/dipper.el"))
  :straight nil
  :load-path "lisp/"
  :config
  (defun dipper-filtered ()
    (interactive)
    (org-ql-search
      (list (get-buffer "*Pending pull requests*"))
      '(and (todo) (not (tags "stale")))
      :sort '(date)
      :narrow nil
      :super-groups '((:auto-parent)))))

;; First try to indent the current line, and if the line
;; was already indented, then try `completion-at-point'
(setq tab-always-indent 'complete)

(use-package vundo
  :after general
  :straight (vundo :type git :host github :repo "casouri/vundo")
  :config
  (general-evil-leader-define-key "u" #'vundo))

(use-package tree-sitter
  :config
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))
(use-package tree-sitter-langs :after tree-sitter)

;; Annotate files without polluting them!
(use-package annotate)

(use-package emojify
  :defer t)

(use-package server
  :defer 2
  :delight server-buffer-clients
  ;; Start server if it isn't already running
  :config
  (unless (server-running-p) (server-start)))

;; After startup, set reasonable values for garbage collection
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold 16777216
                      gc-cons-percentage 0.1)))
