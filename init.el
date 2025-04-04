(setq user-init-file (or load-file-name buffer-file-name))
(setq user-emacs-directory (file-name-directory user-init-file))

(setq package-archives nil)
(setq use-package-enable-imenu-support t
      use-package-always-ensure t
      use-package-compute-statistics t)

;; Elpaca package manager!
(defvar elpaca-installer-version 0.10)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                 ,@(when-let ((depth (plist-get order :depth)))
                                                     (list (format "--depth=%d" depth) "--no-single-branch"))
                                                 ,(plist-get order :repo) ,repo))))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable :elpaca use-package keyword.
  (elpaca-use-package-mode)
  ;; Assume :elpaca t unless otherwise specified.
  (setq elpaca-use-package-by-default t))

;; Block until current queue processed.
(elpaca-wait)

;; Refuse to work with old Emacsen
(when (version< emacs-version "29.0")
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

(add-function :after
              after-focus-change-function
              #'garbage-collect-when-frame-is-unfocused)

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

;; Answering just 'y' or 'n' will do
(setq use-short-answers t)

;; Bring better defaults
(use-package better-defaults :ensure nil)

;; Essential utility libraries!
(use-package f)
(use-package s)

(elpaca project)

(use-package dash
  :defer t
  :config (dash-enable-font-lock))
(elpaca-wait)

(use-package no-littering
  :config
  (setq lock-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))
(elpaca-wait)

(use-package treesit
  :ensure nil
  :config
  ;; Not covered by no-littering yet
  (setq treesit-extra-load-path (list (f-join no-littering-var-directory "tree-sitter"))))

;; Fix path
(use-package exec-path-from-shell
  :config
  (setq exec-path-from-shell-arguments '("-l")
        exec-path-from-shell-shell-name (-first
                                         #'file-exists-p
                                         '("/usr/local/bin/fish" "/usr/bin/fish"))
        exec-path-from-shell-check-startup-files nil
        shell-file-name exec-path-from-shell-shell-name)
  (exec-path-from-shell-initialize))

;; Functions
(require 'defuns)

;; Keybindings
(require 'keybindings)

;; Appearance
(use-package appearance
  :ensure nil)

;; Load local-only settings, not tracked by VCS
;; Makes it easy to customize settings for each machine that I don't want to persist in VCS
(when (f-exists? (f-join lisp-dir "local.el"))
  (require 'local))

(use-package doom-modeline
  :defer t
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-major-mode-icon nil
        doom-modeline-lsp t
        doom-modeline-vcs-max-length 30))

(use-package rainbow-delimiters
  :defer
  :hook (prog-mode . rainbow-delimiters-mode))

;; Display clock in modeline
(use-package time
  :defer t
  :ensure nil
  :config
  (setq display-time-24hr-format t
        display-time-default-load-average nil))

(use-package prog-mode
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
  :ensure nil
  :delight auto-revert-mode)

(use-package eldoc
  :ensure (:host github :repo "emacs-mirror/emacs")
  :defer t
  :delight
  :hook (special-mode . visual-line-mode)
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
  (require 'smartparens-config)
  ;; Fix single quote behavior in minibuffer
  (sp-with-modes '(minibuffer-inactive-mode minibuffer-mode) (sp-local-pair "'" nil :actions nil)))

(use-package evil
  :init (setq evil-want-keybinding nil))
(elpaca-wait)

(require 'setup-evil)

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
  :ensure nil
  :config
  ;; Make imenu rescan automatically
  (setq imenu-auto-rescan t)

  ;; Make imenu work on larger files
  (setq imenu-auto-rescan-maxout 120000)

  ;; Extend the maximum length of entries
  (setq imenu-max-item-length 200)

  ;; Recenter buffer after jumping
  (add-hook 'imenu-after-jump-hook 'recenter))

;; compilation-mode
(use-package compile
  :ensure nil
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
  :ensure (:files (:defaults "extensions/*"))
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
  ;; Allow vertico to be customized depending on context
  (vertico-multiform-mode)

  ;; Allow completion-at-point while in minibuffer
  (setq enable-recursive-minibuffers t)

  (setq vertico-multiform-commands
        '(("magit-branch*" (vertico-sort-override-function . meqif/sort-git-branches))
          ("magit-rebase-\\(subset\\|branch\\)" (vertico-sort-override-function . meqif/sort-git-branches))
          (project-find-file (vertico-sort-override-function . meqif/sort-project-files))))

  ;; Sorting order for magit branch commands:
  ;; - First origin/main or origin/master, if available
  ;; - Then local branches, sorted by name
  ;; - Then remote branches, sorted by name
  (defun meqif/sort-git-branches (branches)
    (let ((grouped-branches (-group-by (lambda (branch)
                                         (cond
                                          ((member branch '("origin/main" "origin/master")) 'main)
                                          ((string-prefix-p "origin/" branch) 'remote)
                                          ((not (member branch '("HEAD" "ORIG_HEAD" "FETCH_HEAD"))) 'local)))
                                       branches)))
      (append (alist-get 'main grouped-branches)
              (vertico-sort-alpha (alist-get 'local grouped-branches))
              (vertico-sort-alpha (alist-get 'remote grouped-branches)))))

  ;; app -> spec -> sig
  (defun meqif/sort-project-files (files)
    (let ((grouped-files (-group-by (lambda (file)
                                      (cond ((string-prefix-p "spec/" file) 'spec)
                                            ((string-prefix-p "sig/" file) 'sig)
                                            ((string-prefix-p "sorbet/" file) 'sorbet)
                                            ((not (string-prefix-p "." file)) 'project)
                                            (t 'hidden)))
                                    files)))
      (append (vertico-sort-alpha (alist-get 'project grouped-files))
              (vertico-sort-alpha (alist-get 'spec grouped-files))
              (vertico-sort-alpha (alist-get 'sig grouped-files))
              (vertico-sort-alpha (alist-get 'sorbet grouped-files))
              (vertico-sort-alpha (alist-get 'hidden grouped-files))))))

(use-package savehist
  :ensure nil
  :init
  (savehist-mode))

(use-package prescient
  :config
  (setq prescient-persist-mode +1))

(use-package consult
  :demand
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

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  (consult-customize
   consult-recent-file :preview-key nil
   consult-buffer :preview-key "s-p")
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
        completion-category-defaults nil
        completion-category-overrides nil)

  (defun without-if-bang (pattern _index _total)
    (when (string-prefix-p "!" pattern)
      `(orderless-without-literal . ,(substring pattern 1))))

  (defun exact-match-if-equals-suffix (pattern _index _total)
    (when (string-suffix-p "=" pattern)
      `(orderless-literal . ,(s-chop-suffix "=" pattern))))

  (setq orderless-style-dispatchers '(without-if-bang exact-match-if-equals-suffix)
        orderless-matching-styles '(orderless-initialism orderless-prefixes orderless-literal)))

(use-package embark
  :demand
  :bind (:map minibuffer-local-map
              ("C-u C-c C-o" . embark-collect)
              ("C-c C-o" . embark-export)
              ("C-c C-c" . embark-act))
  :init
  (defvar ffap-c-path '("/usr/include" "/usr/local/include")
    "List of directories to search for include files.")
  :config
  (general-define-key :keymaps 'embark-collect-mode-map
                      "M-n" #'(lambda () (interactive) (forward-button 1) (push-button))
                      "M-p" #'(lambda () (interactive) (backward-button 1) (push-button)))

  ;; embarQue
  (general-evil-leader-define-key "q" 'embark-act))

(use-package embark-consult
 :after (embark consult)
 :hook (embark-collect-mode . consult-preview-at-point-mode))

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
  :ensure nil
  :after f
  :init
  ;; Increase recent entries list from default (20)
  (setq recentf-max-saved-items 100)
  :config
  (recentf-mode +1)
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory)

  (defun recentf-cleanup-quiet ()
    (interactive)
    (let ((message-log-max nil))
      (with-temp-message ""
        (recentf-cleanup))))

  ;; Clean up recent files list when focus changes
  (add-function :after
                after-focus-change-function
                #'recentf-cleanup-quiet))

(use-package corfu
  :if (version< emacs-version "30.1")
  :ensure (:files (:defaults "extensions/*"))
  :demand
  :bind
  (:map corfu-map
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous))
  :hook (corfu-mode . corfu-popupinfo-mode)
  :config
  (global-corfu-mode)
  (setq corfu-auto t
        corfu-preselect-first nil
        corfu-cycle t
        corfu-popupinfo-delay 0.2))

(use-package completion-preview
  :unless (version< emacs-version "30.1")
  :ensure nil
  :config
  (global-completion-preview-mode))

(use-package svg-lib
  :defer
  :init
  (setq svg-lib-icons-dir (f-join no-littering-var-directory "svg-lib" "cache")))

(use-package kind-icon
  :ensure t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  (kind-icon-default-style '(:padding 0 :stroke 0 :margin 0 :radius 0 :height 1.0 :scale 0.7))
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; Unique buffer names
(use-package uniquify
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
                        "C-k" #'org-previous-visible-heading))

  ;; Allow editing html blocks
  (require 'ox-html))

;; Add Github-Flavored Markdown exporter
(use-package ox-gfm :after org)

;; Use org-mode tables in any mode
(use-package org-table
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

(use-package yasnippet
  :defer 1
  :config
  ;; Prevent yasnippet from logging that it loaded the snippets
  (setq yas-verbosity 2)
  (yas-global-mode))

(use-package yasnippet-capf
  :after yasnippet
  :config
  (add-to-list 'completion-at-point-functions #'yasnippet-capf))

;; Language-specific setup files
(use-package markdown-mode
  :mode ("\\.md\\'" . gfm-mode)
  :config
  (setq markdown-command "marked --gfm")
  (setq markdown-fontify-code-blocks-natively t)
  (add-to-list 'markdown-code-lang-modes '("elixir" . elixir-ts-mode))
  (add-to-list 'markdown-code-lang-modes '("ruby" . ruby-ts-mode))
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

(use-package typescript-ts-mode :ensure nil)

(use-package dockerfile-mode
  :mode "Dockerfile\\'")

(use-package cc-mode
  :ensure nil
  :defer t
  :config
  (setq c-basic-offset 4
        c-default-style "linux"))

(use-package sgml-mode
  :ensure nil
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
  :defer t
  :config
  (setq rust-prettify-symbols-alist nil)
  (add-hook 'rust-mode-hook #'(lambda () (setq-local default-directory (cargo-process--workspace-root)))))

(use-package cargo
  :after rust-mode)

(use-package cargo-transient
  :after rust-mode
  :bind (:map rust-mode-map
              ("C-c C-c" . cargo-transient)))

(use-package eglot
  :ensure nil
  :hook ((rust-mode kotlin-mode ruby-base-mode elixir-ts-mode typescript-ts-mode yaml-ts-mode) . eglot-ensure)
  :bind (:map eglot-mode-map
              ("M-RET" . eglot-code-actions))
  :config
  ;; Big performance boost?
  (fset #'jsonrpc--log-event #'ignore)

  (add-to-list 'eglot-server-programs
             `((js-mode js-ts-mode tsx-ts-mode typescript-ts-mode typescript-mode)
               .
               ("typescript-language-server" "--stdio"
                :initializationOptions
                (:preferences
                 (:includeInlayParameterNameHints "all"
                  :includeInlayParameterNameHintsWhenArgumentMatchesName t
                  :includeInlayFunctionParameterTypeHints t
                  :includeInlayVariableTypeHints t
                  :includeInlayVariableTypeHintsWhenTypeMatchesName t
                  :includeInlayPropertyDeclarationTypeHints t
                  :includeInlayFunctionLikeReturnTypeHints t
                  :includeInlayEnumMemberValueHints t)))))
  (add-to-list 'eglot-server-programs '((elixir-ts-mode) "elixir-ls"))
  (add-to-list 'eglot-server-programs '(rust-mode "rust-analyzer"))
  (add-to-list 'eglot-server-programs '(terraform-mode "terraform-ls" "serve" "-port" :autoport))
  (add-to-list 'eglot-server-programs '(python-mode "pyright-langserver" "--stdio"))
  (setq eglot-autoshutdown t
        eglot-sync-connect nil
        eglot-autoreconnect nil)

  ;; Fix "proc macro Deserialize not expanded"
  (setq-default eglot-workspace-configuration
                `((:elixirLS . (:dialyzerEnabled :json-false))
                  (:rust-analyzer . (:procMacro
                                     (:enable t)
                                     :cargo
                                     (:loadOutDirsFromCheck t)))))

  (general-define-key :keymap 'eglot-mode-map "C-h ." 'eldoc-doc-buffer))

;; Use https://github.com/blahgeek/emacs-lsp-booster to speed up communication between
;; LSP server and Emacs
(use-package eglot-booster
  :if (executable-find "emacs-lsp-booster")
  :after eglot
  :ensure (eglot-booster :type git :host github :repo "jdtsmith/eglot-booster")
  :init (message "Enabling LSP booster for eglot")
  :config (eglot-booster-mode))

;; Misc
(use-package my-misc :ensure nil)

(use-package make-mode
  :ensure nil
  :defer
  ;; Use normal tabs in makefiles
  :hook (makefile-mode . (lambda () (setq indent-tabs-mode t))))

;; Highlight excessively long lines
(use-package whitespace
  :ensure nil
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

  (defun fix-diff-hl-faces (&rest _)
    (-each '(diff-hl-insert diff-hl-change diff-hl-delete)
      (lambda (face-name)
        (let ((background-color (face-attribute face-name :background nil t)))
          (set-face-attribute face-name nil :foreground background-color)))))
  (advice-add 'enable-theme :after #'fix-diff-hl-faces)
  :config
  (fix-diff-hl-faces))

(use-package transient :ensure (:host github :repo "emacs-mirror/emacs"))

(use-package magit
  :bind ("C-x g" . magit-status)
  :init
  ;; Fix evenp not being available in Emacs 30
  (when (not (functionp 'evenp))
    (require 'cl-lib)
    (defalias 'evenp 'cl-evenp))
  (general-evil-leader-define-key "g" #'magit-status)
  ;; Mark setup instructions as read
  (setq magit-last-seen-setup-instructions "1.4.0"
        ;; Silence nag on push
        magit-push-always-verify nil)

  ;; Make fine-grained changes more obvious
  (setq magit-diff-refine-hunk 'all)
  :config
  ;; git-spr integration
  (defun meqif/git-spr-update ()
    (interactive)
    (magit-shell-command "git spr update"))

  ;; Add git-spr to the "push" transient menu
  (transient-append-suffix 'magit-push "p" '("s" "spr update" meqif/git-spr-update))

  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

  (add-hook 'magit-status-mode-hook 'visual-line-mode)

  (add-to-list 'magit-repository-directories '("~/.emacs.d/" . 0))

  ;; Take over entire frame when displaying status
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)

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
  (setq evil-collection-mode-list (-remove-item 'lispy evil-collection-mode-list))
  (evil-collection-init))

(use-package browse-at-remote
  :defer
  :init
  (general-evil-leader-define-key "o" #'browse-at-remote)
  :config
  (setq browse-at-remote-prefer-symbolic nil))

(use-package diff-mode
  :defer t
  :ensure nil
  :config
  ;; Make fine grained changes more obvious
  (set-face-attribute 'diff-refine-added nil :bold t :background 'unspecified)
  (set-face-attribute 'diff-refine-changed nil :bold t :background 'unspecified)
  (set-face-attribute 'diff-refine-removed nil :bold t :background 'unspecified))

(use-package subword
  :ensure nil
  :hook (prog-mode . subword-mode)
  :delight "_")

;; Ruby mode
(use-package ruby-mode
  :ensure nil
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
        ruby-method-call-indent nil
        ruby-insert-encoding-magic-comment nil)

  (add-hook 'ruby-base-mode-hook
            #'(lambda ()
                (setq-local tab-width 2)
                (setq-local evil-shift-width 2)))

  ;; Ensure new RSpec buffers have `rspec-mode' enabled
  (add-hook 'ruby-base-mode-hook
            #'(lambda ()
                (when (s-ends-with? "_spec.rb" (buffer-file-name))
                  (rspec-mode +1))))

  (add-hook 'ruby-base-mode-hook #'meqif/set-fill-column-to-rubocop-max-line-length)

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

  (add-hook 'ruby-base-mode-hook #'maybe-set-docker-cwd))

(use-package rspec-mode
  :functions (rspec-verify
              rspec-verify-all
              rspec-verify-single
              rspec-toggle-spec-and-target-find-example
              rspec-run-test-subset
              hydra-rspec/body)
  :after ruby-base-mode
  :config
  (setq rspec-command-options "--format progress"
        rspec-use-docker-when-possible t
        rspec-docker-container "tests")

  (when (eq window-system 'mac)
    (defun rspec-run-current-file-in-devspace ()
      (interactive)
      (when (not rspec-mode)
        (error "Not an rspec-mode buffer!"))
      (-let* ((target-file (-when-let* ((buffer-name (buffer-file-name)))
                             (f-relative buffer-name (project-root (project-current)))))
              (iterm-command (format "bundle exec rspec %s" target-file))
              (main-command "python3 ~/.emacs.d/python/iterm2_remote_control.py"))
        (shell-command (format "%s %s" main-command iterm-command)))))

  (defun rspec-run-test-subset (type)
    (-let* ((relative-path (pcase type
                             ('unit "spec/unit")
                             ('acceptance "spec/acceptance")
                             (_ (error "Unknown test subset type"))))
            (path (concat (-some-> (project-current) (project-root)) relative-path)))
      (rspec-run-single-file path (rspec-core-options)))))

;; Handy functions to run rubocop from Emacs
(use-package rubocop
  :hook (ruby-base-mode . rubocop-mode)
  :config
  (setq rubocop-check-command "rubocop --format emacs --parallel"))

;; Automatically expand # to #{} inside double-quoted strings
(use-package ruby-tools
  :hook (ruby-base-mode . ruby-tools-mode)
  :delight "🛠")

(use-package rbs-mode)

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
  :ensure (default-text-scale :type git :host github :repo "purcell/default-text-scale"))

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
      ("f" cargo-process-fmt "format")))))

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
  :ensure nil
  :defer t
  :config
  ;; Show human-friendly file sizes and sort numbers properly
  (setq-default dired-listing-switches "-alhv")

  (let ((gnu-ls-path "/usr/local/bin/gls"))
    (when (and (s-equals? system-type "darwin") (f-exists? gnu-ls-path))
      (setq dired-use-ls-dired t
            insert-directory-program gnu-ls-path))))

(use-package ibuffer
  :ensure nil
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
  :ensure nil
  :init
  (evil-set-initial-state 'xref-mode 'normal)
  :config
  (setq xref-search-program 'ripgrep)
  (setq xref-after-jump-hook '(pulsar-recenter-middle pulsar-pulse-line))
  (setq xref-after-return-hook '(pulsar-recenter-middle pulsar-pulse-line)))

(use-package dumb-jump
  :defer t
  :config
  (add-hook 'dumb-jump-after-jump-hook #'pulsar-recenter-middle)
  (add-hook 'dumb-jump-after-jump-hook #'pulsar-pulse-line)
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

  ;; Esc also aborts
  (evil-define-key 'normal evil-multiedit-mode-map [escape] 'evil-multiedit-abort)

  ;; Jump to new matches
  (setq evil-multiedit-follow-matches t))

;; Better % jumping
(use-package evil-matchit
  :init (global-evil-matchit-mode)
  :config
  (evilmi-load-plugin-rules '(ruby-ts-mode) '(simple ruby)))

;; Convenient bindings for align functions
(use-package evil-lion
  :config
  (evil-lion-mode))

(use-package evil-commentary
  :after evil
  :config (evil-commentary-mode))

(use-package evil-textobj-tree-sitter
  :after evil
  :config
  ;; bind `function.outer`(entire function block) to `f` for use in things like `vaf`, `yaf`
  (define-key evil-outer-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.outer"))
  ;; bind `function.inner`(function block without name and args) to `f` for use in things like `vif`, `yif`
  (define-key evil-inner-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.inner")))

;; Unbind keys I accidentally hit too often
(unbind-key (kbd "s-&"))
(unbind-key (kbd "s-k"))
(unbind-key (kbd "s-p"))

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

(use-package smerge-mode :ensure nil :defer)

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
  :ensure nil
  :hook ((prog-mode yaml-mode yaml-ts-mode org-mode) . auto-insert-mode)
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
  :ensure nil
  :config
  ;; Make jumping from occur results easier to follow visually
  (add-hook 'occur-mode-find-occurrence-hook 'pulsar-recenter-middle)
  (add-hook 'occur-mode-find-occurrence-hook 'pulsar-pulse-line))

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

(use-package bookmark
  :ensure nil
  :config
  ;; Persist bookmarks to file after every change
  (setq bookmark-save-flag 1))

(use-package burly)

;; First try to indent the current line, and if the line
;; was already indented, then try `completion-at-point'
(setq tab-always-indent 'complete)

(use-package vundo
  :after general
  :ensure (vundo :type git :host github :repo "casouri/vundo")
  :config
  (general-evil-leader-define-key "u" #'vundo))

;; Annotate files without polluting them!
(use-package annotate)

(use-package emojify
  :defer t)

(use-package lua-mode
  :defer t)

(use-package jsonnet-mode
  :defer t)

(use-package csv-mode
  :defer t
  :hook (csv-mode . csv-align-mode))

(use-package pulsar
  :config
  (setq pulsar-pulse t)
  (setq pulsar-highlight-face 'pulsar-yellow)
  (pulsar-global-mode +1)

  ;; recenter and pulse line after popping the mark
  (advice-add 'meqif/pop-mark :after #'(lambda () (pulsar-pulse-line) (pulsar-recenter-middle)))

  ;; integration with errors in compilation buffers
  (add-hook 'next-error-hook #'pulsar-pulse-line)

  ;; integration with the `avy' package:
  (advice-add 'avy-action-goto :after #'(lambda (_) (pulsar-pulse-line) (pulsar-recenter-middle)))

  ;; integration with the `ctrlf' package:
  (advice-add 'ctrlf--finalize :after #'(lambda () (pulsar-pulse-line) (pulsar-recenter-middle)))

  ;; integration with the `deadgrep' package:
  (advice-add 'deadgrep--visit-result :after #'(lambda (_) (pulsar-pulse-line) (pulsar-recenter-middle)))

  ;; integration with the `consult' package:
  (add-hook 'consult-after-jump-hook #'pulsar-recenter-top)
  (add-hook 'consult-after-jump-hook #'pulsar-reveal-entry)

  ;; integration with the built-in `imenu':
  (add-hook 'imenu-after-jump-hook #'pulsar-recenter-top)
  (add-hook 'imenu-after-jump-hook #'pulsar-reveal-entry))

(use-package substitute
  :ensure (substitute :type git :host sourcehut :repo "meqif/substitute")
  :config
  (setq substitute-highlight t))

(use-package git-gutter
  :hook (prog-mode . git-gutter-mode)
  :config
  (setq git-gutter:window-width 0)
  (defhydra hydra-git-hunk-navigation ()
    "Navigate to hunk"
    ("j" #'(lambda () (interactive) (git-gutter:next-hunk 1) (recenter)) "next")
    ("n" #'(lambda () (interactive) (git-gutter:next-hunk 1) (recenter)) "next")
    ("k" #'(lambda () (interactive) (git-gutter:previous-hunk 1) (recenter)) "previous")
    ("p" #'(lambda () (interactive) (git-gutter:previous-hunk 1) (recenter)) "previous")
    ("q" nil "quit" :color blue))
  (general-evil-leader-define-key "j" #'hydra-git-hunk-navigation/body))

(use-package jinx
  :hook (text-mode . jinx-mode))

(use-package treesit-auto
  :config
  (setq treesit-auto-install 'prompt)
  (global-treesit-auto-mode))

(use-package elixir-ts-mode)

(use-package gleam-ts-mode
  :mode "\\.gleam\\'"
  :ensure (:repo "https://github.com/gleam-lang/gleam-mode.git" :branch "gleam-ts-mode"))

(use-package server
  :ensure nil
  :defer 2
  :delight server-buffer-clients
  ;; Start server if it isn't already running
  :config
  (unless (server-running-p) (server-start)))

;; After startup, set reasonable values for garbage collection
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold 16777216
                      gc-cons-percentage 0.1)))

