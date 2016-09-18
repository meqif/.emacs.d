;; Mark start point for load time measurement

;; (package-initialize)

(defconst emacs-start-time (current-time))
(unless noninteractive
  (message "Loading %s..." load-file-name))

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

(defun find-brew-prefix ()
  "Find Homebrew prefix"
  (substring (shell-command-to-string "brew --prefix") 0 -1))

(defun add-subdirs-to-load-path (path)
  "Recursively add all subdirectories of the given path to load-path."
  (let ((default-directory path))
    (normal-top-level-add-subdirs-to-load-path)))

;; Add packages installed by Homebrew to the load path
(add-subdirs-to-load-path (concat (find-brew-prefix) "/share/emacs/site-lisp/"))

;; Keep emacs Custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" lisp-dir))
(load custom-file)

;; Bring some sanity in
(require 'sane-defaults)

;; Make the minibuffer prompt intangible to stop it from being selectable or
;; navigable with the movement keys
;; Source: https://lists.gnu.org/archive/html/emacs-devel/2016-04/msg00857.html
(unless (version< emacs-version "25.0")
  (let ((default (eval (car (get 'minibuffer-prompt-properties 'standard-value))))
      (dont-touch-prompt-prop '(cursor-intangible t)))
  (setq minibuffer-prompt-properties (append default dont-touch-prompt-prop))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)))

;; Always ask before exiting Emacs
(global-set-key
 (kbd "s-q")
 (lambda ()
   (interactive)
   (set (make-local-variable 'confirm-kill-emacs) 'yes-or-no-p)
   (save-buffers-kill-emacs)))

;; Packages
(require 'cask)
(cask-initialize)

(unless (package-installed-p 'pallet)
  (package-refresh-contents)
  (package-install 'pallet))
(require 'pallet)
(pallet-mode t)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(setq use-package-enable-imenu-support t
      use-package-always-ensure t)
(require 'use-package)

;; Use https for package archives
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

(use-package dash
  :defer t
  :config (dash-enable-font-lock))

(use-package ivy
  :defer t
  :config
  ;; Allow quitting ivy with ESC
  (define-key ivy-minibuffer-map [escape] 'minibuffer-keyboard-quit))

;; Replace default functions with much better alternatives
(use-package counsel
  :defer t
  :bind (("s-r" . counsel-imenu)
         ("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-h f" . counsel-describe-function)
         ("C-h v" . counsel-describe-variable)))

;; Use the fish shell in OSX
(when (eq system-type 'darwin)
  (setenv "SHELL" (expand-file-name "~/homebrew/bin/fish")))

;; Fix path
(use-package exec-path-from-shell
  :defer 1
  :config (exec-path-from-shell-initialize))

;; Functions
(require 'defuns)

;; Keybindings
(require 'keybindings)

;; Appearance
(use-package appearance
  :ensure nil)

;; (set-fontset-font t 'unicode "Apple Color Emoji" nil 'prepend)
(set-fontset-font t 'unicode "Symbola" nil 'prepend)
(when (eq window-system 'mac)
  (set-fontset-font t 'symbol "Symbola" nil 'prepend))
;; 😎😎
;; (defun --set-emoji-font (frame)
;;   "Adjust the font settings of FRAME so Emacs can display emoji properly."
;;   (if (eq system-type 'darwin)
;;       ;; For NS/Cocoa
;;       (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") frame 'prepend)
;;     ;; For Linux
;;     (set-fontset-font t 'symbol (font-spec :family "Symbola") frame 'prepend)))

;; ;; For when Emacs is started in GUI mode:
;; (--set-emoji-font nil)
;; ;; Hook for when a frame is created with emacsclient
;; ;; see https://www.gnu.org/software/emacs/manual/html_node/elisp/Creating-Frames.html
;; (add-hook 'after-make-frame-functions '--set-emoji-font)

;; Unclutter modeline
(use-package diminish)

;; Smarter M-x
(use-package smex
  :init (smex-initialize)
  :bind ("M-x" . smex)
  :config (setq smex-completion-method 'ivy))

;; Awesome project navigation
(use-package projectile
  :diminish projectile-mode
  :defer 5
  :init
  (projectile-global-mode)
  ;; Use ivy for completion
  (setq projectile-completion-system 'ivy)
  (--each '("node_modules" "vendor" ".bundle")
    (add-to-list 'projectile-globally-ignored-directories it))
  )

(use-package smartparens
  :diminish smartparens-mode
  :init
  (progn
    (smartparens-global-mode t)
    ;; Show matching parentheses
    (show-smartparens-global-mode t)
    ;; Remove matching parentheses delay
    (setq sp-show-pair-delay 0)
    ;; Load default smartparens configuration
    (require 'smartparens-config)))

;; Setup extensions
(use-package setup-evil :ensure nil)

;; Port of vim-textobj-anyblock -- easy block selection
(use-package evil-textobj-anyblock
  :config
  (define-key evil-inner-text-objects-map "b" 'evil-textobj-anyblock-inner-block)
  (define-key evil-outer-text-objects-map "b" 'evil-textobj-anyblock-a-block))

;; Enable IDO mode
(use-package ido
  :demand t
  :config
  (ido-mode t)
  (setq ido-enable-prefix nil
        ido-enable-flex-matching t
        ido-create-new-buffer 'always
        ido-use-filename-at-point nil
        ido-max-prospects 10))

(use-package imenu
  :config
  (progn
    ;; Make imenu rescan automatically
    (setq imenu-auto-rescan t)

    ;; Make imenu work on larger files
    (setq imenu-auto-rescan-maxout 120000)))

(use-package compile
  :config
  ;; Scroll compile buffer automatically but stop on the first error
  (setq compilation-scroll-output 'first-error))

(use-package avy
  :demand
  :bind ("C-c SPC" . avy-goto-word-1)
  :init
  (defun avy-goto-ibuffer ()
    "Select and visit buffer"
    (interactive)
    (let ((beg nil)
          (end nil)
          (avy-keys (number-sequence ?0 ?9)))
      (save-excursion
        (goto-char (point-min))
        (forward-line 2)
        (setq beg (point))
        (goto-char (point-max))
        (forward-line -1)
        (setq end (point)))
      (avy-action-goto (avy--line nil beg end))
      (ibuffer-visit-buffer)))
  ;; (add-hook 'ibuffer-hook #'avy-goto-ibuffer)
  :config
  (evil-leader/set-key "<SPC>" 'avy-goto-word-1))

(use-package expand-region
  :defer t)

(use-package multiple-cursors
  :defer t
  :config
  (defhydra hydra-multiple-cursors (:color red)
    "multiple cursors"
    (";" mc/mark-previous-like-this "mark previous like this")
    (":" mc/mark-next-like-this "mark next like this")
    ("a" mc/mark-all-like-this "mark all like this")
    ("d" mc/mark-all-dwim "mark all dwim")
    ("m" mc/mark-more-like-this-extended "mark more extended")
    ("el" mc/edit-lines "edit lines"))
  (evil-leader/set-key "m" #'hydra-multiple-cursors/body))

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
    ;; (setq company-idle-delay 0.1)

    ;; Start completing after two chars
    (setq company-minimum-prefix-length 2)

    ;; Wrap around candidate list
    (setq company-selection-wrap-around t)

    ;; Fix lowercase candidates
    (setq company-dabbrev-downcase nil)

    ;; Traverse candidates with TAB and BACKTAB
    (define-key company-active-map (kbd "TAB") 'company-select-next)
    (define-key company-active-map [tab] 'company-select-next)
    (define-key company-active-map (kbd "BACKTAB") 'company-select-previous)
    (define-key company-active-map [backtab] 'company-select-previous)))

;; Unique buffer names
(use-package uniquify
  :ensure nil
  ;; Make uniquify rename buffers like in path name notation
  :config (setq uniquify-buffer-name-style 'forward))

(use-package wgrep
  :defer t
  :config
  (progn
    (autoload 'wgrep-ag-setup "wgrep-ag")
    (add-hook 'ag-mode-hook 'wgrep-ag-setup)))

(use-package flycheck
  :defer t
  :config
  (setq-default flycheck-display-errors-delay 0.5)
  (define-key flycheck-error-list-mode-map (kbd "j") 'next-line)
  (define-key flycheck-error-list-mode-map (kbd "k") 'previous-line)

  (flycheck-define-checker rust-clippy
    "A lint checker based on rust-clippy."

    :command ("rustup" "run" "nightly" "cargo" "clippy")
    :error-patterns
    ((error line-start (file-name) ":" line ":" column ": "
            (one-or-more digit) ":" (one-or-more digit) " error: "
            (or
             ;; Multiline errors
             (and (message (minimal-match (one-or-more anything)))
                  " [" (id "E" (one-or-more digit)) "]")
             (message))
            line-end)
     (warning line-start (file-name) ":" line ":" column ": "
              (one-or-more digit) ":" (one-or-more digit) " warning: "
              (message) line-end)
     (info line-start (file-name) ":" line ":" column ": "
           (one-or-more digit) ":" (one-or-more digit) " " (or "note" "help") ": "
           (message) line-end))
    :modes rust-mode)
  )

(use-package flycheck-status-emoji
  :config (add-hook 'flycheck-mode-hook #'flycheck-status-emoji-mode))

(use-package org
  :defer
  :init
  (progn
    ;; Don't truncate lines
    (setq org-startup-truncated nil
          ;; Stop org-mode from replacing my window-moving keys
          ;; Has to be defined before loading org-mode
          org-replace-disputed-keys t
          ;; Enable syntax highlighting in source blocks
          org-src-fontify-natively t
          ;; Don't export a table of content by default
          org-export-with-toc nil
          ;; Place footnotes at the end of the current outline node
          org-footnote-section nil
          ;; Change the collapsed-tree ellipsis to something more compact
          org-ellipsis "⤵")
    ;; Enable wordwrap
    (add-hook 'org-mode-hook 'visual-line-mode)
    ;; Break lines automatically
    (add-hook 'org-mode-hook 'auto-fill-mode)

    ;; Org-latex configuration
    (use-package ox-latex
      :defer t
      :config
      ;; Use latexmk and xelatex to generate PDFs
      (setq org-latex-pdf-process '("latexmk -pdflatex=xelatex -pdf -f %f"))
      ;; Default packages
      (add-to-list 'org-latex-packages-alist
                   '("" "MinionPro" nil))
      (add-to-list 'org-latex-packages-alist
                   '("" "microtype" nil)))

    ;; Organizing stuff
    ;; http://sachachua.com/blog/2015/02/learn-take-notes-efficiently-org-mode/

    ;; Set default notes file
    (setq org-default-notes-file "~/organizer.org")

    ;; Shortcut to capture notes
    (global-set-key (kbd "C-c c") 'org-capture)))

(use-package yasnippet
  :diminish yas-minor-mode
  :config
  (progn
    ;; Use only own snippets, do not use bundled ones
    (setq yas-snippet-dirs (expand-file-name "~/.emacs.d/snippets"))

    ;; Don't mess with the indentation
    (setq yas-indent-line 'fixed)

    ;; No need to be so verbose
    (setq yas-verbosity 1)

    ;; Snippets everywhere
    (yas-global-mode)

    ;; ... but not on compilation-mode buffers
    (defun meqif/compilation-buffer-p ()
      ;; Return t if the current buffer is a compilation-mode buffer
      (compilation-buffer-p (current-buffer)))
    (add-to-list 'yas-dont-activate-functions #'meqif/compilation-buffer-p)

    (add-hook 'snippet-mode-hook
              (lambda ()
                ;; Temporarily disable required newline at the end of file
                ;; This fixes the problem with an extra newline when expanding snippets
                (set (make-local-variable 'require-final-newline) nil)))))

(use-package auto-yasnippet)

(defun configure-helm-bibtex ()
  (progn
    (require 'reftex)

    ;; Set default bibliography file
    (if (file-exists-p "bibliography.bib")
        (setq helm-bibtex-bibliography (list "bibliography.bib"))
      (setq helm-bibtex-bibliography
            (list (expand-file-name "~/bibliography.bib"))))

    (add-to-list
     (make-local-variable 'reftex-default-bibliography)
     "bibliography.bib")
    ))

(use-package helm-bibtex
  :defer t
  :config
  (unless (boundp 'helm-bibtex-bibliography) (configure-helm-bibtex))
  ;; Change default action
  (progn
    (helm-delete-action-from-source "Insert BibTeX key" helm-source-bibtex)
    (helm-add-action-to-source
     "Insert BibTeX key" 'helm-bibtex-insert-key helm-source-bibtex 0))
  ;; Use LaTeX autocite even in org-mode.
  (add-to-list
   'helm-bibtex-format-citation-functions
   '(org-mode . (lambda (keys) (format "\\autocite{%s}" (s-join ", " keys))))))

(use-package ivy-bibtex
  :defer t
  :config (unless (boundp 'helm-bibtex-bibliography) (configure-helm-bibtex)))

;; Language-specific setup files
(use-package gfm-mode
  :mode "\\.md\\'"
  :ensure markdown-mode
  :config
  (add-hook 'gfm-mode-hook #'visual-line-mode))

(use-package js2-mode
  :mode "\\.js\\'"
  :config
  ;; Easier refactoring
  (use-package js2-refactor)

  (setq-default js2-global-externs
                '("module" "export" "require" "describe" "it" "before" "after"))

  ;; Let flycheck handle parse errors
  (setq-default js2-mode-show-parse-errors nil)
  (setq-default js2-strict-missing-semi-warning nil)
  ;; jshint does not warn about this now for some reason
  (setq-default js2-strict-trailing-comma-warning t)

  (add-hook 'js2-mode-hook
            (lambda ()
              ;; Enable flycheck
              (flycheck-mode 1)
              ;; Use symbol for anonymous functions
              (push '("function" . ?ƒ) prettify-symbols-alist)
              ;; Enable ternjs (requires tern to be installed through npm)
              (tern-mode t)
              ;; Add tern.js backend
              (add-to-list 'company-backends 'company-tern))))

(use-package json-mode
  :mode "\\.json\\'")

(use-package dockerfile-mode
  :mode "Dockerfile\\'")

(use-package tex-mode
  :mode ("\\.tex\\'" . LaTeX-mode)
  :config
  (progn
    ;; Default options
    (setq TeX-auto-save t)
    (setq TeX-parse-self t)
    (setq-default TeX-master nil)

    ;; Generate PDF by default
    (setq TeX-PDF-mode t)

    ;; Open TeX error overview automatically after compiling
    (setq TeX-error-overview-open-after-TeX-run t)

    ;; Explicitly add XeLaTeX to the available commands
    (eval-after-load "tex"
      '(add-to-list 'TeX-command-list
                    '("XeLaTeX" "xelatex -interaction=nonstopmode %s"
                      TeX-run-command t t :help "Run xelatex") t))

    ;; Set default viewer
    ;; (setq TeX-view-program-list '(("Skim" "open -a Skim.app %(outpage) %o")))

    ;; Use Skim as viewer, enable source <-> PDF sync
    ;; make latexmk available via C-c C-c
    ;; Note: SyncTeX is setup via ~/.latexmkrc (see below)
    (add-hook 'LaTeX-mode-hook (lambda ()
                                 (push
                                  '("latexmk" "latexmk -synctex=1 -pdf %s" TeX-run-TeX nil t
                                    :help "Run latexmk on file")
                                  TeX-command-list)))
    (add-hook 'LaTeX-mode-hook '(lambda () (setq TeX-command-default "latexmk")))

    ;; use Skim as default pdf viewer
    ;; Skim's displayline is used for forward search (from .tex to .pdf)
    ;; option -b highlights the current line; option -g opens Skim in the background
    (setq TeX-view-program-selection '((output-pdf "PDF Viewer")))
    (setq TeX-view-program-list
          '(("PDF Viewer" "/Applications/Skim.app/Contents/SharedSupport/displayline -b %n %o %b")))


    ;; Set default TeX engine
    ;; (setq TeX-engine 'xetex)
    (setq TeX-engine 'luatex) ; LuaTeX works great, even though it's slow

    ;; Enable rainbow delimiters (makes it easier to catch stray curly brackets)
    (add-hook 'LaTeX-mode-hook 'rainbow-delimiters-mode)

    ;; Enable spellcheck for LaTeX
    (add-hook 'LaTeX-mode-hook 'flyspell-mode)

    (defhydra hydra-latex-fonts (:color blue)
      "LaTeX font"
      ("b" (TeX-font nil ?\C-b) "bold")
      ("i" (TeX-font nil ?\C-i) "italic")
      ("t" (TeX-font nil ?\C-t) "monospace")
      ("e" (TeX-font nil ?\C-e) "emphasis"))
    (evil-leader/set-key-for-mode 'latex-mode "f" #'hydra-latex-fonts/body)

    ;; Autosave before compiling
    (setq TeX-save-query nil)

    ;; Enable word wrapping
    (setq-local word-wrap t)

    ;; Enable RefTeX
    (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
    (setq reftex-plug-into-AUCTeX t)

    ;; Add biblatex style cite formats
    (eval-after-load 'reftex-vars
      '(progn
         (setq reftex-cite-format
               '((?\C-m . "\\cite[]{%l}")
                 (?f . "\\footcite[][]{%l}")
                 (?t . "\\textcite[]{%l}")
                 (?p . "\\parencite[]{%l}")
                 (?o . "\\citepr[]{%l}")
                 (?n . "\\nocite{%l}")
                 (?a . "\\autocite[]{%l}")))))

    ;; Expand command regexp with biblatex stuff
    ;; (setq flyspell-tex-command-regexp)

    ;; Enable wordwrap
    (add-hook 'LaTeX-mode-hook 'visual-line-mode)

    ;; Set XeLaTeX as the default command
    (add-hook 'LaTeX-mode-hook
              (lambda ()
                (setq TeX-command-default "XeLaTeX")))

    ;; Add `` and '' to evil-surrond
    (add-hook 'LaTeX-mode-hook
              (lambda ()
                (push '(?\" . ("``" . "''")) evil-surround-pairs-alist)))

    ;; Override <TAB> to jump out of a pair of curly braces if the character
    ;; following the point is a closing curly brace.
    ;; This is much easier to type on an international layout.
    (defun meqif/latex-skip-closing-curly-brace ()
      (interactive)
      (when (char-equal ?} (following-char)) (forward-char)))

    (add-hook 'LaTeX-mode-hook
              (lambda ()
                (define-key LaTeX-mode-map
                  (kbd "<tab>") #'meqif/latex-skip-closing-curly-brace)
                (evil-define-key 'normal LaTeX-mode-map (kbd "<tab>")
                  #'meqif/latex-skip-closing-curly-brace)))

    ;; Set up imenu properly
    (setq imenu-generic-expression
          '(("*Part*" "\\s-*\\\\part{\\(.+\\)}" 1)
            ("*Chapter*" "\\s-*\\\\chapter{\\(.+\\)}" 1)
            ("*Section*" "\\s-*\\\\section{\\(.+\\)}" 1)
            ("*Subsection*" "\\s-*\\\\subsection{\\(.+\\)}" 1)
            ("*Subsubsection*" "\\s-*\\\\subsubsection{\\(.+\\)}" 1)
            ("*Paragraph*" "\\s-*\\\\paragraph{\\(.+\\)}" 1)
            ("*Subparagraph*" "\\s-*\\\\subparagraph{\\(.+\\)}" 1)))
    ))

(use-package cc-mode
  :defer t
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
  :defer t
  :config
  (progn
    ;; Enable tagedit
    (use-package tagedit)
    ;; Auto-close tags and other goodies
    (tagedit-add-experimental-features)
    (add-hook 'html-mode-hook (lambda () (tagedit-mode 1)))
    (add-hook 'mustache-mode (lambda () (tagedit-mode 1)))))

(use-package emmet-mode
  :defer
  :init
  (add-hook 'sgml-mode-hook 'emmet-mode)
  (add-hook 'mustache-mode-hook 'emmet-mode))

(use-package mustache-mode
  :mode "\\.hjs\\'")

(use-package scss-mode
  :defer t
  ;; Don't autocompile SCSS, I usually have task runners doing that
  :config (setq scss-compile-at-save nil))

(use-package rust-mode
  :defer t
  :config
  (progn
    (use-package racer
      :init
      (setq racer-rust-src-path "~/rust/src/")
      :config
      (add-hook 'rust-mode-hook #'racer-mode)
      (add-hook 'rust-mode-hook #'eldoc-mode)
      (define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common))
    (add-hook 'rust-mode-hook
              (lambda ()
                ;; Enable on-the-fly syntax checking
                (flycheck-mode 1)
                ;; Do the required setup
                (flycheck-rust-setup)
                ;; Rust has different rules for too long lines
                (setq-local fill-column 100)
                (setq-local whitespace-line-column 100)
                ;; Reload whitespace mode to make the previous change effective
                (whitespace-mode -1)
                (whitespace-mode 1)
                ;; Prettify some symbols
                (--each '((">=" . (?· (Br . Bl) ?≥))
                          ("<=" . (?· (Br . Bl) ?≤))
                          ("!=" . (?· (Br . Bl) ?≠))
                          ("=>" . (?· (Br . Bl) ?➡))
                          )
                  (push it prettify-symbols-alist))))

    ;; Register rust-mode in company dabbrev code modes
    (add-to-list 'company-dabbrev-code-modes 'rust-mode)

    ;; Add brackets to smartparens pair list
    (sp-local-pair 'rust-mode "<" ">")

    ;; Handy keybindings
    (--each
        '(("C-c C-c" . "cargo build")
          ("C-c C-t" . "cargo test")
          ("C-c C-r" . "cargo run"))
      (-let* (((keycombo . command) it))
        (define-key rust-mode-map (kbd keycombo)
          `(lambda () (interactive) (save-buffer) (compile ,command)))))))

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

;; Easier kill-ring viewing
(use-package browse-kill-ring
  :defer t
  :config
  ;; Highlight the current entry in browse-kill-ring
  (setq browse-kill-ring-highlight-current-entry t)
  ;; VIM-like navigation between candidates
  (define-key browse-kill-ring-mode-map (kbd "j") #'browse-kill-ring-forward)
  (define-key browse-kill-ring-mode-map (kbd "k") #'browse-kill-ring-previous))

;; Misc
(require 'my-misc)

;; Highlight excessively long lines
(use-package whitespace
  :diminish whitespace-mode
  :config
  (progn
    (setq whitespace-line-column 80
          whitespace-style '(face lines-tail))
    (add-hook 'prog-mode-hook 'whitespace-mode)))

(use-package undo-tree
  :diminish undo-tree-mode
  :config
  ;; Show timestamps
  (setq undo-tree-visualizer-timestamps t)
  ;; Split undo-tree side-by-side, like decent people do.
  (defadvice undo-tree-visualize (around undo-tree-split-side-by-side activate)
    "Split undo-tree side-by-side"
    (let ((split-height-threshold nil)
          (split-width-threshold 0))
      ad-do-it)))

;; Enable diff indication on the fringe
(use-package diff-hl
  :commands diff-hl-magit-post-refresh
  :init
  (add-hook 'prog-mode-hook 'turn-on-diff-hl-mode)
  (add-hook 'vc-dir-mode-hook 'turn-on-diff-hl-mode))

(use-package ag
  :defer
  :config
  ;; Highlight ag's search results
  (setq ag-highlight-search t))

(use-package magit
  :bind ("C-x g" . magit-status)
  :init
  ;; Mark setup instructions as read
  (setq magit-last-seen-setup-instructions "1.4.0"
        ;; Silence nag on push
        magit-push-always-verify nil
        magit-revert-buffers t)
  :config
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (progn
    (setq magit-set-upstream-on-push t)
    ;; Fix gravatars in commit log in OSX
    (when (eq system-type 'darwin)
      (setq magit-revision-use-gravatar-kludge t))
    ;; Verbose commits (show changes to be commited) by default
    (advice-add #'magit-key-mode-popup-committing :after
                (lambda ()
                  (magit-key-mode-toggle-option 'committing "--verbose")))))

(use-package subword
  :defer t
  :diminish "sub")

;; Ruby mode
(use-package ruby-mode
  :mode ("\\.rb\\'"
         "\\.rake\\'"
         "Rakefile\\'"
         "\\.gemspec\\'"
         "\\.ru\\'"
         "Gemfile\\'"
         "Guardfile\\'"
         "Capfile\\'"
         "Vagrantfile\\'")
  ;; Don't deep indent arrays and hashes
  :config
  (setq ruby-deep-ident-paren nil
        ruby-insert-encoding-magic-comment nil)
  (add-hook 'ruby-mode-hook #'subword-mode)
  (add-hook 'ruby-mode-hook #'flycheck-mode)
  (add-hook 'ruby-mode-hook
            (lambda () (setq-local tab-width 2)
              (setq-local evil-shift-width 2)))

  (use-package rspec
    :defer
    :init
    (setq rspec-command-options "--format progress")
    (defhydra hydra-rspec (:color blue)
      "rspec"
      ("a" rspec-verify-all "run all specs")
      ("s" rspec-verify-single "run specs for this context")
      ("v" rspec-verify "run specs for this buffer")
      ("t" rspec-toggle-spec-and-target-find-example
       "toggle between spec and class")))

  (evil-leader/set-key-for-mode 'ruby-mode "c" #'hydra-rspec/body)

  ;; Run specs on save
  (firestarter-mode)
  (defun rspec-verify-firestarter ()
    (interactive)
    (when (s-matches-p "\.rb$" (buffer-name))
      (rspec-verify)))
  (defun rspec-firestarter ()
    (setq firestarter #'rspec-verify-firestarter))
  (add-hook 'ruby-mode-hook #'rspec-firestarter)

  ;; Automatically expand # to #{} inside double-quoted strings
  (use-package ruby-tools
    :diminish ruby-tools-mode))

(use-package inf-ruby
  :defer
  :config
  (add-hook 'ruby-mode-hook #'inf-ruby-minor-mode))

;; Better completion and documentation access for Ruby
(use-package robe
  :defer
  :init (add-hook 'enh-ruby-mode-hook 'robe-mode)
  :config
  ;; Add robe to company mode backends
  (push 'company-robe company-backends))

;; Better package management
(use-package paradox
  :defer 5
  ;; Always update in background
  :init (use-package spinner)
  :config
  (setq paradox-execute-asynchronously t
        paradox-github-token t
        paradox-automatically-star nil)
  (evil-add-hjkl-bindings paradox-menu-mode-map 'emacs
    (kbd "J") #'paradox-next-describe
    (kbd "K") #'paradox-previous-describe
    (kbd "H") #'paradox-menu-quick-help
    (kbd "L") #'(lambda (pkg) (interactive '(nil)) (paradox-menu-view-commit-list pkg))))

;; Enable paredit for Emacs Lisp
(use-package paredit
  :init
  (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode))

;; Group reusable keyboard bindings behind a common prefix
(use-package hydra
  :config
  ;; Easy error navigation
  (global-set-key
   (kbd "M-ç")
   (defhydra hydra-flyspell (:color red)
     "spellchecking"
     ("r" flyspell-buffer "rerun on buffer")
     ("n" flyspell-goto-next-error "next")
     ("c" ispell-word "correct word")
     ("q" nil "quit")))
  (global-set-key
   (kbd "M-g")
   (defhydra hydra-error (:color red)
     "goto-error"
     ("h" first-error "first")
     ("n" next-error "next")
     ("p" previous-error "prev")
     ("v" recenter-top-bottom "recenter")
     ("q" nil "quit")))
  (global-set-key
   (kbd "M-n")
   (defhydra hydra-narrow (:color blue)
     "narrow"
     ("f" narrow-to-defun "to function")
     ("p" narrow-to-page "to page")
     ("r" narrow-to-region "to region")
     ("w" widen "widen"))))


;; Macro expansion for ease of debugging
(use-package macrostep
  :defer t
  :config
  (define-key emacs-lisp-mode-map (kbd "C-c e") 'macrostep-expand)
  ;; Make macrostep play well with evil
  (evil-define-key 'normal macrostep-keymap
    "q" 'macrostep-collapse-all))

(use-package dired
  :ensure nil
  :defer t
  :config
  ;; Show human-friendly file sizes and sort numbers properly
  (setq-default dired-listing-switches "-alhv"))

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer))

;; Unbind s-&, as I hit it accidentally too often and it kills the buffer. 😞
(unbind-key (kbd "s-&"))

;; Post initialization -- calculate loading time
;; Copied from jwiegley's configuration
(when window-system
  (let ((elapsed (float-time (time-subtract (current-time) emacs-start-time))))
    (message "Loading %s...done (%.3fs)" load-file-name elapsed))

  (add-hook 'after-init-hook
            `(lambda ()
               (let ((elapsed (float-time
                               (time-subtract (current-time) emacs-start-time))))
                 (message "Loading %s...done (%.3fs) [after-init]"
                          ,load-file-name elapsed)))
            t))

(use-package server
  :diminish server-buffer-clients
  ;; Start server if it isn't already running
  :config
  (unless (server-running-p) (server-start)))
