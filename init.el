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

;; Reduce how often garbage collection runs (which negatively impacts
;; performance)
(setq gc-cons-threshold 50000000)

;; Run garbage collection when the frame is unfocused
(add-hook 'focus-out-hook 'garbage-collect)

;; Disable splash screen
(setq inhibit-startup-message t)

;; Disable startup message in minibuffer
(defun display-startup-echo-area-message ()
  (message ""))

;; Set path to dependencies
(setq lisp-dir (expand-file-name "lisp" user-emacs-directory))

;; Set up load path
(add-to-list 'load-path lisp-dir)

(defun find-brew-prefix ()
  "Find Homebrew prefix"
  (substring (shell-command-to-string "brew --prefix") 0 -1))
(defvar brew-prefix (find-brew-prefix))

(defun add-subdirs-to-load-path (path)
  "Recursively add all subdirectories of the given path to load-path."
  (let ((default-directory path))
    (normal-top-level-add-subdirs-to-load-path)))

;; Add packages installed by Homebrew to the load path
(add-subdirs-to-load-path (concat brew-prefix "/share/emacs/site-lisp/"))

;; Keep emacs Custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" lisp-dir))
(load custom-file)

;; Bring some sanity in
(require 'sane-defaults)

;; Keyboard smooth scrolling: Prevent the awkward "snap to re-center" when
;; the text cursor moves off-screen. Instead, only scroll the minimum amount
;; necessary to show the new line. (A number of 101+ disables re-centering.)
(setq scroll-conservatively 101)

;; Make the minibuffer prompt intangible to stop it from being selectable or
;; navigable with the movement keys
;; Source: https://lists.gnu.org/archive/html/emacs-devel/2016-04/msg00857.html
(unless (version< emacs-version "25.0")
  (let ((default (eval (car (get 'minibuffer-prompt-properties 'standard-value))))
      (dont-touch-prompt-prop '(cursor-intangible t)))
  (setq minibuffer-prompt-properties (append default dont-touch-prompt-prop))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)))

;; Always ask before exiting Emacs
(setq confirm-kill-emacs 'yes-or-no-p)

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
                         ("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")))

(let ((trustfile (expand-file-name "~/.emacs.d/cacert.pem")))
  (unless (file-exists-p trustfile)
    (error (concat "No certificate bundle file found! "
                   "Please download it with "
                   (format "`curl https://curl.haxx.se/ca/cacert.pem -o %s`" trustfile))))
  (setq tls-checktrust 'always
        tls-program (list (format "gnutls-cli --x509cafile %s -p %%p %%h" trustfile))
        gnutls-verify-error t
        gnutls-trustfiles (list trustfile)))

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
         ("C-h v" . counsel-describe-variable))
  :config
  (define-key counsel-find-file-map (kbd "TAB") 'ivy-alt-done))

;; Use the fish shell in OSX
(when (eq system-type 'darwin)
  (setenv "SHELL" (concat brew-prefix "/bin/fish")))

;; Fix path
(use-package exec-path-from-shell
  :init (setq exec-path-from-shell-arguments '("-l"))
  :config (exec-path-from-shell-initialize))

;; Functions
(require 'defuns)

;; Keybindings
(require 'keybindings)

;; Appearance
(use-package appearance
  :ensure nil)

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
  (when (member "Symbola" (font-family-list))
    (set-fontset-font t 'symbol (font-spec :family "Symbola") frame 'prepend)))

;; For when Emacs is started in GUI mode:
(add-hook 'emacs-startup-hook #'(lambda () (--set-emoji-font nil)))
;; Hook for when a frame is created with emacsclient
;; see https://www.gnu.org/software/emacs/manual/html_node/elisp/Creating-Frames.html
(add-hook 'after-make-frame-functions '--set-emoji-font)

;; Unclutter modeline
(use-package diminish)

;; Smarter M-x
(use-package smex
  :init (smex-initialize)
  :config (setq smex-completion-method 'ivy))

(use-package eldoc
  :defer t
  :diminish "📖")

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
    (setq imenu-auto-rescan-maxout 120000)

    ;; Recenter buffer after jumping
    (add-hook 'imenu-after-jump-hook 'recenter)))

;; compilation-mode
(use-package compile
  :init
  (add-hook 'compilation-mode-hook 'visual-line-mode)
  :config
  ;; Scroll compile buffer automatically but stop on the first error
  (setq compilation-scroll-output 'first-error))

(use-package general
  :config
  (setq general-default-keymaps 'evil-normal-state-map)
  (setq general-default-prefix "\\")

  (general-define-key :prefix nil :keymaps 'global "<s-return>" #'promote-demote-window-dwim)

  ;; Global evil leader shortcuts
  (general-define-key
   "f" 'projectile-or-counsel-find-file
   "p" 'counsel-yank-pop
   "b" 'ivy-switch-buffer
   "r" 'counsel-recentf
   "l" 'avy-goto-line
   "g" 'magit-status
   "s" 'counsel-grep-or-swiper
   "S" #'(lambda ()
           (interactive)
           (let ((current-prefix-arg '(4)))
             (call-interactively 'counsel-rg)))
   "\\" 'meqif/pop-mark)

  ;; LaTeX leader shortcuts
  (general-define-key :keymaps 'latex-mode-map :states 'normal
                      "s" 'flyspell-buffer
                      "t" #'(lambda () (interactive) (TeX-insert-macro "todo"))
                      "cc" 'TeX-command-master
                      "cv" 'TeX-view)

  ;; Org-mode leader shortcuts
  (general-define-key :keymaps 'org-mode-map :states 'normal "ce" #'org-export-dispatch))

(use-package avy
  :demand
  :bind ("C-c SPC" . avy-goto-char-timer)
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
  (general-define-key "<SPC>" 'avy-goto-char-timer))

(use-package ace-window
  :init
  (general-define-key "w" #'ace-window)
  :config
  (set-face-attribute 'aw-leading-char-face nil :height 10.0))

(use-package expand-region
  :defer t
  :config
  (general-define-key "e" #'er/expand-region))

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
  (general-define-key "m" #'hydra-multiple-cursors/body))

;; Save a list of recent files visited
(use-package recentf
  :init
  ;; Increase recent entries list from default (20)
  (setq recentf-max-saved-items 100)
  :config
  ;; Exclude installed packages
  (push (f-join user-emacs-directory ".cask") recentf-exclude))

;; Misery loves this
(use-package company
  :diminish company-mode
  :config
  ;; Enable company mode for every programming major mode
  (add-hook 'prog-mode-hook 'company-mode)

  (define-key prog-mode-map (kbd "TAB") #'company-indent-or-complete-common)

  (setq
   ;; Offer completions quickly
   company-idle-delay 0.1
   ;; Align tooltips
   company-tooltip-align-annotations t
   ;; Start completing after two chars
   company-minimum-prefix-length 2
   ;; Wrap around candidate list
   company-selection-wrap-around t
   ;; Fix lowercase candidates
   company-dabbrev-downcase nil)

  ;; Traverse candidates with TAB and BACKTAB
  (define-key company-active-map (kbd "TAB") 'company-select-next)
  (define-key company-active-map [tab] 'company-select-next)
  (define-key company-active-map (kbd "BACKTAB") 'company-select-previous)
  (define-key company-active-map [backtab] 'company-select-previous)

  ;; Make ESC abort the completion popup
  (--each
      (list company-active-map
            company-filter-map
            company-mode-map
            company-search-map)
    (define-key it [escape] 'company-abort)))

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
  (define-key flycheck-error-list-mode-map (kbd "k") 'previous-line))

(use-package flycheck-status-emoji
  :after flycheck
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

    ;; Organizing stuff
    ;; http://sachachua.com/blog/2015/02/learn-take-notes-efficiently-org-mode/

    ;; Set default notes file
    (setq org-default-notes-file "~/organizer.org")

    ;; Shortcut to capture notes
    (global-set-key (kbd "C-c c") 'org-capture)

    ;; Hydra
    (defhydra hydra-org-mode-narrow (:color blue)
      "Narrow buffer to"
      ("s" org-narrow-to-subtree "Subtree")
      ("b" org-narrow-to-block "Block")
      ("w" widen "Widen"))
    (general-define-key :keymaps 'org-mode-map :states 'normal "n" #'hydra-org-mode-narrow/body))
  :config
  ;; Org-latex configuration
  (use-package ox-latex
    :ensure org
    :defer t
    :config
    ;; Use latexmk and xelatex to generate PDFs
    (setq org-latex-pdf-process '("latexmk -pdflatex=xelatex -pdf -f %f"))
    ;; Default packages
    (add-to-list 'org-latex-packages-alist
                 '("" "MinionPro" nil))
    (add-to-list 'org-latex-packages-alist
                 '("" "microtype" nil)))

  ;; Add Github-Flavored Markdown exporter
  (use-package ox-gfm :defer t)

  ;; Allow editing html blocks
  (use-package ox-html :ensure org))

;; Use org-mode tables in any mode
(use-package org-table
  :ensure nil
  :commands orgtbl-mode)

(use-package yasnippet
  :diminish yas-minor-mode
  :config
  (progn
    ;; Use only own snippets, do not use bundled ones
    (setq yas-snippet-dirs (list (expand-file-name "~/.emacs.d/snippets")))

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
                (setq-local 'require-final-newline nil)))))

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
(use-package markdown-mode
  :mode ("\\.md\\'" . gfm-mode)
  :config
  (setq markdown-command "marked --gfm")
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
              (push '("function" . ?ƒ) prettify-symbols-alist))))

(use-package json-mode
  :mode "\\.json\\'"
  :config
  (add-hook 'json-mode-hook (lambda (setq js-indent-level 2))))

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
    (general-define-key :keymaps 'latex-mode-map :states 'normal "f" #'hydra-latex-fonts/body)

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
  (require 'lsp-rust)
  (add-hook 'rust-mode-hook #'lsp-mode)
  ;; Add brackets to smartparens pair list
  (sp-local-pair 'rust-mode "<" ">")

  (add-hook 'rust-mode-hook
            (lambda ()
              ;; Enable on-the-fly syntax checking
              (flycheck-mode 1)
              ;; Do the required setup
              (flycheck-rust-setup)
              ;; Rust has different rules for too long lines
              (setq-local fill-column 100)
              ;; Reload whitespace mode to make the previous change effective
              (whitespace-mode -1)
              (whitespace-mode 1)
              ;; Prettify some symbols
              (--each '((">=" . (?· (Br . Bl) ?≥))
                        ("<=" . (?· (Br . Bl) ?≤))
                        ("!=" . (?· (Br . Bl) ?≠))
                        ("=>" . (?· (Br . Bl) ?➡))
                        ("->" . (?· (Br . Bl) ?→))
                        )
                (push it prettify-symbols-alist))))

  (use-package cargo
    :config
    ;; Hydra for rust's cargo
    (defhydra hydra-cargo (:color blue :columns 4)
      "cargo"
      ("c" cargo-process-build "build")
      ("tt" cargo-process-test "test all")
      ("tf" cargo-process-current-test "test current function")
      ("b" cargo-process-bench "benchmark all")
      ("C" cargo-process-clean "clean")
      ("dd" cargo-process-doc "build documentation")
      ("do" cargo-process-doc-open "build and open documentation")
      ("r" cargo-process-run "run")
      ("y" cargo-process-clippy "clippy"))
    (general-define-key :keymaps 'rust-mode-map :states 'normal "c" #'hydra-cargo/body))

  ;;   ;; Register rust-mode in company dabbrev code modes
  ;;   (add-to-list 'company-dabbrev-code-modes 'rust-mode)

  ;; Handy keybindings
  (--each
      '(("C-c C-c" . "cargo build")
        ("C-c C-t" . "cargo test")
        ("C-c C-r" . "cargo run"))
    (-let* (((keycombo . command) it))
      (define-key rust-mode-map (kbd keycombo)
        `(lambda () (interactive) (save-buffer) (compile ,command)))))

  ;; Compile a single file
  (defun meqif/compile-single-rust-file ()
    (interactive)
    (when (and (f-exists? (buffer-name))
               (f-file? (buffer-name)))
      (compile (concat "rustc " (buffer-name) " -o " (f-no-ext (buffer-name))))))

  ;; Useful rustup bindings
  (progn
    (require 'ivy)
    (defvar rustup-binary "rustup")

    (defun rustup--set-default-toolchain (version)
      (let ((short-version (car (split-string version))))
        (shell-command (concat rustup-binary " default " short-version))))

    (defun rustup--list-toolchains ()
      (split-string (shell-command-to-string
                     (concat rustup-binary " toolchain list"))))

    (defun rustup--full-toolchains ()
      (let ((toolchains (rustup--list-toolchains)))
        (--zip-with
         (concat it " (" other ")")
         toolchains
         (--map
          (cadr
           (split-string
            (shell-command-to-string
             (concat rustup-binary " run " it " rustc --version"))))
          (rustup--list-toolchains)))))

    (defun rustup-change-toolchain ()
      (interactive)
      (let ((ivy-count-format ""))
        (ivy-read "Rust version: " (rustup--full-toolchains)
                  :action #'rustup--set-default-toolchain)))

    (defun rustup--run-cargo-clippy ()
      (compile (concat rustup-binary " run nightly cargo clippy")))
    )
  )

(use-package flyspell
  :defer
  :init
  ;; Use Aspell for spellcheck
  (setq ispell-program-name (concat brew-prefix "/bin/aspell"))
  (setq ispell-list-command "--list")

  ;; Default language is Portuguese.
  (setq ispell-dictionary "pt_PT")

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
  (advice-add 'org-mode-flyspell-verify :filter-return 'org-mode-flyspell-verify-ignore-blocks)

  :config
  (use-package flyspell-correct-ivy
    :defer t
    :config
    ;; set ivy as correcting interface
    (setq flyspell-correct-interface 'flyspell-correct-ivy)
    ;; bind flyspell-correct-word-generic
    (define-key flyspell-mode-map (kbd "C-;") 'flyspell-correct-word-generic)))


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
  (setq whitespace-style '(face lines-tail))
  (setq-default whitespace-line-column nil)
  (add-hook 'prog-mode-hook 'whitespace-mode))

(use-package undo-tree
  :diminish undo-tree-mode
  :config
  ;; Show timestamps
  (setq undo-tree-visualizer-timestamps t)
  ;; Split undo-tree side-by-side, like decent people do.
  (defun undo-tree-split-side-by-side (original-function &rest args)
    "Split undo-tree side-by-side"
    (let ((split-height-threshold nil)
          (split-width-threshold 0))
      (apply original-function args)))
  (advice-add 'undo-tree-visualize :around #'undo-tree-split-side-by-side))

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
  :pin melpa-stable
  :bind ("C-x g" . magit-status)
  :init
  ;; Mark setup instructions as read
  (setq magit-last-seen-setup-instructions "1.4.0"
        ;; Silence nag on push
        magit-push-always-verify nil
        magit-revert-buffers t)
  ;; Make fine-grained changes more obvious
  (setq magit-diff-refine-hunk 'all)
  :config
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

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
        ;; Use ivy to complete magit's prompts
        magit-completing-read-function 'ivy-completing-read)

  ;; Make <SPC> insert dashes instead. Useful when creating new branches
  (define-key magit-minibuffer-local-ns-map "\s" "-")

  (defun guess-jira-ticket-identifier ()
    "Attempt to extract JIRA identifier from git branch name.

It only really works if the branch follows the PROJECT-TICKETNUMBER-description
naming scheme."
    (when-let ((current-branch (magit-get-current-branch)))
      (-first-item (s-match "^[A-Z]+-[0-9]+" current-branch))))

  (defun append-jira-ticket-identifier ()
    (when-let ((jira-ticket-identifier (guess-jira-ticket-identifier)))
      (goto-char (point-min))
      (insert "\n\n")
      (insert jira-ticket-identifier)
      (goto-char (point-min))))

  (add-hook 'git-commit-setup-hook 'append-jira-ticket-identifier)
  :config
  (use-package evil-magit
    :config
    (evil-define-key evil-magit-state magit-mode-map "\\" nil)))

(use-package diff-mode
  :ensure nil
  :config
  ;; Make fine grained changes more obvious
  (set-face-attribute 'diff-refine-added nil :bold t :background 'unspecified)
  (set-face-attribute 'diff-refine-changed nil :bold t :background 'unspecified)
  (set-face-attribute 'diff-refine-removed nil :bold t :background 'unspecified))

(use-package subword
  :defer t
  :diminish "_")

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
            #'(lambda ()
                (setq mode-name "💎")
                (setq-local tab-width 2)
                (setq-local evil-shift-width 2)))

  ;; Run specs on save
  ;; (firestarter-mode)
  (defun rspec-verify-firestarter ()
    (interactive)
    (when (s-matches-p "\.rb$" (buffer-name))
      (rspec-verify)))
  (defun rspec-firestarter ()
    (setq firestarter #'rspec-verify-firestarter))
  (add-hook 'ruby-mode-hook #'rspec-firestarter)
  )

(use-package enh-ruby-mode
  :defer
  :config
  (add-hook 'enh-ruby-mode-hook
            #'(lambda ()
                (setq mode-name "💎🕷")
                (setq-local tab-width 2)
                (setq-local evil-shift-width 2)
                (subword-mode)
                (flycheck-mode))))

(use-package rspec
  :ensure rspec-mode
  :defer
  :after (ruby-mode enh-ruby-mode)
  :init
  (setq rspec-command-options "--format progress")
  (defhydra hydra-rspec (:color blue)
    "rspec"
    ("a" rspec-verify-all "run all specs")
    ("s" rspec-verify-single "run specs for this context")
    ("v" rspec-verify "run specs for this buffer")
    ("t" rspec-toggle-spec-and-target-find-example
     "toggle between spec and class"))
  (general-define-key :keymaps '(ruby-mode-map enh-ruby-mode-map) :states 'normal "c" #'hydra-rspec/body))

;; Handy functions to run rubocop from Emacs
(use-package rubocop
  :after (ruby-mode enh-ruby-mode))

;; Automatically expand # to #{} inside double-quoted strings
(use-package ruby-tools
  :after (ruby-mode enh-ruby-mode)
  :diminish (ruby-tools-mode . "🛠"))

(use-package inf-ruby
  :defer
  :config
  (add-hook 'ruby-mode-hook #'inf-ruby-minor-mode))

;; Better completion and documentation access for Ruby
(use-package robe
  :defer
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

(use-package lispy
  :defer t
  :init
  (add-hook 'emacs-lisp-mode-hook #'lispy-mode)
  (add-hook 'clojure-mode-hook #'lispy-mode))

(use-package lispyville
  :defer t
  :init
  (add-hook 'lispy-mode-hook #'lispyville-mode))

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
     ("en" #'(lambda ()
               (interactive)
               (setq-local ispell-local-dictionary "en_GB"))
      "change language to en_GB")
     ("pt" #'(lambda ()
               (interactive)
               (setq-local ispell-local-dictionary "pt_PT"))
      "change language to pt_PT")
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
     ("w" widen "widen")))

  (defhydra hydra-zoom ()
    "zoom"
    ("i" text-scale-increase "in")
    ("o" text-scale-decrease "out")
    ("0" (text-scale-adjust 0) "reset")
    ("q" nil "quit" :color blue))
  (general-define-key "z" #'hydra-zoom/body))

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
  (setq-default dired-listing-switches "-alhv"))

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer))

(defun meqif/jump-to-definition ()
  (interactive)
  (funcall (case major-mode
             ('rust-mode 'racer-find-definition)
             (t 'dumb-jump-go))))

(use-package dumb-jump
  :defer t
  :commands (dumb-jump-go dumb-jump-back)
  :init
  ;; (define-key evil-normal-state-map (kbd "C-]") 'dumb-jump-go)
  (define-key evil-normal-state-map (kbd "C-]") 'meqif/jump-to-definition)
  (define-key evil-normal-state-map (kbd "C-t") 'dumb-jump-back)
  :config
  (setq dumb-jump-selector 'ivy))

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

;; Unbind s-&, as I hit it accidentally too often and it kills the buffer. 😞
(unbind-key (kbd "s-&"))
(unbind-key (kbd "s-k"))

(use-package faun-mode
  :ensure nil
  :after 'org
  :load-path "lisp/"
  :diminish (faun-mode . "👹"))

;; Wrap lines in visual-line-mode at the fill column
(use-package visual-fill-column
  :disabled
  :init
  (add-hook 'visual-line-mode-hook 'visual-fill-column-mode))

(use-package clojure-mode
  :defer t
  :init
  (add-hook 'clojure-mode-hook 'cider-mode))

(use-package cider
  :after 'clojure-mode
  :init
  (setq cider-repl-display-help-banner nil)
  :config
  (evil-define-key 'normal cider-repl-mode-map (kbd "j") 'cider-repl-previous-input)
  (evil-define-key 'normal cider-repl-mode-map (kbd "k") 'cider-repl-next-input)
  (--each '('normal 'insert)
    (progn
      (evil-define-key it cider-repl-mode-map (kbd "<up>") 'cider-repl-previous-input)
      (evil-define-key it cider-repl-mode-map (kbd "<down>") 'cider-repl-next-input))))

;; Diffs like vimdiff
(use-package vdiff
  :defer
  :config
  (setq ediff-split-window-function 'split-window-horizontally))

(use-package ess
  :defer t
  :pin melpa-stable
  :config
  (add-hook 'ess-mode-hook 'company-mode)
  (add-hook 'inferior-ess-mode-hook 'company-mode))

;; Improve readability of ELisp regular expressions
(use-package easy-escape
  :init
  (add-hook 'lisp-mode-hook 'easy-escape-minor-mode)
  (add-hook 'emacs-lisp-mode-hook 'easy-escape-minor-mode))

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
