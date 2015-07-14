;; Mark start point for load time measurement
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

;; Keep emacs Custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" lisp-dir))
(load custom-file)

;; Bring some sanity in
(require 'sane-defaults)

;; Packages
(require 'cask "~/homebrew/share/emacs/site-lisp/cask.el")
(cask-initialize)
(require 'pallet)
(pallet-mode t)
(require 'use-package)

(use-package dash
  :config (dash-enable-font-lock))
  :defer t

;; Fix path
(use-package exec-path-from-shell
  :config (exec-path-from-shell-initialize))

;; Functions
(require 'defuns)

;; Keybindings
(require 'keybindings)

;; Appearance
(require 'appearance)

;; Unclutter modeline
(use-package diminish)

;; Smarter M-x
(use-package smex
  :init (smex-initialize)
  :bind ("M-x" . smex))

;; Awesome project navigation
(use-package projectile
  :diminish projectile-mode
  :defer 5
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
(use-package ido
  :demand t
  :config
  (ido-mode t)
  (setq ido-enable-prefix nil
        ido-enable-flex-matching t
        ido-create-new-buffer 'always
        ido-use-filename-at-point nil
        ido-max-prospects 10))

(use-package helm-imenu
  :bind ("s-r" . helm-imenu))

(use-package ace-jump-mode
  :defer t
  :bind ("C-c SPC" . ace-jump-mode)
  :config
  (progn
    (define-key global-map (kbd "C-c SPC") 'ace-jump-mode)))

(use-package expand-region
  :defer t
  :bind ("C-=" . er/expand-region))

(use-package multiple-cursors
  :defer t
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)))

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
  :defer t
  :config
  (progn
    (autoload 'wgrep-ag-setup "wgrep-ag")
    (add-hook 'ag-mode-hook 'wgrep-ag-setup)))

(use-package flycheck
  :defer t
  :config
  (setq-default flycheck-display-errors-delay 0.5))

(use-package org
  :defer 30
  :init
  (progn
    ;; Don't truncate lines
    (setq org-startup-truncated nil
          ;; Enable syntax highlighting in source blocks
          org-src-fontify-natively t
          ;; Add more capture templates
          org-capture-templates
          '(("t" "Todo" entry (file+headline "~/org/tasks.org" "Tasks")
             "* TODO %?\n  %i\n  %a")
            ("j" "Dissertation" entry (file+datetree "~/org/dissertation.org")
             "* %?\nEntered on %U\n  %i\n  %a")
            ("b" "Blog post idea" entry (file+headline "~/org/blog/post_ideas.org" "Ideas")
             "* %?")))
    ;; Enable wordwrap
    (add-hook 'org-mode-hook 'visual-line-mode)

    ;; Organizing stuff
    ;; http://sachachua.com/blog/2015/02/learn-take-notes-efficiently-org-mode/

    ;; Set default notes file
    (setq org-default-notes-file "~/organizer.org")

    ;; Shortcut to open notes file
    (global-set-key (kbd "C-c o")
                    (lambda () (interactive) (find-file "~/org/tasks.org")))
    (global-set-key (kbd "C-c d")
                    (lambda () (interactive) (find-file "~/org/dissertation.org")))
    (global-set-key (kbd "C-c b")
                    (lambda () (interactive) (find-file "~/org/blog/post_ideas.org")))

    ;; Shortcut to capture notes
    (global-set-key (kbd "C-c c") 'org-capture)))

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
    (yas-global-mode)

    (add-hook 'snippet-mode-hook
              (lambda ()
                ;; Temporarily disable required newline at the end of file
                ;; This fixes the problem with an extra newline when expanding snippets
                (set (make-local-variable 'require-final-newline) nil)))))

(use-package helm-bibtex
  :defer t
  :config
  (progn
    (require 'reftex)

    ;; Set default bibliography file
    (setq helm-bibtex-bibliography "bibliography.bib")
    (add-to-list 'reftex-default-bibliography "bibliography.bib")

    ;; Use LaTeX autocite even in org-mode.
    (add-to-list
     'helm-bibtex-format-citation-functions
     '(org-mode . (lambda (keys) (format "\\autocite{%s}" (s-join ", " keys)))))))

;; Language-specific setup files
(load-config
    ;; 'tex-mode      'setup-latex
    'markdown-mode 'setup-markdown
  'js2-mode      'setup-javascript)

(use-package tex-mode
  :mode ("\\.tex\\'" . LaTeX-mode)
  :config
  (progn
    (use-package flyspell)

    ;; Default options
    (setq TeX-auto-save t)
    (setq TeX-parse-self t)
    (setq-default TeX-master nil)

    ;; Generate PDF by default
    (setq TeX-PDF-mode t)

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

    ;; Quick spellcheck shortcut
    (add-hook 'LaTeX-mode-hook
              (lambda()
                (define-key LaTeX-mode-map (kbd "§") 'flyspell-buffer)
                (define-key LaTeX-mode-map (kbd "C-c b") 'helm-bibtex)
                (define-key LaTeX-mode-map (kbd "<M-up>") 'outline-move-subtree-up)
                (define-key LaTeX-mode-map (kbd "<M-down>") 'outline-move-subtree-down)
                (define-key LaTeX-mode-map (kbd "C-c t") #'(lambda () (interactive) (TeX-insert-macro "todo")))))

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
                ;; 34 is the code for " (double quote)
                (push '(34 . ("``" . "''")) evil-surround-pairs-alist)))

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
  :defer t
  :config
  (progn
    ;; Enable Emmet in tag soup modes
    (add-hook 'sgml-mode-hook 'emmet-mode)
    (add-hook 'mustache-mode-hook 'emmet-mode)))

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
                (whitespace-mode 1)))

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

(use-package js2-mode
  :diminish (js2-mode "JS2")
  :mode ("\\.js$" "\\.json$"))

;; Easier kill-ring viewing
(use-package browse-kill-ring
  :defer t
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
  :diminish whitespace-mode
  :config
  (progn
    (setq whitespace-line-column 80
          whitespace-style '(face lines-tail))
    (add-hook 'prog-mode-hook 'whitespace-mode)))

(use-package undo-tree
  :diminish undo-tree-mode
  ;; Show timestamps
  :config (setq undo-tree-visualizer-timestamps t))

(use-package magit
  :diminish magit-auto-revert-mode
  :bind ("C-x g" . magit-status)
  :init
  ;; Mark setup instructions as read
  (setq magit-last-seen-setup-instructions "1.4.0")
  :config
  (progn
    (setq magit-set-upstream-on-push t)
    ;; Verbose commits (show changes to be commited) by default
    (advice-add #'magit-key-mode-popup-committing :after
                (lambda ()
                  (magit-key-mode-toggle-option 'committing "--verbose")))))

(use-package enh-ruby-mode
  :mode "\\.rb\\'"
  ;; Don't deep indent arrays and hashes
  :config (setq enh-ruby-deep-ident-paren nil))

;; Better package management
(use-package paradox
  :defer 5
  ;; Always update in background
  :config (setq paradox-execute-asynchronously t))

;; Better interactive search
(use-package swiper
  :defer t
  :init (global-set-key (kbd "C-s") 'swiper))

;; Enable paredit for Emacs Lisp
(use-package paredit
  :init
  (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode))

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
     ("q" nil "quit"))))

(use-package ivy
  :defer t
  :config
  ;; Allow quitting ivy with ESC
  (define-key ivy-minibuffer-map [escape] 'minibuffer-keyboard-quit))

;; Macro expansion for ease of debugging
(use-package macrostep
  :defer t
  :config (define-key emacs-lisp-mode-map (kbd "C-c e") 'macrostep-expand))

;; Unbind s-&, as I hit it accidentally too often and it kills the buffer. :(
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
