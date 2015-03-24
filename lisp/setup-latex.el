(require 'flyspell)

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
(setq TeX-view-program-list '(("Skim" "open -a Skim.app %(outpage) %o")))

;; Set default TeX engine
; (setq TeX-engine 'xetex)
(setq TeX-engine 'luatex) ; LuaTeX works great, although it's slow

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

(defun flyspell-check-next-highlighted-word ()
  "Custom function to spell check next highlighted word"
  (interactive)
  (flyspell-goto-next-error)
  (ispell-word)
  )
(global-set-key (kbd "<f5>") 'flyspell-check-next-highlighted-word)


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

(provide 'setup-latex)
