;; The following was taken from:
;; http://orgmode.org/worg/org-faq.html#using-xelatex-for-pdf-export

(require 'org-latex)
(setq org-export-latex-listings t)

;; Originally taken from Bruno Tavernier: http://thread.gmane.org/gmane.emacs.orgmode/31150/focus=31432
;; but adapted to use latexmk 4.20 or higher.
(defun my-auto-tex-cmd ()
  "When exporting from .org with latex, automatically run latex,
     pdflatex, or xelatex as appropriate, using latexmk."
  (let ((texcmd)))
  ;; default command: oldstyle latex via dvi
  (setq texcmd "latexmk -dvi -pdfps -quiet %f")
  ;; pdflatex -> .pdf
  (cond
   ((string-match "LATEX_CMD: pdflatex" (buffer-string))
       (setq texcmd "latexmk -pdf -quiet %f"))
   ;; xelatex -> .pdf
   ((string-match "LATEX_CMD: xelatex" (buffer-string))
    (setq texcmd "latexmk -pdflatex=xelatex -pdf -quiet %f"))
   ((string-match "LATEX_CMD: lualatex" (buffer-string))
    (setq texcmd "latexmk -pdflatex=lualatex -pdf -quiet %f")))
  ;; LaTeX compilation command
  (setq org-latex-to-pdf-process (list texcmd)))

(add-hook 'org-export-latex-after-initial-vars-hook 'my-auto-tex-cmd)

;; Specify default packages to be included in every tex file, whether pdflatex or xelatex
(setq org-export-latex-packages-alist
      '(("" "graphicx" t)
            ("" "longtable" nil)
            ("" "float" nil)))

(defun my-auto-tex-parameters ()
      "Automatically select the tex packages to include."
      ;; default packages for ordinary latex or pdflatex export
      (setq org-export-latex-default-packages-alist
            '(("AUTO" "inputenc" t)
              ("T1"   "fontenc"   t)
              (""     "fixltx2e"  nil)
              (""     "wrapfig"   nil)
              (""     "soul"      t)
              (""     "textcomp"  t)
              (""     "marvosym"  t)
              (""     "wasysym"   t)
              (""     "latexsym"  t)
              (""     "amssymb"   t)
              (""     "hyperref"  nil)))

      ;; Packages to include when xelatex or lualatex is used
      (if (or (string-match "LATEX_CMD: xelatex" (buffer-string))
              (string-match "LATEX_CMD: lualatex" (buffer-string)))
          (progn
            (setq org-export-latex-default-packages-alist
                  '(("" "fontspec" t)
                    ("" "url" t)
                    ("" "hyperref" nil)
                    ))
            (setq org-export-latex-classes
                  (cons '("article"
                          "\\documentclass[11pt,article,oneside]{memoir}"
                          ("\\section{%s}" . "\\section*{%s}")
                          ("\\subsection{%s}" . "\\subsection*{%s}")
                          ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                          ("\\paragraph{%s}" . "\\paragraph*{%s}")
                          ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
                        org-export-latex-classes)))))

(add-hook 'org-export-latex-after-initial-vars-hook 'my-auto-tex-parameters)

;; Don't truncate lines
(setq org-startup-truncated nil)

;; Enable wordwrap
(add-hook 'org-mode-hook 'visual-line-mode)

(provide 'setup-org-mode)
