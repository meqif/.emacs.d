(require 'reftex)

(setq helm-bibtex-bibliography "bibliography.bib")
(add-to-list 'reftex-default-bibliography "bibliography.bib")

;; Use LaTeX autocite even in org-mode.
(add-to-list 'helm-bibtex-format-citation-functions
             '(org-mode . (lambda (keys)
                            (format "\\autocite{%s}" (s-join ", " keys)))))

(provide 'setup-helm-bibtex)
