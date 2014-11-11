(require 'reftex)

(setq helm-bibtex-bibliography "bibliography.bib")
(add-to-list 'reftex-default-bibliography "bibliography.bib")

;; Use LaTeX autocite even in org-mode.
(add-to-list 'helm-bibtex-format-citation-functions
             '(org-mode . (lambda (keys)
                            (format "\\autocite{%s}" (s-join ", " keys)))))

(setq helm-source-bibtex
      '((name . "BibTeX entries")
        (init . helm-bibtex-init)
        (candidates . helm-bibtex-candidates)
        (filtered-candidate-transformer . helm-bibtex-candidates-formatter)
        (action
         ("Insert citation" . helm-bibtex-insert-citation)
         ("Insert reference" . helm-bibtex-insert-reference)
         ("Insert BibTeX key" . helm-bibtex-insert-key)
         ("Insert BibTeX entry" . helm-bibtex-insert-bibtex)
         ("Attach PDF to email" . helm-bibtex-add-PDF-attachment)
         ("Edit notes" . helm-bibtex-edit-notes)
         ("Open PDF file (if present)" . helm-bibtex-open-pdf)
         ("Open URL or DOI in browser" . helm-bibtex-open-url-or-doi)
         ("Show entry" . helm-bibtex-show-entry))))

(provide 'setup-helm-bibtex)
