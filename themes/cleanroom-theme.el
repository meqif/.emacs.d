;; Author:  Vy-Shane Sin Fat <shane@node.mu>
;; Author:  Ricardo Martins <ricardo@scarybox.net>

(deftheme cleanroom "Emacs port of the vim cleanroom theme")
  (custom-theme-set-faces 'cleanroom
   '(default ((t (:foreground "#111111" :background "#ffffff"))))
   '(cursor ((t (:background "#ff7311"))))
   '(fringe ((t (:background "#f8f8f8"))))
   '(linum ((t (:foreground "#bebebe" :background "#f8f8f8"))))
   '(nlinum-current-line ((t (:foreground "#a52a2a" :background "white" :bold t))))
   '(hl-line ((t (:background "#f1faff"))))
   '(highlight ((t (:background "#fcfcaa"))))
   '(mode-line ((t (:foreground "#555555" :background "#e3e3e3"))))
   '(mode-line-inactive ((t (:foreground "#888888" :background "#eeeeee"))))
   '(region ((t (:background "#ddeeff"))))
   '(secondary-selection ((t (:background "#cddbec"))))
   '(font-lock-builtin-face ((t (:foreground "#4477bb"))))
   '(font-lock-comment-face ((t (:foreground "#667799"))))
   '(font-lock-function-name-face ((t (:foreground "#111111"))))
   '(font-lock-keyword-face ((t (:foreground "#005070"))))
   '(font-lock-string-face ((t (:foreground "#667799"))))
   '(font-lock-type-face ((t (:foreground "#1a1a1a"))))
   '(font-lock-constant-face ((t (:foreground "#113388"))))
   '(font-lock-variable-name-face ((t (:foreground "#1a1a1a"))))
   '(font-lock-preprocessor-face ((t (:foreground "#6699dd" :bold t))))
   '(minibuffer-prompt ((t (:foreground "#7299ff" :bold t))))
   '(font-lock-warning-face ((t (:foreground "black" :background "#ffdddd"))))
   '(error ((t (:foreground "#bb3355" :bold t))))
   '(isearch ((t (:background "#ffff33" :bold t))))
   '(lazy-highlight ((t (:background "#fcfcaa"))))
   '(sp-show-pair-match-face ((t (:background "#ccddff"))))
   ;; company
   '(company-tooltip ((t (:foreground "#60656f" :background "#f0f5ff"))))
   '(company-tooltip-selection ((t (:foreground "white" :background "#3585ef" :bold t))))
   '(company-tooltip-common ((t (:bold t))))
   '(company-tooltip-annotation ((t (:foreground "black"))))
   '(company-scrollbar-bg ((t (:background "#e0e5ee" :bold t))))
   '(company-scrollbar-fg ((t (:background "#d0d5dd" :bold t))))
   '(diff-added ((t (:background "#ecffec"))))
   '(diff-changed ((t (:background "#ffffe6"))))
   '(diff-removed ((t (:background "#fff1f1"))))
   '(flyspell-incorrect ((t (:underline (:style wave :color "#ff7777")))))
   ;; ivy
   '(ivy-current-match ((t (:background "#ddeeff" :bold t))))
   '(ivy-minibuffer-match-face-1 ((t (:background "#f0f5ff"))))
   '(ivy-minibuffer-match-face-2 ((t (:background "#fcfcaa" :bold t))))
   '(ivy-minibuffer-match-face-3 ((t (:background "#ff7777" :bold t))))
   '(ivy-minibuffer-match-face-4 ((t (:background "#667799" :bold t))))
   ;; org headers
   '(org-level-1 ((t (:foreground "#113388" :height 1.1 :bold t))))
   '(org-level-2 ((t (:foreground "#3585ef" :height 1.1 :bold t))))
   '(org-level-3 ((t (:foreground "#4477bb" :height 1.1 :bold t))))
   '(org-level-4 ((t (:foreground "#667799" :height 1.1 :bold t))))
   ;; '(org-level-5 ((t (:foreground "#667799" :height 1.1 :bold t))))
   ;; '(org-level-6 ((t (:foreground "#667799" :height 1.1 :bold t))))
   ;; '(org-level-7 ((t (:foreground "#667799" :height 1.1 :bold t))))
   ;; '(org-level-8 ((t (:foreground "#667799" :height 1.1 :bold t))))
   ;; dired
   '(dired-directory ((t (:foreground "#6699dd"))))
  )

;;;###autoload
(and load-file-name
    (boundp 'custom-theme-load-path)
    (add-to-list 'custom-theme-load-path
                 (file-name-as-directory
                  (file-name-directory load-file-name))))
;; Automatically add this theme to the load path

(provide-theme 'cleanroom)
