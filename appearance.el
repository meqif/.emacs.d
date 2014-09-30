;; Appearance

; I like line numbers, thankyouverymuch.

(global-linum-mode 1)

; (add-to-list 'load-path "~/.emacs.d/color-theme-6.6.0")
; (require 'color-theme)
; (color-theme-initialize)
;
; (load "~/.emacs.d/colorschemes/color-theme-molokai.el")
; (color-theme-molokai)

(setq custom-theme-directory "~/.emacs.d/themes")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/emacs-color-theme-solarized")
;(load-theme 'default-black t)
(load-theme 'solarized-light t)

;; mac friendly font
(when window-system
  (setq magnars/default-font "-apple-Monaco-medium-normal-normal-*-16-*-*-*-m-0-iso10646-1")
  (setq meqif/default-font "-apple-Source Code Pro-medium-normal-normal-*-14-*-*-*-m-0-iso10646-1")
  (setq magnars/presentation-font "-apple-Monaco-medium-normal-normal-*-28-*-*-*-m-0-iso10646-1")
  (set-face-attribute 'default nil :font meqif/default-font)
)

(when window-system (set-frame-size (selected-frame) 120 40))

;; Highlight current line
(global-hl-line-mode 1)

;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)
(setq show-paren-delay 0)


;; Unclutter modeline
(require 'diminish)
(eval-after-load "undo-tree" '(diminish 'undo-tree-mode))
(eval-after-load "smartparens" '(diminish 'smartparens-mode))
(eval-after-load "magit" '(diminish 'magit-auto-revert-mode))

(defmacro rename-modeline (package-name mode new-name)
  `(eval-after-load ,package-name
     '(defadvice ,mode (after rename-modeline activate)
        (setq mode-name ,new-name))))

(rename-modeline "js2-mode" js2-mode "JS2")

(provide 'appearance)
