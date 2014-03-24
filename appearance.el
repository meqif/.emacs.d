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

(setq mode-line-format
      (quote
       (" "
        mode-line-mule-info
        mode-line-modified
        mode-line-frame-identification
        mode-line-buffer-identification
        "   "
        global-mode-string
        "   %[("
        ;; mode-line-mode-name is a function
        ;; that copies the mode name and adds text
        ;; properties to make it mouse-sensitive.
        (:eval (mode-line-mode-name))
        mode-line-process
        minor-mode-alist
        "%n"
        ")%]  "
        (which-func-mode ("" which-func-format " "))
        (line-number-mode "%l")
	","
        (column-number-mode "%c    ")
)))

;; Unclutter modeline
(require 'diminish)
(eval-after-load "undo-tree" '(diminish 'undo-tree-mode))
(eval-after-load "smartparens" '(diminish 'smartparens-mode))

(provide 'appearance)
