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

;; Shorten nyan cat bar length
(eval-after-load "nyan-mode" (setq-default nyan-bar-length 16))
(autoload 'nyan-create "nyan-mode") ; Nyan doesn't how to autoload :(

;; Enable nyan cat :3
(nyan-mode)

(defvar mein-projectile-mode-line
  '(:propertize
    (:eval (when (ignore-errors (projectile-project-root))
             ;; (concat " " (projectile-project-name))))
             (projectile-project-name)))
    face font-lock-constant-face)
  "Mode line format for Projectile.")
(put 'mein-projectile-mode-line 'risky-local-variable t)

(defvar mein-vc-mode-line
  '(" " (:propertize
         ;; Strip the backend name from the VC status information
         (:eval (let ((backend (symbol-name (vc-backend (buffer-file-name)))))
                  (substring vc-mode (+ (length backend) 2))))
         face font-lock-variable-name-face))
  "Mode line format for VC Mode.")
(put 'mein-vc-mode-line 'risky-local-variable t)

(setq-default mode-line-format
              '("%e" mode-line-front-space
                ;; Standard info about the current buffer
                mode-line-mule-info
                mode-line-client
                mode-line-modified
                mode-line-remote
                mode-line-frame-identification
                mode-line-buffer-identification " " mode-line-position
                ;; Show evil-mode state
                evil-mode-line-tag
                ;; Project information
                mein-projectile-mode-line
                ;; Version control information
                mein-vc-mode-line
                ;; Misc information
                " "
                mode-line-misc-info
                ;; Finally, the modes
                " " mode-line-modes mode-line-end-spaces
                )
              mode-line-position
              '((:eval (when nyan-mode (nyan-create)))
                " "
                (line-number-mode
                 ("%l" (column-number-mode ":%c"))))
              )

;; Unclutter modeline
(require 'diminish)
(eval-after-load "undo-tree" '(diminish 'undo-tree-mode))
(eval-after-load "smartparens" '(diminish 'smartparens-mode))
(eval-after-load "magit" '(diminish 'magit-auto-revert-mode))
(eval-after-load "projectile" '(diminish 'projectile-mode))

(defmacro rename-modeline (package-name mode new-name)
  `(eval-after-load ,package-name
     '(defadvice ,mode (after rename-modeline activate)
        (setq mode-name ,new-name))))

(rename-modeline "js2-mode" js2-mode "JS2")

(provide 'appearance)
