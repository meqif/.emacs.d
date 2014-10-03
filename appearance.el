;; Appearance

; I like line numbers, thankyouverymuch.
(global-linum-mode 1)

;; Clean previous themes definitions when loading a theme
(defadvice load-theme
  (before theme-dont-propagate activate)
  (mapc #'disable-theme custom-enabled-themes))

;; Custom tweaks to solarized-theme
(setq
 ;; Fix underscored mode-line
 x-underline-at-descent-line t
 ;; Don't use as much bold
 solarized-use-less-bold t
 ;; Disable variable pitch fonts in Solarized theme
 solarized-use-variable-pitch nil
 ;; Don't add too much colours to the fringe
 solarized-emphasize-indicators nil
 ;; Fix the fringe and linum
 solarized-distinct-fringe-background t)

;; I prefer the show-paren faces of the official solarized theme to bbatsov's
;; version
(custom-set-faces
 '(show-paren-match ((t (:background "#e9e2cb" :foreground "#259185" :weight bold))))
 '(show-paren-mismatch ((t (:background "#81908f" :foreground "#c60007" :weight bold)))))

;; Load theme
;; (setq custom-theme-directory "~/.emacs.d/themes")
;; (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/emacs-color-theme-solarized")
(load-theme 'solarized-light t)

;; mac friendly font
(when window-system
  (setq magnars/default-font "-apple-Monaco-medium-normal-normal-*-16-*-*-*-m-0-iso10646-1")
  (setq meqif/default-font "-apple-Source Code Pro-medium-normal-normal-*-14-*-*-*-m-0-iso10646-1")
  (setq magnars/presentation-font "-apple-Monaco-medium-normal-normal-*-28-*-*-*-m-0-iso10646-1")
  (set-face-attribute 'default nil :font meqif/default-font)
  (set-frame-size (selected-frame) 120 40)
)

;; Highlight current line
(global-hl-line-mode 1)

;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)
(setq show-paren-delay 0)

;; Make show-paren highlight the entire expression
(setq show-paren-style 'expression)

;; Shorten nyan cat bar length
(eval-after-load "nyan-mode" (setq-default nyan-bar-length 16))
(autoload 'nyan-create "nyan-mode") ; Nyan doesn't how to autoload :(

;; Enable nyan cat :3
(nyan-mode)

;; Customize Projectile mode line
(setq
 projectile-mode-line '(:propertize
                        (:eval
                         (if (string= (projectile-project-name) (getenv "USER"))
                             '(" ¬_¬")
                           (concat " " (projectile-project-name))))
                        face font-lock-constant-face))

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
                (projectile-mode projectile-mode-line)
                ;; Version control information
                (vc-mode mein-vc-mode-line)
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
                 ("%l" (column-number-mode ":%c")))))

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
