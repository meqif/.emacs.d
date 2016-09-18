;; Appearance

;; Clean previous themes definitions when loading a theme
(defadvice load-theme
    (before theme-dont-propagate activate)
  (mapc #'disable-theme custom-enabled-themes))

;; Load theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

;; (progn
;;   ;; Custom tweaks to solarized-theme
;;   (setq
;;    ;; Make the mode-line high contrast
;;    solarized-high-contrast-mode-line t
;;    ;; Fix underscored mode-line
;;    x-underline-at-descent-line t
;;    ;; Don't use as much bold
;;    solarized-use-less-bold t
;;    ;; Disable variable pitch fonts in Solarized theme
;;    solarized-use-variable-pitch nil
;;    ;; Fix the fringe and linum
;;    solarized-distinct-fringe-background t)
;;   (load-theme 'solarized-light t)
;;   (set-face-attribute 'mode-line nil :box nil :overline nil :underline nil)
;;   (set-face-attribute 'mode-line-inactive nil :box nil :overline nil :underline nil))

;; (progn
;;   (load-theme 'eink t)
;;   (set-face-attribute 'font-lock-keyword-face nil :foreground "#ab4642")
;;   (set-face-attribute 'highlight nil :background "#fff1aa")
;;   ;;(set-face-attribute 'whitespace-line nil :background 'unspecified :foreground "magenta")
;;   (set-face-attribute 'font-lock-doc-face nil :foreground "#808080"))
;; (load-theme 'duotone-denim t)
;; (load-theme 'gruvbox t)
(load-theme 'cleanroom t)

;; Mac friendly font
(when window-system
  (setq meqif/default-font "-apple-Fira Mono-medium-normal-normal-*-14-*-*-*-m-0-iso10646-1"
        meqif/fullscreen-font "-apple-Fira Mono-medium-normal-normal-*-16-*-*-*-m-0-iso10646-1")
  (set-face-attribute 'default nil :font meqif/default-font)
  (set-frame-size (selected-frame) 120 38))

(defun gogo-fullscreen ()
  "Change to fullscreen mode.

Make frame fullscreen and change the default font unless it's
already in fullscreen"
  (interactive)
  ;; (if (memq (frame-parameter nil 'fullscreen) '(fullscreen fullboth))
  ;; (set-face-attribute 'default nil :font meqif/default-font)
  ;; (set-face-attribute 'default nil :font meqif/fullscreen-font))
  (toggle-frame-fullscreen))

;; Highlight current line
(global-hl-line-mode 1)

;; Customize mode-line project face
(copy-face 'font-lock-constant-face 'project-face)
(set-face-attribute 'project-face nil :foreground "#19D0FF")
(set-face-attribute 'project-face nil :foreground "#086199")

;; Customize mode-line project branch face
(copy-face 'project-face 'branch-face)
(set-face-attribute 'branch-face nil :bold nil)

;; Customize Projectile mode line
(setq projectile-mode-line
 '(:propertize
   (:eval
    (if (string= (projectile-project-name) (getenv "USER"))
        '(" ")
      (concat " " (projectile-project-name))))
   face project-face))

(defvar meqif-vc-mode-line
  '(" " (:propertize
         ;; Strip the backend name from the VC status information
         (:eval (let ((backend (symbol-name (vc-backend (buffer-file-name)))))
                  (substring vc-mode (+ (length backend) 2))))
         face branch-face))
  "Mode line format for VC Mode.")
(put 'meqif-vc-mode-line 'risky-local-variable t)

(use-package spaceline-config
  :ensure spaceline
  :config
  (setq ns-use-srgb-colorspace nil)
  (spaceline-spacemacs-theme)
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state
        spaceline-minor-modes-separator " · "
        powerline-default-separator 'slant)
  (spaceline-toggle-buffer-position-off)
  (spaceline-toggle-hud-off)
  (set-face-attribute 'powerline-active1 nil :foreground "gray99"))

;; Tweak display-time mode-line format
(setq display-time-24hr-format t)
(setq display-time-default-load-average nil)

;; Enable prettify symbols mode
(global-prettify-symbols-mode +1)

(defun font-lock-comment-annotations ()
  "Highlight a bunch of well known comment annotations."
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\(ME\\)?\\|TODO\\):"
          1 font-lock-warning-face t))))
(add-hook 'prog-mode-hook 'font-lock-comment-annotations)

(provide 'appearance)
