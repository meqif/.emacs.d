;;; color-theme-quiet-light.el --- Emacs port of the Quiet Light foam for Espresso

;; Copyright (C) 2017 Ricardo Martins

;; Author: Ricardo Martins
;; URL: http://github.com/meqif/quiet-light-emacs
;; Version: 1.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A port of the Espresso foam Quiet Light for Emacs 24, built on top
;; of the new built-in theme support in Emacs 24, and based on Martin Kühl's
;; original port.
;;
;; Based on the Quiet Light foam for Espresso, by Ian Beck
;; Source: <http://github.com/onecrayon/quiet-light.foam>
;; Initially ported to Emacs by Martin Kühl <purl.org/net/mkhl>
;; Updated to use Emacs 24+'s deftheme by Ricardo Martins

;;; Code:

(deftheme quiet-light "The Quiet Light color theme")

;;; Color Palette

(defvar quiet-light-colors-alist
  '(
    ;; ("background-mode" . "light")
    ("background-color" . "#F5F5F5")
    ("border-color" . "black")
    ("cursor-color" . "black")
    ("foreground-color" . "#333333")
    ("selection-color" . (if (featurep 'ns) "ns_selection_color" "#C9D0D9"))
    ("highlight-color" . "#EEE00A")
    ("secondary-color" . "#D3E1CD")
    ("active-color" . "#FFFEDB")
    ("passive-color" . "#CCCCCC")
    ("subtle-color" . "#E8E8E8")
    ("error-color" . "#EEE3E3"))
  "List of Quiet Light colors.
Each element has the form (NAME . HEX).")

(defmacro quiet-light-with-color-variables (&rest body)
  "`let' bind all colors defined in `quiet-light-colors-alist' around BODY.
Also bind `class' to ((class color) (min-colors 89))."
  (declare (indent 0))
  `(let ((class '((class color) (min-colors 89)))
         ,@(mapcar (lambda (cons)
                     (list (intern (car cons)) (cdr cons)))
                   quiet-light-colors-alist))
     ,@body))

;;; Theme Faces
(quiet-light-with-color-variables
  (custom-theme-set-faces
   'quiet-light
   ;; Basics
   `(default ((t (:foreground ,foreground-color :background ,background-color))))
   `(blue ((t (:foreground "blue"))))
   `(bold ((t (:bold t))))
   `(bold-italic ((t (:italic t :bold t))))
   `(border-glyph ((t (nil))))
   `(green ((t (:foreground "green"))))
   `(info-node ((t (:italic t :bold t))))
   `(info-xref ((t (:bold t))))
   `(italic ((t (:italic t))))
   `(left-margin ((t (nil))))
   `(pointer ((t (nil))))
   `(red ((t (:foreground "red"))))
   `(right-margin ((t (nil))))
   `(underline ((t (:underline t))))
   `(yellow ((t (:foreground "yellow"))))

   ;; Parens
   `(show-paren-match ((t (:background ,passive-color))))
   `(show-paren-mismatch ((t (:foreground ,error-color :background "#660000"))))

   ;; Highlighting
   `(hl-line ((t (:background ,active-color))))
   `(highlight ((t (:background ,highlight-color))))
   `(highlight-symbol-face ((t (:background ,secondary-color))))
   `(isearch ((t (:background ,highlight-color))))
   `(lazy-highlight ((t (:background ,secondary-color))))
   `(primary-selection ((t (:background ,selection-color))))
   `(region ((t (:background ,selection-color))))
   `(secondary-selection ((t (:background ,secondary-color))))
   `(shadow ((t (:foreground "grey50" :background ,subtle-color))))
   `(text-cursor ((t (:background "black" :foreground ,passive-color))))
   `(zmacs-region ((t (:background ,selection-color))))

   ;; More
   `(mumamo-background-chunk-submode ((t (:background "#EAEBE6"))))

   ;; Font-lock
   `(font-lock-builtin-face ((t (:foreground "#91B3E0"))))
   `(font-lock-comment-face ((t (:italic t :foreground "#AAAAAA"))))
   `(font-lock-comment-delimiter-face ((t (:italic nil :foreground "#AAAAAA"))))
   `(font-lock-constant-face ((t (:foreground "#AB6526"))))
   `(font-lock-doc-string-face ((t (:foreground "#448C27"))))
   `(font-lock-function-name-face ((t (:foreground "#AA3731"))))
   `(font-lock-keyword-face ((t (:foreground "#4B83CD"))))
   `(font-lock-preprocessor-face ((t (:foreground "#434343"))))
   `(font-lock-reference-face ((t (:foreground "#AB6526"))))
   `(font-lock-string-face ((t (:foreground "#448C27"))))
   `(font-lock-type-face ((t (:foreground "#7A3E9D"))))
   `(font-lock-variable-name-face ((t (:foreground "#7A3E9D"))))
   `(font-lock-warning-face ((t (:foreground "#660000" :background "#EEE3E3"))))

   ;; Diff Mode
   `(diff-file-header ((t (:bold t :inherit diff-header))))
   `(diff-header ((t (:background "#DDDDFF" :foreground "#434343"))))
   `(diff-added ((t (:background "#DDFFDD"))))
   `(diff-removed ((t (:background "#FFDDDD"))))
   `(diff-changed ((t (:background "#FFFFDD"))))
   `(diff-refine-change ((t (:background "#DDDDFF"))))
   ;; Magit
   `(magit-diff-file-header ((t (:bold t :inherit diff-header))))
   `(magit-diff-hunk-header ((t (:inherit diff-header))))
   `(magit-diff-add ((t (:inherit diff-added :foreground "#434343"))))
   `(magit-diff-del ((t (:inherit diff-removed :foreground "#434343"))))
   `(magit-diff-none ((t (:inherit diff-context :foreground "#434343"))))
   `(magit-item-highlight ((t (:background nil :foreground "#232323"))))
   ))

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide 'quiet-light)

;;; quiet-light-theme.el ends here
