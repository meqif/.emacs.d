;;; zettelkasten.el --- Zettelkasten-related functions and variables  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Ricardo Martins

;; Author: Ricardo Martins <ricardo@scarybox.net>
;; Keywords: outlines, convenience
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1") (markdown-mode "2.4") (f "0.20.0") (s "1.12.0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; An Emacs mode for Zettelkasten-style note-taking.

;;; Code:

(require 'f)
(require 's)

(defvar zettelkasten-directory "~/.zettelkasten")
(defvar zettelkasten-extension "md")
(defvar zettelkasten-link-prefix "§")
(defvar zettelkasten-link-suffix "")
(defvar zettelkasten-identifier-format "[0-9][0-9][0-9][0-9]-[012][0-9]-[0123][0-9]-[012][0-9]-[0-5][0-9]")
(defvar zettelkasten-link-format
  (format "%s\\(%s\\)%s" zettelkasten-link-prefix zettelkasten-identifier-format zettelkasten-link-suffix))
(defvar zettelkasten-filename-format
  (format "^\\(%s\\) .+\.md" zettelkasten-identifier-format))

;;;###autoload
(defun zettelkasten-follow-link ()
  (interactive)
  (-when-let (identifier (substring (save-match-data
                                      (thing-at-point-looking-at zettelkasten-link-format)
                                      (match-string-no-properties 0))
                                    1 -1))
    (find-file
     (s-trim-right
      (shell-command-to-string
       (concat "fd --extension " zettelkasten-extension " " identifier " " zettelkasten-directory))))))

(defun zettelkasten-find-tag (tag)
  "Find files matching TAG."
  (-remove #'s-blank?
           (s-split "\n"
                    (shell-command-to-string
                     (format "zettelkasten-searcher find-tag '%s'" tag)))))

(defun zettelkasten-list-tags (&rest _rest)
  "List all known tags."
  (s-split "\n"
           (shell-command-to-string "zettelkasten-searcher list-tags")))

(defun counsel-zettelkasten-tag--files-matching-tag (tag)
  "Find Zettelkasten notes containing TAG."
  (ivy-read "Find note: "
            (zettelkasten-find-tag tag)
            :action #'(lambda (id) (find-file (f-join zettelkasten-directory id)))))

;;;###autoload
(defun counsel-zettelkasten-find-by-tag ()
  "Find Zettelkasten notes by tag."
  (interactive)
  (ivy-read "Find note by tag: "
            #'zettelkasten-list-tags
            :dynamic-collection nil
            :action #'counsel-zettelkasten-tag--files-matching-tag))

;;;###autoload
(defun counsel-zettelkasten-find ()
  "Find Zettelkasten notes by searching the title, tags and body for keywords."
  (interactive)
  (ivy-read "Find note: "
            #'(lambda (needle)
                (counsel--async-command (format "zettelkasten-searcher find %s" (shell-quote-argument needle)))
                nil)
            :dynamic-collection t
            :unwind (lambda ()
                      (counsel-delete-process)
                      (swiper--cleanup))
            :action #'(lambda (id) (find-file (f-join zettelkasten-directory id)))))

(defun zettelkasten--parse-result (json)
  (--map
   (propertize (alist-get 'title it)
               'filename (alist-get 'filename it)
               'tags (alist-get 'tags it))
   json))

(defun zettelkasten--list-all (&rest _ignored)
  "List all Zettelkasten notes with some structure."
  (zettelkasten--parse-result
   (json-read-from-string (shell-command-to-string "zettelkasten-searcher list-files"))))

;;;###autoload
(defun counsel-zettelkasten-open ()
  "List all Zettelkasten notes by title."
  (interactive)
  (ivy-read "Find note: "
            #'zettelkasten--list-all
            :action #'(lambda (note) (find-file (f-join zettelkasten-directory (get-text-property 0 'filename note))))
            :caller #'counsel-zettelkasten-open))

(defun zettelkasten--ivy-display-transformer (input)
  "Transforms a propertized zettelkasten-searcher result into a string with the note's title and tags."
  (concat (s-pad-right 80 " " input)
          (propertize (s-join ", " (get-text-property 0 'tags input)) 'face 'ivy-virtual)))

(ivy-set-display-transformer
 'counsel-zettelkasten-open
 'zettelkasten--ivy-display-transformer)

(defun zettelkasten--find-backreferences ()
  "Find backreferences to the current Zettelkasten note."
  (-when-let (current-id (cadr (s-match zettelkasten-filename-format (buffer-name))))
    (s-split
     "\n"
     (shell-command-to-string
      (format "rg --files-with-matches §%s %s" current-id zettelkasten-directory)) t)))

;;;###autoload
(defun counsel-zettelkasten-backreferences ()
  (interactive)
  (ivy-read "Backreferences: " (zettelkasten--find-backreferences)
            :action #'(lambda (id) (find-file (f-join zettelkasten-directory id)))))

;;;###autoload
(defun zettelkasten-create-note ()
  (interactive)
  (-when-let (note-title (ivy-read "Insert note title: " nil))
    (find-file
     (f-join zettelkasten-directory
             (format "%s %s.%s" (zettelkasten--generate-id) (downcase note-title) zettelkasten-extension)))
    (insert (format "---\ntitle: %s\ntags:\n---\n\n" note-title))))

(defun zettelkasten--generate-id ()
  (format-time-string "%Y-%m-%d-%H-%M"))

(defun zettelkasten-xref-backend ()
  "Zettelkasten backend for `xref'."
  'zettelkasten-xref)

(defun zettelkasten--get-title (filename)
  "Returns the title of the note stored at FILENAME."
  (shell-command-to-string (format "rg 'title: (.+)' -r '$1' '%s'" filename)))

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql zettelkasten-xref)))
  (save-match-data
    (-if-let* ((_ (thing-at-point-looking-at zettelkasten-link-format))
               (identifier (match-string-no-properties 0)))
        (s-chop-prefix "§" identifier)
      (cadr (s-match zettelkasten-filename-format (buffer-name))))))

(cl-defmethod xref-backend-definitions ((_backend (eql zettelkasten-xref)) identifier)
  (-when-let (filename (s-trim-right
                        (shell-command-to-string
                         (concat "fd --extension " zettelkasten-extension " " identifier " " zettelkasten-directory))))
    (list
     (xref-make
      (zettelkasten--get-title filename)
      (xref-make-file-location filename 0 0)))))

(cl-defmethod xref-backend-references ((_backend (eql zettelkasten-xref)) identifier)
  (zettelkasten--xref-backend-references identifier))

(defun zettelkasten--xref-backend-references (identifier)
  (--map
   (xref-make
    (zettelkasten--get-title it)
    (xref-make-file-location it 0 0))
   (s-split
    "\n"
    (shell-command-to-string
     (format "rg --files-with-matches §%s %s" identifier zettelkasten-directory)) t)))

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql zettelkasten-xref)))
  nil)

;;;###autoload
(define-derived-mode zettelkasten-mode gfm-mode "🆉"
  "Major mode for Zettelkasten notes."
  (progn
    (add-hook 'xref-backend-functions 'zettelkasten-xref-backend nil t)
    (font-lock-add-keywords 'zettelkasten-mode `((,zettelkasten-link-format . 'font-lock-warning-face)))))

(add-to-list 'auto-mode-alist
             (cons (format "^%s" (f-expand zettelkasten-directory))
                   'zettelkasten-mode))

(provide 'zettelkasten)
;;; zettelkasten.el ends here
