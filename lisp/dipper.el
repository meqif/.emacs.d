;;; dipper.el -- Display pending pull requests in a nicer way  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Ricardo Martins

;; Author: Ricardo Martins
;; Keywords:
;; Package-Requires: ((emacs "27.0") (ghub "3.5.1") (dash "2.17.0") (s "1.12.0") (ts "0.2"))

;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;; http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;;; Code:

;; Included with Emacs
(require 'map)

(require 'ts)
(require 'dash)
(require 'ghub)

(defvar dipper-github-username (ghub--username "api.github.com"))
(defvar dipper-subscribed-github-repositories nil)

(defun dipper--labels-to-tags (labels)
  (s-replace "::" ":" (s-join "" (--map (format ":%s:" (s-replace " " "_" it)) labels))))

(defun dipper--pr-needs-attention-p (pull-request reviews)
  (-let* ((updated-at (alist-get 'updated_at pull-request))
          (author (map-nested-elt pull-request '(user login)))
          (reviews-by-me (--filter (string-equal dipper-github-username (map-nested-elt it '(user login))) reviews))
          (latest-review-by-me (-last-item reviews-by-me)))
    ;; A pull request needs attention if:
    (or
     (and
      ;; is mine
      (string-equal author dipper-github-username)
      ;; has been reviewed
      (< 0 (seq-length reviews)))
     (and
      ;; isn't mine
      (not (string-equal author dipper-github-username))
      (or
       ;; it hasn't been reviewed
       (= 0 (seq-length reviews))
       ;; or it has been reviewed by me and has been updated since my last review
       (and latest-review-by-me
            ;; (not (string-equal "APPROVED" (alist-get 'state latest-review-by-me)))
            (string< (alist-get 'submitted_at latest-review-by-me) updated-at)))))))

(defun dipper--pr-reviewed-by-me-p (pull-request reviews)
  (-let* ((updated-at (alist-get 'updated_at pull-request))
          (author (map-nested-elt pull-request '(user login)))
          (reviews-by-me (--filter (string-equal dipper-github-username (map-nested-elt it '(user login))) reviews))
          (latest-review-by-me (-last-item reviews-by-me)))
    latest-review-by-me))

(defun dipper--pr-is-stale (pull-request)
  (-let ((updated-at (alist-get 'updated_at pull-request))
         (labels (--map (alist-get 'name it) (alist-get 'labels pull-request))))
    (> (ts-difference (ts-now) (ts-parse updated-at))
       (* 3600 24 7))))

(defun dipper--format-org-todo-state (pull-request reviews)
  (cond ((dipper--pr-needs-attention-p pull-request reviews) "TODO ")
        ((dipper--pr-reviewed-by-me-p pull-request reviews) "DONE ")
        (t "")))

(defun dipper--format-pull-request (pull-request)
  (-let* ((url (alist-get 'url pull-request))
          (reviews-url (s-concat (s-chop-prefix "https://api.github.com" url) "/reviews"))
          (reviews (ghub-get reviews-url))
          (reviewers (-uniq (-sort 'string< (--map (map-nested-elt it '(user login)) reviews))))
          (html-url (alist-get 'html_url pull-request))
          (title (alist-get 'title pull-request))
          (number (alist-get 'number pull-request))
          (created-at (alist-get 'created_at pull-request))
          (author (map-nested-elt pull-request '(user login)))
          (labels (--map (alist-get 'name it) (alist-get 'labels pull-request)))
          ;; Mark pull requests without activity for over a week as stale
          (labels (if (dipper--pr-is-stale pull-request) (cons "stale" labels) labels)))
    (format
     "** %s[[%s][%s (#%s)]] %s\n   :PROPERTIES:\n   :created-at: %s\n   :author: %s\n%s   :END:\n\n"
     (dipper--format-org-todo-state pull-request reviews)
     html-url
     title
     number
     (dipper--labels-to-tags labels)
     (ts-format (ts-parse created-at))
     author
     (if (not (eq nil reviewers)) (format "   :reviewed-by: %s\n" (s-join " " reviewers)) ""))))

(defun dipper--insert-org-preamble ()
  (insert "#+TITLE: Pending Pull Requests\n")
  (insert "#+STARTUP: content\n")
  (insert (format "#+UPDATED_AT: %s\n" (ts-format (ts-now))))
  (insert "\n"))

(defun dipper--process-repository (repository)
  ;; Insert repository header
  (insert (format "* %s\n" repository))
  ;; Insert pull requests
  (-let ((pull-requests (ghub-get (format "/repos/%s/pulls" repository))))
    (-map (lambda (pull-request)
            ;; Insert pull request header and details
            (insert (dipper--format-pull-request pull-request)))
          pull-requests)))

;;;###autoload
(defun display-pending-pull-requests ()
  "Display all pending pull requests for the chosen Github repositories."
  (interactive)
  (let ((buffer (get-buffer-create "*Pending pull requests*"))
        (repo-names dipper-subscribed-github-repositories))
    (with-current-buffer buffer
      (erase-buffer)
      (save-excursion
        (dipper--insert-org-preamble)
        (-map 'dipper--process-repository repo-names))
      (org-mode)
      (org-align-tags t)
      (switch-to-buffer buffer))))

(provide 'dipper)
;;; dipper.el ends here
