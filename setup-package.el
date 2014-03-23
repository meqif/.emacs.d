(require 'package)

;; Add melpa to package repositories
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)

(unless (fboundp 'package-cleanup)
  (require 'cl)

  (defun package-version-for (package)
    (package-desc-vers (cdr (assoc package package-alist))))

  (defun package-delete-by-name (package)
    (package-delete (symbol-name package)
                    (package-version-join (package-version-for package))))

  (defun package-maybe-install (name)
    (or (package-installed-p name) (package-install name)))

  (defun package-cleanup (packages)
    "Remove packages not explicitly declared"
    (let ((removes (set-difference (mapcar 'car package-alist) packages)))
      (mapc 'package-delete-by-name removes))))

(provide 'setup-package)
