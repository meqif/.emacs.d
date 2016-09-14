;;; Compiled snippets and support files for `org-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'org-mode
                     '(("title" "#+TITLE: $0\n" "title" nil nil nil "/Users/meqif/.emacs.d/snippets/org-mode/title" nil nil)
                       ("SRC" "#+BEGIN_SRC $1\n$0\n#+END_SRC" "SRC" nil nil nil "/Users/meqif/.emacs.d/snippets/org-mode/src" "direct-keybinding" nil)
                       ("QUOTE" "#+BEGIN_QUOTE\n$1\n#+END_QUOTE\n\n$0" "quote" nil nil nil "/Users/meqif/.emacs.d/snippets/org-mode/quote" "direct-keybinding" nil)
                       ("beg" "#+BEGIN_${1:src$(upcase yas-text)} $2\n$0\n#+END_$1" "#+BEGIN_XXX" nil nil nil "/Users/meqif/.emacs.d/snippets/org-mode/begin" "direct-keybinding" nil)))


;;; Do not edit! File generated at Mon Sep  5 19:09:27 2016
