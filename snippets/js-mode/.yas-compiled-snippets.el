;;; Compiled snippets and support files for `js-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'js-mode
                     '(("req" "var ${3:${1:$(to-camel-case (file-name-nondirectory yas/text))}} = require('${1:fs}')$2;$0" "node.require" nil nil nil "/Users/meqif/.emacs.d/snippets/js-mode/node.require.yasnippet" nil nil)
                       ("it" "it('$1', function(${2:done}) {\n    $0\n});" "mocha-it" nil nil nil "/Users/meqif/.emacs.d/snippets/js-mode/mocha-it.yasnippet" nil nil)
                       ("desc" "describe('$1', function() {\n    $0\n});" "mocha-describe" nil nil nil "/Users/meqif/.emacs.d/snippets/js-mode/mocha-describe.yasnippet" nil nil)
                       ("fn" "function($1) {$0}" "function" nil nil nil "/Users/meqif/.emacs.d/snippets/js-mode/function.yasnippet" nil nil)))


;;; Do not edit! File generated at Mon Sep  5 19:09:27 2016
