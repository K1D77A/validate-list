;;;; package.lisp

(defpackage #:validate-list
  (:use #:cl)
  (:export #:validate-list-p
           #:add-new-symbol
           #:repeat-pattern
           #:repeat-test
           #:unknown-keyword
           #:unknown-keyword-message
           #:unknown-keyword-keyword)
  (:shadowing-import-from
   #:arithmetic-operators-as-words
   #:less-than
   #:greater-than-or-equal
   #:greater-than))
