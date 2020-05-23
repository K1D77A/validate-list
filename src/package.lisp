;;;; package.lisp

(defpackage #:validate-list
  (:use #:cl)
  (:shadowing-import-from
   #:arithmetic-operators-as-words
   #:less-than
   #:greater-than-or-equal
   #:greater-than))
