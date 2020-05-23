;;;; package.lisp

(defpackage #:validate-list
  (:use #:cl)
  (:export #:validate-list-p
           #:add-new-symbol)
  (:shadowing-import-from
   #:arithmetic-operators-as-words
   #:less-than
   #:greater-than-or-equal
   #:greater-than))
