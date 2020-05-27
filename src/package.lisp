;;;; package.lisp

(defpackage #:validate-list
  (:use #:cl)
  (:export #:validate-list
           #:same-structures-p
           #:define-key
           #:repeat-pattern
           #:current-keys
           #:repeat-test
           #:unknown-keyword
           #:unknown-keyword-message
           #:unknown-keyword-keyword
           #:compile-template
           #:is-valid-template
           #:bad-template-format
           #:bad-template-format-template
           #:bad-template-format-signaller
           #:bad-template-format-message
           #:failed-to-validate
           #:failed-to-validate-key
           #:failed-to-validate-arg
           #:failed-to-validate-entry
           #:failed-to-validate-message)
  (:shadowing-import-from
   #:arithmetic-operators-as-words
   #:less-than
   #:greater-than-or-equal
   #:greater-than))
