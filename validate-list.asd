;;;; validate-list.asd

(asdf:defsystem #:validate-list
  :description "Allows you to validate the contents and structure of a list based off of 
a template"
  :author "K1D77A"
  :license  "MIT"
  :version "0.0.1"
  :pathname "src"
  :serial t
  :depends-on (:arithmetic-operators-as-words
               :lisp-unit
               :alexandria)
  :components ((:file "package")
               (:file "conditions")
               (:file "validate-list")
               (:file "compile-template")
               (:file "tests")))
