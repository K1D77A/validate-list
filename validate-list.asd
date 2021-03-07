;;;; validate-list.asd

(asdf:defsystem #:validate-list
  :description "Allows you to validate the contents and structure of a list based off of 
a template"
  :author "K1D77A"
  :license  "MIT"
  :version "1.0.1"
  :in-order-to ((asdf:test-op (asdf:load-op "validate-list/tests")))
  :perform (asdf:test-op (o c)
                         (uiop:symbol-call
                          "LISP-UNIT" "RUN-TESTS" :all "VALIDATE-LIST/TESTS"))
  :depends-on (:arithmetic-operators-as-words
               :lisp-unit
               :alexandria)
  :components ((:module "src"
                :serial t
                :components
                ((:file "package")
                 (:file "conditions")
                 (:file "validate-list")
                 (:file "compile-template")))))

(asdf:defsystem #:validate-list/tests
  :description "Tests for validate-list"
  :author "K1D77A"
  :license "MIT"
  :version "1.0.0"
  :serial t
  :depends-on (:validate-list
               :lisp-unit)
  :components ((:module "tests"
                :serial t
                :components
                ((:file "package")
                 (:file "tests")))))
