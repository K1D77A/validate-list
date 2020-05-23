(in-package #:validate-list)
;;;these should go in another file
(lisp-unit:define-test test-validation
  (lisp-unit:assert-true (validate-list-p *test-list1* *test-template1*))
  (lisp-unit:assert-true (validate-list-p *test-list2* *test-template2*))
  (lisp-unit:assert-true (validate-list-p *test-list3* *test-template3*))
  (lisp-unit:assert-true (validate-list-p *test-list4* *test-template4*))
  (lisp-unit:assert-true (validate-list-p *test-list5* *test-template5*))
  (lisp-unit:assert-true (validate-list-p *test-list7* *test-template7*))
  (lisp-unit:assert-true (validate-list-p *test-list8* *test-template8*))
  (lisp-unit:assert-false (validate-list-p *test-list6* *test-template6*))
  (lisp-unit:assert-false (validate-list-p *test-list1* *test-template2*))
  (lisp-unit:assert-false (validate-list-p *test-list2* *test-template1*))
  (list-unit:assert-false (validate-list-p *test-list9* *test-template9*)))
