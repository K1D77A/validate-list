(in-package #:validate-list)
;;;these should go in another file

(defparameter *test-list1* '("key" "abcdeegadfgfsdf"))
(defparameter *test-template1* '((:equal "key") (:type string :maxlen 40)))

(defparameter *test-list2*  '("year" 2020 ("country" "USA")))
(defparameter *test-template2* '((:equal "year")(:type integer :between (2100 1900))
                                 ((:or ("cookie" "country"))(:type string :maxlen 50))))

(defparameter *test-list3*  '("year" 98 ("country" ("USA" "UK" "Poland"))))
(defparameter *test-template3* '((:equal "year")(:type integer :or (96 97 98))
                                 ((:or ("cookie" "country"))
                                  ((:equal "USA")(:equal "UK")(:equal "Poland")))))

(defparameter *test-list4*  '("year" 98 ("country" ("USA" "UK" "Poland"))))
(defparameter *test-template4* `((:equal "year")(:type integer :or (96 97 98))
                                 ((:or ("cookie" "country"))
                                  ,(repeat-test 3 '(:type string :maxlen 6 :minlen 2)))))

(defparameter *test-list5*  '("year" 98 ("keyvals" ("USA" 35 "Poland" 55 "UK" 96))))
(defparameter *test-template5* `((:equal "year")(:type integer :or (96 97 98))
                                 ((:or ("cookie" "country" "keyvals"))
                                  ,(repeat-pattern 3 '((:type string :maxlen 6 :minlen 2)
                                                       (:type number :between (0 100)))))))

(defparameter *test-list6*  '("year" 98 ("keyvals" ("USA" 35 "Poland" 55 "UK" 96))))
(defparameter *test-template6* `((:equal "year")(:type integer :or (96 97 98))
                                 ((:or ("cookie" "country" "keyvals"))
                                  ,(repeat-pattern 4 '((:type string :maxlen 6 :minlen 2)
                                                       (:type number :between (0 100)))))))
;;;^should fail because the end is len 4 instead of len 3
(defparameter *test-list7*  '("year" 98 ("keyvals" ("USA" 35 "Poland" 55 "UK" 96) 2 4 6)))
(defparameter *test-template7* `((:equal "year")(:type integer :or (96 97 98))
                                 ((:or ("cookie" "country" "keyvals"))
                                  ,(repeat-pattern 3 '((:type string :maxlen 6 :minlen 2)
                                                       (:type number :between (0 100)))))
                                 ,(repeat-test 3 '(:type number :satisfies #'evenp))))

(defparameter *test-list8*  '("year" 98 ("keyvals" ("USA" 35 "Poland" 55 "UK" 96) 2 5 6)))
(defparameter *test-template8* `((:equal "year")(:type integer :or (96 97 98))
                                 ((:or ("cookie" "country" "keyvals"))
                                  ,(repeat-pattern 3 '((:type string :maxlen 6 :minlen 2)
                                                       (:type number :between (0 100)))))
                                 ,(repeat-test 3 '(:type number :satisfies (#'evenp #'oddp)))))

(defparameter *test-list9*  '("year" 98 ("keyvals" ("USA" 35 "Poland" 55 "UK" 96) 2 5 6)))
(defparameter *test-template9* `((:equal "year")(:type integer :or (96 97 98))
                                 ((:or ("cookie" "country"))
                                  ,(repeat-pattern 3 '((:type string :maxlen 6 :minlen 2)
                                                       (:type number :between (0 100)))))
                                 ,(repeat-test 3 '(:type number :satisfies (#'evenp #'oddp)))))

(defparameter *test-list10*  '("year" 98 ("keyvals" ("USA" 35 "Poland" 55 "UK" 96) 2 5 6)))
(defparameter *test-template10* `((:equal "year")(:type integer :or (96 97 98))
                                  ((:or ) ;;broken intentionally
                                   ,(repeat-pattern 3 '((:type string :maxlen 6 :minlen 2)
                                                        (:type number :between (0 100)))))
                                  ,(repeat-test 3 '(:type number :satisfies (#'evenp #'oddp)))))

(defparameter *test-list11*  '("year" 98 ("keyvals" ("USA" 35 "Poland" 55 "UK" 96) 2 5 6)))
(defparameter *test-template11* `((:equal "year")(:type integer :or (96 97 98))
                                  ((:or 'abc 'def 'hhh) ;;broken intentionally
                                   ,(repeat-pattern 3 '((:type string :maxlen 6 :minlen 2)
                                                        (:type number :between (0 100)))))
                                  ,(repeat-test 3 '(:type number :satisfies (#'evenp #'oddp)))))


(defun compile-template-and-test (list template)
  (check-type template list)
  (check-type list list)
  (let ((compiled (compile-template template)))
    (check-type compiled function)
    (funcall compiled list)))

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
  (lisp-unit:assert-false (validate-list-p *test-list9* *test-template9*))
  (lisp-unit:assert-error 'bad-template-format (validate-list-p *test-list10* *test-template10*))
  (lisp-unit:assert-error 'bad-template-format (validate-list-p *test-list11* *test-template11*)))

(lisp-unit:define-test test-compiled
  (lisp-unit:assert-true (compile-template-and-test *test-list1* *test-template1*))
  (lisp-unit:assert-true (compile-template-and-test *test-list2* *test-template2*))
  (lisp-unit:assert-true (compile-template-and-test *test-list3* *test-template3*))
  (lisp-unit:assert-true (compile-template-and-test *test-list4* *test-template4*))
  (lisp-unit:assert-true (compile-template-and-test *test-list5* *test-template5*))
  (lisp-unit:assert-true (compile-template-and-test *test-list7* *test-template7*))
  (lisp-unit:assert-true (compile-template-and-test *test-list8* *test-template8*))
  (lisp-unit:assert-false (compile-template-and-test *test-list6* *test-template6*))
  (lisp-unit:assert-false (compile-template-and-test *test-list1* *test-template2*))
  (lisp-unit:assert-false (compile-template-and-test *test-list2* *test-template1*))
  (lisp-unit:assert-false (compile-template-and-test *test-list9* *test-template9*))
  (lisp-unit:assert-error 'bad-template-format
                          (compile-template-and-test *test-list10* *test-template10*))
  (lisp-unit:assert-error 'bad-template-format
                          (compile-template-and-test *test-list11* *test-template11*)))
  


