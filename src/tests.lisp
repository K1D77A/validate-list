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
                                 ,(repeat-test 3 '(:type number :satisfies evenp))))

(defparameter *test-list8*  '("year" 98 ("keyvals" ("USA" 35 "Poland" 55 "UK" 96) 2 5 6)))
(defparameter *test-template8* `((:equal "year")(:type integer :or (96 97 98))
                                 ((:or ("cookie" "country" "keyvals"))
                                  ,(repeat-pattern 3 '((:type string :maxlen 6 :minlen 2)
                                                       (:type number :between (0 100))))
                                  ,@(repeat-test 3 '(:type number :satisfies (evenp oddp))))))

(defparameter *test-list9*  '("year" 98 ("keyvals" ("USA" 35 "Poland" 55 "UK" 96) 2 5 6)))
(defparameter *test-template9* `((:equal "year")(:type integer :or (96 97 98))
                                 ((:or ("cookie" "country"))
                                  ,(repeat-pattern 3 '((:type string :maxlen 6 :minlen 2)
                                                       (:type number :between (0 100)))))
                                 ,(repeat-test 3 '(:type number :satisfies (evenp oddp)))))

(defparameter *test-list10*  '("year" 98 ("keyvals" ("USA" 35 "Poland" 55 "UK" 96) 2 5 6)))
(defparameter *test-template10* `((:equal "year")(:type integer :or (96 97 98))
                                  ((:or ) ;;broken intentionally
                                   ,(repeat-pattern 3 '((:type string :maxlen 6 :minlen 2)
                                                        (:type number :between (0 100)))))
                                  ,(repeat-test 3 '(:type number :satisfies (evenp oddp)))))

(defparameter *test-list11*  '("year" 98 ("keyvals" ("USA" 35 "Poland" 55 "UK" 96) 2 5 6)))
(defparameter *test-template11* `((:equal "year")(:type integer :or (96 97 98))
                                  ((:or 'abc 'def 'hhh) ;;broken intentionally
                                   ,(repeat-pattern 3 '((:type string :maxlen 6 :minlen 2)
                                                        (:type number :between (0 100)))))
                                  ,(repeat-test 3 '(:type number :satisfies (evenp oddp)))))

(defparameter *test-list12*  '("year" 98 ("keyvals" ("USA" 35 "Poland" 55 "UK" 96) 2 5 (6 7 8))))
(defparameter *test-template12* `((:equal "year")(:type integer :or (96 97 98))
                                  ((:or "keyvals" "time") ;;broken intentionally
                                   ,(repeat-pattern 3 '((:type string :maxlen 6 :minlen 2)
                                                        (:type number :between (0 100)))))
                                  ,(repeat-test 2 '(:type number :satisfies (evenp oddp)))
                                  (:type list :minlen 1 :maxlen 3)))

(defparameter *test-list12a*  '("year" 98 ("keyvals" ("USA" 35 "Poland" 55 "UK" 96)
                                           1 2 (6 7) (8) "abc")))
(defparameter *test-template12a* `((:equal "year")(:type integer :or (96 97 98))
                                   ((:or ("keyvals" "time")) ;;broken intentionally
                                    ,(repeat-pattern 3 '((:type string :maxlen 6 :minlen 2)
                                                         (:type number :between (0 100))))
                                    ,@(repeat-test 2 '(:type number :satisfies (evenp oddp)))
                                    (:type list :length 2)
                                    (:type list :minlen 1)
                                    (:type string :equal "abc"))))

(defparameter *test-list13*  '("year" 98 ("keyvals" ("USA" 35 "Poland" 55 "UK" 96)
                                          (2) (6 7) ("oof" "boof" "kadoof") "abc")))
(defparameter *test-template13* `((:equal "year")(:type integer :or (96 97 98))
                                  ((:or ("keyvals" "time")) ;;broken intentionally
                                   ,(repeat-pattern 3 '((:type string :maxlen 6 :minlen 2)
                                                        (:type number :between (0 100))))
                                   (:type list :minlen 1)
                                   (:type list :maxlen 2)
                                   (:type list :minlen 1 :maxlen 5 :contents (:type string))
                                   (:type string :equal "abc"))))

(defparameter *test-list14*  '("year" 98 ("keyvals" ("USA" 35 "Poland" 55 "UK" 96)
                                          1 2 (2) (6 7) ("oof" 23 "kadoof") "abc")))

(defparameter *test-template14* `((:equal "year")(:type integer :or (96 97 98))
                                  ((:or ("keyvals" "time")) ;;broken intentionally
                                   ;;this is an example of messing up ,@
                                   ,(repeat-pattern 3 '((:type string :maxlen 6 :minlen 2)
                                                        (:type number :between (0 100))))
                                   ;;fixing this template would be easy with ,@ here instead of ,
                                   ;;this is because @, will generate two lists, while , is
                                   ;;a list with two lists inside, this has a big effect on the
                                   ;;validation
                                   ,(repeat-test 2 '(:type number :satisfies (evenp oddp)))
                                   (:type list :length 1)
                                   (:type list :length 2)
                                   (:type list :minlen 1 :maxlen 5 :contents (:type string))
                                   (:type string :equal "abc"))))

(defparameter *test-list15*  '("year" 98 ("keyvals" ("USA" 35 "Poland" 55 "UK" 96)
                                          (2) (6 7) ("oof" 23 "kadoof") "abc")))

(defparameter *test-template15* `((:equal "year")(:type integer :or (96 97 98))
                                  ((:or ("keyvals" "time")) 
                                   ,(repeat-pattern 3
                                                    '((:type string :maxlen 6 :minlen 2)
                                                      (:type number :between (0 100))))
                                   (:type list :length 1 :contents (:type number))
                                   (:type list :length 2)
                                   (:type list :minlen 1 :maxlen 5 :contents (:type (string number)))
                                   (:type string :equal "abc"))))

(defparameter *test-list16*  '("year" 98 ("keyvals" ("USA" 35 "Poland" 55 "UK" 96)
                                          1 2 (2) (6 7) ("oof" "oof" "oof") "abc")))
(defparameter *test-template16* `((:equal "year")(:type integer :or (96 97 98))
                                  ((:or ("keyvals" "time")) 
                                   ,(repeat-pattern 3 '((:type string :maxlen 6 :minlen 2)
                                                        (:type number :between (0 100))))
                                   ,@(repeat-test 2 '(:type number :satisfies (evenp oddp)))
                                   (:type list :length 1)
                                   (:type list :contents (:type number :satisfies (evenp oddp)))
                                   (:type list :minlen 1 :maxlen 5
                                    :contents (:type string :maxlen 5 :equal "oof"))
                                   (:type string :equal "abc"))))

;;;broken structures
(defparameter *broken-struct-list1*  '("year" 98 ("keyvals" ("USA" 35 "Poland" 55 "UK" 96) 2 5 6)))
(defparameter *broken-struct1* `((:equal "year")(:type integer :or (96 97 98))
                                 ((:or ("cookie" "country" "keyvals"))
                                  ,(repeat-pattern 3 '((:type string :maxlen 6 :minlen 2)
                                                       (:type number :between (0 100))))
                                  ,(repeat-test 3 '(:type number :satisfies (evenp oddp))))))


;;because its of type list we don't want to recurse over it...

(defun compile-template-and-test (list template)
  (check-type template list)
  (check-type list list)
  (let ((compiled (compile-template template)))
    (check-type compiled function)
    (funcall compiled list)))

(lisp-unit:define-test test-validation-true
  (lisp-unit:assert-true (validate-list-p *test-list1* *test-template1*))
  (lisp-unit:assert-true (validate-list-p *test-list2* *test-template2*))
  (lisp-unit:assert-true (validate-list-p *test-list3* *test-template3*))
  (lisp-unit:assert-true (validate-list-p *test-list4* *test-template4*))
  (lisp-unit:assert-true (validate-list-p *test-list5* *test-template5*))
  (lisp-unit:assert-true (validate-list-p *test-list7* *test-template7*))
  (lisp-unit:assert-true (validate-list-p *test-list8* *test-template8*))
  (lisp-unit:assert-true (validate-list-p *test-list12a* *test-template12a*))
  (lisp-unit:assert-true (validate-list-p *test-list15* *test-template15*))
  (lisp-unit:assert-true (validate-list-p *test-list13* *test-template13*))
  (lisp-unit:assert-true (validate-list-p *test-list16* *test-template16*)))

(lisp-unit:define-test test-validation-false
  (lisp-unit:assert-error 'failed-to-validate (validate-list-p *test-list1* *test-template2*))
  ;;^ fails to validate before the bad structure is noticed
  (lisp-unit:assert-error 'failed-to-validate (validate-list-p *test-list2* *test-template1*))
  ;;^ fails to validate before the bad structure is noticed
  (lisp-unit:assert-error 'failed-to-validate (validate-list-p *test-list9* *test-template9*))
  
  (lisp-unit:assert-error 'bad-template-format (validate-list-p *test-list14* *test-template14*)))

(lisp-unit:define-test test-validation-error
  (lisp-unit:assert-error 'bad-template-format (validate-list-p *test-list6* *test-template6*))
  (lisp-unit:assert-error 'bad-template-format (validate-list-p *test-list12* *test-template12*))
  (lisp-unit:assert-error 'bad-template-format (validate-list-p *test-list10* *test-template10*))
  (lisp-unit:assert-error 'bad-template-format (validate-list-p *test-list11* *test-template11*)))

(lisp-unit:define-test test-compiled-true
  (lisp-unit:assert-true (compile-template-and-test *test-list1* *test-template1*))
  (lisp-unit:assert-true (compile-template-and-test *test-list2* *test-template2*))
  (lisp-unit:assert-true (compile-template-and-test *test-list3* *test-template3*))
  (lisp-unit:assert-true (compile-template-and-test *test-list4* *test-template4*))
  (lisp-unit:assert-true (compile-template-and-test *test-list5* *test-template5*))
  (lisp-unit:assert-true (compile-template-and-test *test-list7* *test-template7*))
  (lisp-unit:assert-true (compile-template-and-test *test-list8* *test-template8*)))

(lisp-unit:define-test test-compiled-false 
  (lisp-unit:assert-false (compile-template-and-test *test-list6* *test-template6*))
  (lisp-unit:assert-false (compile-template-and-test *test-list1* *test-template2*))
  (lisp-unit:assert-false (compile-template-and-test *test-list2* *test-template1*))
  (lisp-unit:assert-false (compile-template-and-test *test-list9* *test-template9*)))

(lisp-unit:define-test test-compiled-error
  (lisp-unit:assert-error 'bad-template-format
                          (compile-template-and-test *test-list10* *test-template10*))
  (lisp-unit:assert-error 'bad-template-format
                          (compile-template-and-test *test-list11* *test-template11*)))



