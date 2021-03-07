(in-package #:validate-list/tests)
;;;these should go in another file

(defparameter *test-list1*
  '("key" "abcdeegadfgfsdf"))
(defparameter *test-template1*
  '((:equal "key") (:type string :maxlen 40)))

(defparameter *test-list2*
  '("year" 2020 ("country" "USA")))
(defparameter *test-template2*
  '((:equal "year")(:type integer :between (2100 1900))
    ((:or ("cookie" "country"))(:type string :maxlen 50))))

(defparameter *test-list3*
  '("year" 98 ("country" ("USA" "UK" "Poland"))))
(defparameter *test-template3*
  '((:equal "year")(:type integer :or (96 97 98))
    ((:or ("cookie" "country"))
     ((:equal "USA")(:equal "UK")(:equal "Poland")))))

(defparameter *test-list4*
  '("year" 98 ("country" ("USA" "UK" "Poland"))))
(defparameter *test-template4*
  `((:equal "year")(:type integer :or (96 97 98))
    ((:or ("cookie" "country"))
     ,(repeat-test 3 '(:type string :maxlen 6 :minlen 2)))))

(defparameter *test-list5*
  '("year" 98 ("keyvals" ("USA" 35 "Poland" 55 "UK" 96))))
(defparameter *test-template5*
  `((:equal "year")(:type integer :or (96 97 98))
    ((:or ("cookie" "country" "keyvals"))
     ,(repeat-pattern 3 '((:type string :maxlen 6 :minlen 2)
                          (:type number :between (0 100)))))))

(defparameter *test-list6*
  '("year" 98 ("keyvals" ("USA" 35 "Poland" 55 "UK" 96))))
(defparameter *test-template6*
  `((:equal "year")(:type integer :or (96 97 98))
    ((:or ("cookie" "country" "keyvals"))
     ,(repeat-pattern 4 '((:type string :maxlen 6 :minlen 2)
                          (:type number :between (0 100)))))))
;;;^should fail because the end is len 4 instead of len 3
(defparameter *test-list7*
  '("year" 98 ("keyvals" ("USA" 35 "Poland" 55 "UK" 96) 2 4 6)))
(defparameter *test-template7*
  `((:equal "year")(:type integer :or (96 97 98))
    ((:or ("cookie" "country" "keyvals"))
     ,(repeat-pattern 3 '((:type string :maxlen 6 :minlen 2)
                          (:type number :between (0 100))))
     ,@(repeat-test 3 '(:type number :satisfies evenp)))))

(defparameter *test-list8*
  '("year" 98 ("keyvals" ("USA" 35 "Poland" 55 "UK" 96) 2 5 6)))
(defparameter *test-template8*
  `((:equal "year")(:type integer :or (96 97 98))
    ((:or ("cookie" "country" "keyvals"))
     ,(repeat-pattern 3 '((:type string :maxlen 6 :minlen 2)
                          (:type number :between (0 100))))
     ,@(repeat-test 3 '(:type number :satisfies (evenp oddp))))))
;;9 is broken intentionally. bad :or 
(defparameter *test-list9*
  '("year" 98 ("keyvals" ("USA" 35 "Poland" 55 "UK" 96) 2 5 6)))
(defparameter *test-template9*
  `((:equal "year")(:type integer :or (96 97 98))
    ((:or ("cookie" "country"))
     ,(repeat-pattern 3 '((:type string :maxlen 6 :minlen 2)
                          (:type number :between (0 100)))))
    ,(repeat-test 3 '(:type number :satisfies (evenp oddp)))))

;;10 is broken intentionally. bad :equal
(defparameter *test-list10*
  '("year" 98 ("keyvals" ("USA" 35 "Poland" 55 "UK" 96) 2 5 6)))
(defparameter *test-template10*
  `((:equal "year")(:type integer :or (96 97 98))
    ((:or ) ;;broken intentionally
     ,(repeat-pattern 3 '((:type string :maxlen 6 :minlen 2)
                          (:type number :between (0 100)))))
    ,(repeat-test 3 '(:type number :satisfies (evenp oddp)))))

;;11 is broken intentionally. invalid :or
(defparameter *test-list11*
  '("year" 98 ("keyvals" ("USA" 35 "Poland" 55 "UK" 96) 2 5 6)))
(defparameter *test-template11*
  `((:equal "year")(:type integer :or (96 97 98))
    ((:or abc def hhh) ;;broken intentionally
     ,(repeat-pattern 3 '((:type string :maxlen 6 :minlen 2)
                          (:type number :between (0 100)))))
    ,(repeat-test 3 '(:type number :satisfies (evenp oddp)))))

;;12 is broken intentionally, bad :or
(defparameter *test-list12*
  '("year" 98 ("keyvals" ("USA" 35 "Poland" 55 "UK" 96) 2 5 (6 7 8))))
(defparameter *test-template12*
  `((:equal "year")(:type integer :or (96 97 98))
    ((:or "keyvals" "time") ;;broken intentionally
     ,(repeat-pattern 3 '((:type string :maxlen 6 :minlen 2)
                          (:type number :between (0 100)))))
    ,(repeat-test 2 '(:type number :satisfies (evenp oddp)))
    (:type list :minlen 1 :maxlen 3)))

(defparameter *test-list12a*
  '("year" 98 ("keyvals" ("USA" 35 "Poland" 55 "UK" 96)
               1 2 (6 7) (8) "abc")))
(defparameter *test-template12a*
  `((:equal "year")(:type integer :or (96 97 98))
    ((:or ("keyvals" "time")) ;;broken intentionally
     ,(repeat-pattern 3 '((:type string :maxlen 6 :minlen 2)
                          (:type number :between (0 100))))
     ,@(repeat-test 2 '(:type number :satisfies (evenp oddp)))
     (:type list :length 2)
     (:type list :minlen 1)
     (:type string :equal "abc"))))

(defparameter *test-list13*
  '("year" 98 ("keyvals" ("USA" 35 "Poland" 55 "UK" 96)
               (2) (6 7) ("oof" "boof" "kadoof") "abc")))
(defparameter *test-template13*
  `((:equal "year")(:type integer :or (96 97 98))
    ((:or ("keyvals" "time")) ;;broken intentionally
     ,(repeat-pattern 3 '((:type string :maxlen 6 :minlen 2)
                          (:type number :between (0 100))))
     (:type list :minlen 1)
     (:type list :maxlen 2)
     (:type list :minlen 1 :maxlen 5 :contents (:type string))
     (:type string :equal "abc"))))

;;14 is bad on purpose, , instead of ,@
(defparameter *test-list14*
  '("year" 98 ("keyvals" ("USA" 35 "Poland" 55 "UK" 96)
               1 2 (2) (6 7) ("oof" 23 "kadoof") "abc")))
(defparameter *test-template14*
  `((:equal "year")(:type integer :or (96 97 98))
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

(defparameter *test-list15*
  '("year" 98 ("keyvals" ("USA" 35 "Poland" 55 "UK" 96)
               (2) (6 7) ("oof" 23 "kadoof") "abc")))
(defparameter *test-template15*
  `((:equal "year")(:type integer :or (96 97 98))
    ((:or ("keyvals" "time")) 
     ,(repeat-pattern 3
                      '((:type string :maxlen 6 :minlen 2)
                        (:type number :between (0 100))))
     (:type list :length 1 :contents (:type number))
     (:type list :length 2)
     (:type list :minlen 1 :maxlen 5 :contents (:type (string number)))
     (:type string :equal "abc"))))

(defparameter *test-list16*
  '("year" 98 ("keyvals" ("USA" 35 "Poland" 55 "UK" 96)
               1 2 (2) (6 7) ("oof" "oof" "oof") "abc")))
(defparameter *test-template16*
  `((:equal "year")(:type integer :or (96 97 98))
    ((:or ("keyvals" "time")) 
     ,(repeat-pattern 3 '((:type string :maxlen 6 :minlen 2)
                          (:type number :between (0 100))))
     ,@(repeat-test 2 '(:type number :satisfies (evenp oddp)))
     (:type list :length 1)
     (:type list :contents (:type number :satisfies (evenp oddp)))
     (:type list :minlen 1 :maxlen 5
      :contents (:type string :maxlen 5 :equal "oof"))
     (:type string :equal "abc"))))

;;bad :maxlen 3 when maxlen is 6
(defparameter *test-list17*
  '("year" 98 ("keyvals" ("USA" 35 "Poland" 55 "UK" 96)
               1 2 (2) (6 7) ("oof" "oof" "oof") "abc")))
(defparameter *test-template17*
  `((:equal "year")(:type integer :or (96 97 98))
    ((:or ("keyvals" "time")) 
     ,(repeat-pattern 3 '((:type string :maxlen 3 :minlen 2)
                          (:type number :between (0 100))))
     ,@(repeat-test 2 '(:type number :satisfies (evenp oddp)))
     (:type list :length 1)
     (:type list :contents (:type number :satisfies (evenp oddp)))
     (:type list :minlen 1 :maxlen 5
      :contents (:type string :maxlen 5 :equal "oof"))
     (:type string :equal "abc"))))

;;bad :type string when number
(defparameter *test-list18*
  '("year" 98 ("keyvals" ("USA" 35 "Poland" 55 "UK" 96)
               1 2 (2) (6 7) ("oof" "oof" "oof") "abc")))
(defparameter *test-template18*
  `((:equal "year")(:type integer :or (96 97 98))
    ((:or ("keyvals" "time")) 
     ,(repeat-pattern 3 '((:type string :maxlen 6 :minlen 2)
                          (:type number :between (0 100))))
     ,@(repeat-test 2 '(:type number :satisfies (evenp oddp)))
     (:type list :length 1)
     (:type list :contents (:type string))
     (:type list :minlen 1 :maxlen 5
      :contents (:type string :maxlen 5 :equal "oof"))
     (:type string :equal "abc"))))

;;bad :equal "abcd" instead of "abc"
(defparameter *test-list19*
  '("year" 98 ("keyvals" ("USA" 35 "Poland" 55 "UK" 96)
               1 2 (2) (6 7) ("oof" "oof" "oof") "abc")))
(defparameter *test-template19*
  `((:equal "year")(:type integer :or (96 97 98))
    ((:or ("keyvals" "time")) 
     ,(repeat-pattern 3 '((:type string :maxlen 6 :minlen 2)
                          (:type number :between (0 100))))
     ,@(repeat-test 2 '(:type number :satisfies (evenp oddp)))
     (:type list :length 1)
     (:type list :contents (:type number :satisfies (evenp oddp)))
     (:type list :minlen 1 :maxlen 5
      :contents (:type string :maxlen 5 :equal "oof"))
     (:type string :equal "abcd"))))

;;;broken structures
(defparameter *broken-struct-list1*
  '("year" 98 ("keyvals" ("USA" 35 "Poland" 55 "UK" 96) 2 5 6)))
(defparameter *broken-struct1*
  `((:equal "year")(:type integer :or (96 97 98))
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

(define-test test-validation-true
  (assert-true (validate-list *test-list1* *test-template1*))
  (assert-true (validate-list *test-list2* *test-template2*))
  (assert-true (validate-list *test-list3* *test-template3*))
  (assert-true (validate-list *test-list4* *test-template4*))
  (assert-true (validate-list *test-list5* *test-template5*))
  (assert-true (validate-list *test-list7* *test-template7*))
  (assert-true (validate-list *test-list8* *test-template8*))
  (assert-true (validate-list *test-list12a* *test-template12a*))
  (assert-true (validate-list *test-list15* *test-template15*))
  (assert-true (validate-list *test-list13* *test-template13*))
  (assert-true (validate-list *test-list16* *test-template16*)))

(define-test test-validation-false
  (assert-error 'failed-to-validate (validate-list *test-list1* *test-template2*))
  ;;^ fails to validate before the bad structure is noticed
  (assert-error 'failed-to-validate (validate-list *test-list2* *test-template1*))
  ;;^ fails to validate before the bad structure is noticed
  (assert-error 'failed-to-validate (validate-list *test-list9* *test-template9*))
  (assert-error 'failed-to-validate (validate-list *test-list17* *test-template17*))
  (assert-error 'failed-to-validate (validate-list *test-list18* *test-template18*))
  (assert-error 'failed-to-validate
                (validate-list *test-list19* *test-template19*))  
  (assert-error 'bad-template-format
                (validate-list *test-list14* *test-template14*)))

(define-test test-validation-error
  (assert-error 'bad-template-format (validate-list *test-list6* *test-template6*))
  (assert-error 'bad-template-format (validate-list *test-list10* *test-template10*))
  (assert-error 'bad-template-format (validate-list *test-list12* *test-template12*))
  (assert-error 'bad-template-format
                (validate-list *test-list11* *test-template11*)))

(define-test test-compiled-true
  (assert-true (compile-template-and-test *test-list1* *test-template1*))
  (assert-true (compile-template-and-test *test-list2* *test-template2*))
  (assert-true (compile-template-and-test *test-list3* *test-template3*))
  (assert-true (compile-template-and-test *test-list4* *test-template4*))
  (assert-true (compile-template-and-test *test-list5* *test-template5*))
  (assert-true (compile-template-and-test *test-list7* *test-template7*))
  (assert-true (compile-template-and-test *test-list8* *test-template8*))
  (assert-true (compile-template-and-test *test-list12a* *test-template12a*))
  (assert-true (compile-template-and-test *test-list15* *test-template15*))
  (assert-true (compile-template-and-test *test-list13* *test-template13*))
  (assert-true (compile-template-and-test *test-list16* *test-template16*)))

(define-test test-compiled-false 
  (assert-error 'bad-template-format
                (compile-template-and-test *test-list6* *test-template6*))
  (assert-error 'bad-template-format
                (compile-template-and-test *test-list10* *test-template10*))
  (assert-error 'bad-template-format
                (compile-template-and-test *test-list12* *test-template12*))
  (assert-error 'bad-template-format
                (compile-template-and-test *test-list10* *test-template10*))
  (assert-error 'bad-template-format
                (compile-template-and-test *test-list11* *test-template11*))
  (assert-error 'bad-template-format
                (compile-template-and-test *test-list1* *test-template2*))
  ;;^ fails to validate before the bad structure is noticed
  (assert-error 'bad-template-format
                (compile-template-and-test *test-list2* *test-template1*))
  ;;^ fails to validate before the bad structure is noticed
  (assert-error 'bad-template-format
                (compile-template-and-test *test-list9* *test-template9*))
  
  (assert-error 'bad-template-format
                (compile-template-and-test *test-list14* *test-template14*))
  (assert-error 'failed-to-validate
                (compile-template-and-test *test-list17* *test-template17*))
  (assert-error 'failed-to-validate
                (compile-template-and-test *test-list18* *test-template18*))
  (assert-error 'failed-to-validate
                (compile-template-and-test *test-list19* *test-template19*)))

(define-test test-compiled-error
  (assert-error 'bad-template-format
                (compile-template-and-test *test-list10* *test-template10*))
  (assert-error 'bad-template-format
                (compile-template-and-test *test-list11* *test-template11*)))

(define-test compile-quoted
  "Test compiling quoted templates"
  (assert-true (funcall (compile-template *test-template7*) *test-list7*))
  (assert-true (funcall (compile-template *test-template7c*) *test-list7*)))

(define-test validate-quoted
  "Test validating with quoted templates"
  (assert-true (validate-list *test-list7* *test-template7c*)))

(define-test compiler-macro
  "Automatic conversion of the constant list to its compiled counterpart."
  (assert-true (validate-list *test-list7*
                              '((:EQUAL "year") (:TYPE INTEGER :OR (96 97 98))
                                ((:OR ("cookie" "country" "keyvals"))
                                 ((:TYPE STRING :MAXLEN 6 :MINLEN 2)
                                  (:TYPE NUMBER :BETWEEN (0 100))
                                  (:TYPE STRING :MAXLEN 6 :MINLEN 2)
                                  (:TYPE NUMBER :BETWEEN (0 100))
                                  (:TYPE STRING :MAXLEN 6 :MINLEN 2)
                                  (:TYPE NUMBER :BETWEEN (0 100)))
                                 (:TYPE NUMBER :SATISFIES EVENP)
                                 (:TYPE NUMBER :SATISFIES EVENP)
                                 (:TYPE NUMBER :SATISFIES EVENP)))))
  (assert-true (validate-list *test-list8*
                              `((:equal "year")(:type integer :or (96 97 98))
                                ((:or ("cookie" "country" "keyvals"))
                                 ,(repeat-pattern 3 '((:type string :maxlen 6
                                                       :minlen 2)
                                                      (:type number
                                                       :between (0 100))))
                                 ,@(repeat-test 3 '(:type number
                                                    :satisfies (evenp oddp))))))))
