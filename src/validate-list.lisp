;;;; validate-list.lisp

(in-package #:validate-list)

;;;;okay, consider we are taking input from the outside world in the form of json
;;;;and this json is converted into a list so it can be easily manipulated in Lisp.
;;;;However whatever json is parsed could be nothing like what you expect.
;;;;so perhaps you expect two bits of data in your json
;;;;perhaps you expect a key like "key" and some data as a string of a maximum length say
;;;;a maximum of length 40 well this library attempts to allow you to construct templates
;;;;of what you would expect, for example parsed that json would look something like
;;;;("key" "abcdeegadfgfsdf") now a template for this would look something like
;;;;'((:equal "key") (:type string :maxlen 40)) the length indicates two arguments
;;;;another example could ("year" 2020 ("country" "USA")) where a valid template would look like
;;;;'((:equal "year")(:type integer :between (2100 1900))
;;;;  ((:equal "country")(:type string :maxlen 50))


(defparameter *valid-syms* '(:equal :type :between :minlen
                             :maxlen :less-than :greater-than
                             :or :satisfies))

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

(defvar *other-symbols* (make-hash-table :test #'eq))


(define-condition failed-to-validate (error)
  ((sym
    :initarg :sym 
    :accessor sym)
   (arg
    :initarg :arg
    :accessor arg)
   (entry
    :initarg :entry
    :accessor entry)
   (message
    :initarg :message
    :accessor :message
    :documentation "Message indicating what when wrong")))

(define-condition unknown-keyword (error)
  ((keyword
    :initarg :unknown-keyword-keyword 
    :accessor unknown-keyword-keyword)
   (message
    :initarg :unknown-keyword-message
    :accessor unknown-keyword-message
    :documentation "Message indicating what when wrong")))


(defun signal-failed-to-validate (sym arg entry message)
  (error 'failed-to-validate
         :sym sym
         :arg arg
         :entry entry
         :message message))

(defun signal-unknown-keyword (keyword message)
  (error 'unknown-keyword
         :unknown-keyword-keyword keyword
         :unknown-keyword-message message))

(defun repeat-test (length validation-list)
  "returns a list of length 'length' which simply repeats validation-list"
  (loop :for x :from 0 :below length
        :collect validation-list))

(defun repeat-pattern (length pattern-list)
  "given a length and a pattern-list will return a list of length with pattern-list repeated length
times"
  (loop :for x :from 0 :below length
        :appending pattern-list))



;;;for example if you have a list containing 3 elements and you just need to know they are strings
;;;then you could just generate the code to do it
;;;our list is ("USA" "UK" "Poland") this is 3 strings maxlen 6 and minlen 2 so we could just generate
;;;((:type string :maxlen 6 :minlen 2)
;;;(:type string :maxlen 6 :minlen 2)
;;;(:type string :maxlen 6 :minlen 2))
;;;as a drop in



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


(defun add-new-symbol (symbol function)
  "Takes in a symbol and associates the symbol with the function. the function must accept two
arguments, the first an entry ie a value in a list wanting to be validated and the second an object
see any of the definitions of handle-* to get an idea what your lambda should look like. Here is 
an example (add-new-symbol :n= 
               (lambda (entry arg) 
                       (check-type entry number)
                       (check-type arg number)
                       (= arg entry)))
Now with the new symbol :n= defined this can be used in a template like so where list is '(100)
and the template is '((:n= 100)). 

"
  (check-type symbol symbol)
  (check-type function function)
  (setf (gethash symbol *other-symbols*)
        function))


(defun handle-type (entry type)
  (check-type type symbol)
  (typep entry type))

(defun handle-minlen (entry minlen)
  (check-type minlen integer)
  (when (less-than minlen 0)
    (error "minlen is less than 0"))
  (greater-than-or-equal (length entry) minlen))

(defun handle-maxlen (entry maxlen)
  (check-type maxlen integer)
  (unless (greater-than-or-equal maxlen 1)
    (error "maxlen is not 1 or greater"))
  (greater-than-or-equal maxlen (length entry)))

(defun handle-less-than (entry less-than)
  (check-type less-than number)
  (check-type entry number)
  (less-than entry less-than))

(defun handle-greater-than (entry greater-than)
  (check-type greater-than number)
  (check-type entry number)
  (greater-than entry greater-than))

(defun handle-or (entry list-of-potentials)
  (check-type list-of-potentials list)
  (some (lambda (pot)
          (equalp pot entry))
        list-of-potentials))

(defun handle-equal (entry equal)
  (equalp entry equal))

(defun handle-satisfies (entry func)
  (check-type func (or function list))
  (typecase func
    (list (some (lambda (func)
                  (funcall func entry))
                func))
    (function (funcall func entry))))

(defun handle-between (entry between-list)
  (check-type entry number)
  (check-type between-list list)
  (let ((max (reduce #'max between-list))
        (min (reduce #'min between-list)))
    (greater-than max entry min)))

(defun handle-other-sym (entry sym sym-arg)
  (check-type sym symbol)
  (let ((fun (gethash sym *other-symbols*)))
    (if fun
        (funcall fun entry sym-arg)
        (signal-unknown-keyword sym
                                (format nil "There is no function in *other-symbols* 
associated with ~A. Please define this with `add-new-symbol` or remove ~A from your template"
                                        sym sym)))))

(defun map-plist (func plist)
  "maps a plist and calls a func that accepts two arguments. returns a list of
  funcall-result"
  (check-type plist list)
  (check-type func function)
  (loop :with len := (length plist)
        :for x :from 0 :to (1- len) :by 2
        :for y :from 1 :to  len :by 2
        :for key := (nth x plist)
        :for val := (nth y plist)
        :collect (funcall func key val)))

(defun call-right-fun-from-sym (sym sym-arg entry)
  (check-type sym symbol)
  (case sym
    (:equal        (handle-equal entry sym-arg))
    (:type         (handle-type entry sym-arg))
    (:between      (handle-between entry sym-arg))
    (:minlen       (handle-minlen entry sym-arg))
    (:maxlen       (handle-maxlen entry sym-arg))
    (:less-than    (handle-less-than entry sym-arg))
    (:greater-than (handle-greater-than entry sym-arg))
    (:or           (handle-or entry sym-arg))
    (:satisfies    (handle-satisfies entry sym-arg))
    (:otherwise    (handle-other-sym entry sym sym-arg))))
;;;should add a way to add new symbols which is pretty trivial, just a hashtable which associates
;;;the symbols with a lambda that takes 1 arg, if call-right-fun-from-sym fails to find it
;;;immediately then it can look in the hashtable and find the func and call it. ez

(defun process-template-entry (template-entry entry)
  (map-plist (lambda (sym arg)
               (if (not (call-right-fun-from-sym sym arg entry))
                   (signal-failed-to-validate sym arg entry
                                              (format nil "failed to validate '(~A ~A) with entry ~A"
                                                      sym arg entry))
                   t))
             template-entry))

(defun nested-list-length (list)
  "recurses through list and returns how many elements there are in the nested list"
  (check-type list list)
  (let ((n 0))
    (labels ((rec (list)
               (cond ((null list)
                      nil)
                     ((atom list)
                      (incf n))
                     ((listp list)
                      (progn (rec (first list))
                             (rec (rest list)))))))
      (rec list))
    n))

(defun template-nested-length (list)
  "counts how many 'valid' lists are contained within list. Checking if a list is valid is done
by assuming that the first element is a keyword. This means that no keywords can occupy the first
element of a list being passed as args to a function"
  (check-type list list)
  (let ((n 0))
    (labels ((rec (list)
               (cond ((null list)
                      nil)
                     ((listp list)
                      (progn
                        (if (and (listp (first list)) (keywordp (first (first list))))
                            ;;attemping to ignore lists that are args to functions. don't use
                            ;;keywords as args to funcs
                            (incf n))
                        (rec (first list))
                        (rec (rest list)))))))
      (rec list))
    n))

(defun validate-list-p (list template)
  "takes in a list that you want to validate, and a template, the template is a list of lists,
each list contains keywords and their values (a list of the keywords is in *valid-syms*). Each list
within the template represents 1 element in the list and is a 'description' of its contents. 
For example given the template '((:equal \"key\") (:type string :maxlen 40)) this could be used
to validate the list '(\"key\" \"abcdeegadfgfsdf\") because as the template says, the first item in 
list is \"key\" and the second according to the template should be of type 'string and no longer
than 40 characters long, which it is not, so this is valid and will return t, if a list fails when
checked against the template then this func returns nil. another example list and template can be 
found in *test-list2* and *test-template2* respectively"
  (check-type list list)
  (check-type template list)
  (handler-case 
      (if (/= (template-nested-length template)(nested-list-length list))
          nil
          (labels ((rec (list template acc)
                     (cond ((or (null list)
                                (null template))
                            nil)
                           ((listp (first template))
                            (append (rec (first list) (first template) acc)
                                    (rec (rest list) (rest template) acc)))
                           (t (process-template-entry template list)))))
            (not (some #'null (rec list template '())))))
    (failed-to-validate ()
      nil)))


