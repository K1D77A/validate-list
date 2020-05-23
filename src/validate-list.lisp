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


(defparameter *current-keys* '(:equal :type :between :minlen
                               :maxlen :less-than :greater-than
                               :or :satisfies))

(defvar *functions* (make-hash-table :test #'eq))

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

(define-condition failed-to-validate (error)
  ((key
    :initarg :key 
    :accessor key)
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

(defun signal-failed-to-validate (keyword arg entry message)
  (error 'failed-to-validate
         :key keyword
         :arg arg
         :entry entry
         :message message))

(defun signal-unknown-keyword (keyword message)
  (error 'unknown-keyword
         :unknown-keyword-keyword keyword
         :unknown-keyword-message message))

(defun repeat-test (length validation-list)
  "Returns a list of length LENGTH which simply repeats validation-list"
  (loop :for x :from 0 :below length
        :collect validation-list))

(defun repeat-pattern (length pattern-list)
  "Given a length and a pattern-list this will return a list of length with pattern-list 
repeated LENGTH times"
  (loop :for x :from 0 :below length
        :appending pattern-list))

(defun define-key (key func)
  "Takes in a keywrod and associates the keyword with the function. The function must accept two
  arguments, the first an entry ie a value in a list wanting to be validated and the second an object
  see any of the other uses of DEFINE-KEY in src/validate-list.lisp to get an idea what your lambda should look like. Here is 
  an example (define-key :n= 
                  (lambda (entry arg) 
                      (check-type entry number)
                      (check-type arg number)
                      (= entry arg)))
  Now with the new keyword :n= defined this can be used in a template like so where list is '(100)
  and the template is '((:n= 100)). 
"
  (check-type key keyword)
  (check-type func function)
  (setf (gethash key *functions*)
        func))

(define-key :type
    (lambda (entry type)
      (check-type type symbol)
      (typep entry type)))

(define-key :minlen
    (lambda (entry minlen)
      (check-type minlen (integer 0))
      (greater-than-or-equal (length entry) minlen)))

(define-key :maxlen
    (lambda (entry maxlen)
      (check-type maxlen (integer 1))
      (greater-than-or-equal maxlen (length entry))))

(define-key :less-than
    (lambda (entry less-than)
      (check-type less-than number)
      (check-type entry number)
      (less-than entry less-than)))

(define-key :greater-than
    (lambda (entry greater-than)
      (check-type greater-than number)
      (check-type entry number)
      (greater-than entry greater-than)))

(define-key :or
    (lambda (entry list-of-potentials)
      (find entry list-of-potentials :test #'equalp)))

(define-key :equal
    (lambda (entry equal)
      (equalp entry equal)))

(define-key :satisfies
    (lambda (entry func)
      (check-type func (or function list))
      (typecase func
        (list (some (lambda (func)
                      (funcall func entry))
                    func))
        (function (funcall func entry)))))

(define-key :between
    (lambda (entry between-list)
      (check-type entry number)
      (check-type between-list list)
      (let ((max (reduce #'max between-list))
            (min (reduce #'min between-list)))
        (greater-than max entry min))))

;;;map-list is not efficient should just traverse list once
(defun map-plist (func plist)
  "Maps a PLIST and calls FUNC that accepts two arguments. returns a list of
  funcall-result"
  (check-type plist list)
  (check-type func function)
  (loop :with len := (length plist)
        :for x :from 0 :to (1- len) :by 2
        :for y :from 1 :to  len :by 2
        :for key := (nth x plist)
        :for val := (nth y plist)
        :collect (funcall func key val)))

(defun keyword->function (keyword)
  (check-type keyword keyword)
  (let ((func (gethash keyword *functions*)))
    (if func
        func
        (signal-unknown-keyword keyword
                                (format nil
                                        "There is no function associated with the keyword ~A. Please define one with (define-key ..) or remove ~A from your template"
                                        keyword keyword)))))

(defun process-template-entry (template-entry entry)
  (map-plist (lambda (keyword arg)
               (if (not (funcall (keyword->function keyword) entry arg))
                   (signal-failed-to-validate keyword arg entry
                                              (format nil
                                                      "failed to validate '(~A ~A) with entry ~A"
                                                      keyword arg entry))
                   t))
             template-entry))

(defun nested-list-length (list)
  "Recurses through LIST and returns how many elements there are in the nested list"
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
  "Counts how many 'valid' lists are contained within LIST. Checking if a list is valid is done
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
                            ;;attempting to ignore lists that are args to functions. don't use
                            ;;keywords as args to funcs
                            (incf n))
                        (rec (first list))
                        (rec (rest list)))))))
      (rec list))
    n))

(defun validate-list-p (list template)
  "Takes in a LIST that you want to validate, and a TEMPLATE, the TEMPLATE is a list of lists,
each list contains keywords and their values (a list of the keywords is in *current-keys*). Each list
within the template represents 1 element in the LIST and is a 'description' of its contents. 
For example given the template '((:equal \"key\") (:type string :maxlen 40)) this could be used
to validate the list '(\"key\" \"abcdeegadfgfsdf\") because as the template says, the first item in 
list is \"key\" and the second according to the template should be of type 'string and no longer
than 40 characters long, which it is not, so this is valid and will return t, if a list fails when
checked against the template then this func returns nil. For a list of examples see src/validate-list.lisp"
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


