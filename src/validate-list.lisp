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

(defvar *functions* (make-hash-table :test #'eq))

(defun current-keys ()
  (alexandria:hash-table-keys *functions*))

(defun repeat-test (length validation-list)
  "Returns a list of length LENGTH which simply repeats VALIDATION-LIST"
  (loop :for x :from 0 :below length
        :collect validation-list))

(defun repeat-pattern (length pattern-list)
  "Given a length and a list this will return a list of LENGTH with PATTERN-LIST 
repeated LENGTH times"
  (loop :for x :from 0 :below length
        :appending pattern-list))

(defun define-key (key func)
  "Takes in a keyword and associates the keyword with the function. The function must accept two
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

(defun handle-type (entry type)
  (check-type type symbol)
  (typep entry type))

(define-key :type #'handle-type)

(defun handle-minlen (entry minlen)
  (check-type minlen (integer 0))
  (greater-than-or-equal (length entry) minlen))

(define-key :minlen #'handle-minlen)

(defun handle-maxlen (entry maxlen)
  (check-type maxlen (integer 1))
  (greater-than-or-equal maxlen (length entry)))

(define-key :maxlen #'handle-maxlen)
    
(defun handle-less-than (entry less-than)
  (check-type less-than number)
  (check-type entry number)
  (less-than entry less-than))

(define-key :less-than #'handle-less-than)

(defun handle-greater-than (entry greater-than)
  (check-type greater-than number)
  (check-type entry number)
  (greater-than entry greater-than))

(define-key :greater-than #'handle-greater-than)

(defun handle-or (entry list-of-potentials)
  (check-type list-of-potentials list)
  (some (lambda (pot)
          (equalp pot entry))
        list-of-potentials))

(define-key :or #'handle-or)

(defun handle-equal (entry equal)
  (equalp entry equal))

(define-key :equal #'handle-equal)

(defun handle-satisfies (entry func)
  (check-type func (or function list))
  (typecase func
    (list (some (lambda (func)
                  (funcall func entry))
                func))
    (function (funcall func entry))))

(define-key :satisfies #'handle-satisfies)

(defun handle-between (entry between-list)
  (check-type entry number)
  (check-type between-list list)
  (let ((max (reduce #'max between-list))
        (min (reduce #'min between-list)))
    (greater-than max entry min)))

(define-key :between #'handle-between)

(defun map-plist (func plist)
  "Maps a PLIST and calls FUNC that accepts two arguments. returns a list of
  funcall-result"
  (check-type plist list)
  (check-type func function)
  (let ((res))
    (alexandria:doplist (key val plist res)
                        (check-type key keyword)
      (setf res (append res (list (funcall func key val)))))))

(defun keyword->function (keyword)
  "Given a keyword in KEYWORD, this function looks for the associated function in *functions* if found
then the function is returned, otherwise a UNKNOWN-KEYWORD condition is signalled."
  (check-type keyword keyword)
  (let ((func (gethash keyword *functions*)))
    (if func
        func
        (signal-unknown-keyword keyword
                                (format nil
                                        "There is no function associated with the keyword ~A.
 Please define one with (define-key ..) 
or remove ~A from your template"
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

(defun is-valid-template (template)
  "Takes in a TEMPLATE and attempts to make sure it has a valid structure. If it does not then signals
condition BAD-TEMPLATE-FORMAT"
  (check-type template list)
  (handler-case
      (labels ((rec (list)
                 (cond ((null list)
                        nil)
                       ((listp list)
                        (progn
                          (if (and (listp (first list)) (keywordp (first (first list))))
                              (map-plist (lambda (key val)
                                           (declare (ignore key val))
                                           (values))
                                         (first list)))
                          (rec (first list))
                          (rec (rest list)))))))
        (rec template)
        t)
    (SIMPLE-TYPE-ERROR (c)
      (signal-bad-template-format
       template (format nil "The keys within TEMPLATE plist are not all keywords.") c))
    (SIMPLE-ERROR (c)
      (signal-bad-template-format
       template (format nil "TEMPLATE provided contains invalid plists.") c))))

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
each list contains keywords and their values (a full list of keys can be found by calling CURRENT-KEYS), if TEMPLATE is 'invalid' then the condition BAD-TEMPLATE-FORM is signalled. Each list
within the template represents 1 element in the LIST and is a 'description' of its contents. 
For example given the template '((:equal \"key\") (:type string :maxlen 40)) this could be used
to validate the list '(\"key\" \"abcdeegadfgfsdf\") because as the template says, the first item in 
list is \"key\" and the second according to the template should be of type 'string and no longer
than 40 characters long, which it is not, so this is valid and will return t, if a list fails when
checked against the template then this func returns nil. For a list of examples see src/tests.lisp"
  (check-type list list)
  (check-type template list)
  (when (is-valid-template template)
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
              (rec list template '())
              t))
      (failed-to-validate ()
        nil))))
