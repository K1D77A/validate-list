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
  (check-type type (or symbol list))
  (typecase type
    (list (some (lambda (type)
                  (typep entry type))
                type))
    (symbol (typep entry type))))

(define-key :type #'handle-type)

(defun handle-minlen (entry minlen)
  (check-type minlen (integer 0))
  (greater-than-or-equal (length entry) minlen))

(define-key :minlen #'handle-minlen)

(defun handle-maxlen (entry maxlen)
  (check-type maxlen (integer 1))
  (greater-than-or-equal maxlen (length entry)))

(define-key :maxlen #'handle-maxlen)

(defun handle-length (entry length)
  (check-type length (integer 1))
  (= length (length entry)))

(define-key :length #'handle-length)

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
  (check-type func (or symbol function list))
  (typecase func
    (list (some (lambda (fun)
                  (if (equal (type-of fun) 'CONS)
                      (error "Please remove #' from the functions names within the list: ~A" func)
                      (funcall fun entry)))
                func))
    ((or function symbol) (funcall func entry))))

(define-key :satisfies #'handle-satisfies)

(defun handle-between (entry between-list)
  (check-type entry number)
  (check-type between-list list)
  (let ((max (reduce #'max between-list))
        (min (reduce #'min between-list)))
    (greater-than max entry min)))

(define-key :between #'handle-between)

(defun handle-contents (entry template-entry)
  (check-type entry list)
  (check-type template-entry list)
  (not (some #'null (mapcar (lambda (ele)
                              (process-template-entry template-entry ele))
                            entry))))

(define-key :contents #'handle-contents)



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
  (handler-case
      (map-plist (lambda (keyword arg)
                   (if (not (funcall (keyword->function keyword) entry arg))
                       (signal-failed-to-validate keyword arg entry
                                                  (format nil
                                                          "failed to validate '(~A ~A) with entry ~S"
                                                          keyword arg entry))
                       t))
                 template-entry)
    (simple-type-error (c)
      (signal-bad-template-format
       template-entry
       (format nil "One of the keywords (or its args) within TEMPLATE is not valid. Either correct the error or add the keyword and its functionality using 'define-key'") c))))


(defun same-structures-p (list template &optional (print-lists nil))
  "Given LIST and TEMPLATE of arbitrary depth, return t if they have the same structure or nil if
 not. In the event of any error this function returns nil, as the assumption is that the structures
are not the same."
  (handler-case
      (labels ((rec (lst1 templ)
                 (when print-lists
                   (format t "List: ~A~%Template: ~A~%" lst1 templ))
                 (cond ((and (null lst1)
                             (null templ))
                        t)
                       ((or (and (null lst1)
                                 templ)
                            (and (null templ)
                                 lst1))
                        (signal-bad-template-format template
                                                    "TEMPLATE and LIST do not have equal structures"))
                       ((listp (first templ))
                        (progn (rec (first lst1) (first templ))
                               (rec (rest lst1) (rest templ)))))))
        (rec list template)
        t)
    (condition ()
      nil)
    (bad-template-format ()
      nil)));;just a placeholder error

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
                                           (declare (ignore val))
                                           (keyword->function key)
                                           (values))
                                         (first list)))
                          (rec (first list))
                          (rec (rest list)))))))
        (rec template)
        t)
    ((or unknown-keyword bad-template-format) (c)
      (signal-bad-template-format
       template (format nil "One of the keywords within TEMPLATE are not valid. Either correct the error or add the keyword and its functionality using 'define-key'") c))
    (SIMPLE-TYPE-ERROR (c)
      (signal-bad-template-format
       template (format nil "The keys within TEMPLATE plist are not all keywords.") c))
    (SIMPLE-ERROR (c)
      (signal-bad-template-format
       template (format nil "TEMPLATE provided contains invalid plists.") c))))

(defparameter *validate-list-p-err-message*
  "The structure of LIST and TEMPLATE are not the same. Often this error occurs because you have
 messed up the construction of your TEMPLATE because getting the nesting correct can be a pain, it
 is also possible that the list you are trying to validate doesn't have the structure you expected.
Please consider looking at how you have constructed your template especially use of , and ,@.
 Consider using 'is-valid-template' to check to make sure the structure of your template is valid
 and no keywords are broken. You can also use 'same-structures-p' if you have an example list to 
validate your TEMPLATE with.")

(defun validate-list (list template)
  "Takes in a LIST that you want to validate, and a TEMPLATE, the TEMPLATE is a list of lists,
each list contains keywords and their values (a full list of keys can be found by calling CURRENT-KEYS), if TEMPLATE is 'invalid' then the condition BAD-TEMPLATE-FORM is signalled. Each list
within the template represents 1 element in the LIST and is a 'description' of its contents. 
For example given the template '((:equal \"key\") (:type string :maxlen 40)) this could be used
to validate the list '(\"key\" \"abcdeegadfgfsdf\") because as the template says, the first item in 
list is \"key\" and the second according to the template should be of type 'string and no longer
than 40 characters long, which it is not, so this is valid and will return t, if a list fails when
checked against the template then this func signals the condition FAILED-TO-VALIDATE, which
will contain information about where validation failed. For a list of examples see src/tests.lisp. 
In the interests of speed no checks are done to validate your template before validation happens, 
you can use 'is-valid-template' as a precursory check to make sure that the template is
constructed with valid plists and valid keywords."
  ;;need to state that only copy the ones that pass xD
  (check-type list list)
  (check-type template list)
  (handler-case 
      (labels ((rec (list templ acc)                 
                 (cond ((and (null list)
                             (null templ))
                        nil)
                       ((or (and (null list) templ)
                            (and (null templ) list))
                        (signal-bad-template-format
                         template
                         (format nil "~A" *validate-list-p-err-message*)))
                       ;; (error "list or templ is nil"))
                       ((listp (first templ))                        
                        (append (rec (first list) (first templ) acc)
                                (rec (rest list) (rest templ) acc)))
                       ;;here I need a check (find :type templ)
                       ;;and (find 'list )
                       ;;and then have to handle processing differently :cry:
                       ((and (listp list)
                             (and (= (position :type templ :test #'eq)
                                     (1- (position 'list templ :test #'eq)))))
                        (process-template-entry templ list))
                       (t                        
                        (process-template-entry templ list)))))
        (rec list template '())
        t)
    (SIMPLE-ERROR (c)
      (signal-bad-template-format
       template (format nil "You hit a type error SIMPLE-ERROR. Alexandria's DOPLIST signals
conditions of this type when a plist is malformed, so go and make sure your TEMPLATE consists
of valid plists") c))
    (type-error (c)
      (signal-bad-template-format
       template (format nil "You hit a type error. The implication is that you are trying to
validate a LIST whose structure is not the same as your TEMPLATE.") c))
    (unknown-keyword (c)
      (signal-bad-template-format
       template (format nil "One of the keywords within TEMPLATE are not valid. Either correct the error or add the keyword and its functionality using 'define-key'") c))))

