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
;;;;'(("key") (:type string :maxlen 40)) the length indicates two arguments
;;;;another example could ("year" 2020 ("country" "USA")) where a valid template would look like
;;;;'(("year")(:type integer :between (2100 1900))
;;;;  (("country")(:type string :maxlen 50))

;;;;proposed symbols
;;;; :equal :type :between :minlen :maxlen :less-than :greater-than :or :satisfies
(defparameter *test-list1* '("key" "abcdeegadfgfsdf"))
(defparameter *test-template1* '((:equal "key") (:type string :maxlen 40)))

(defparameter *test-list2*  '("year" 2020 ("country" "USA")))
(defparameter *test-template2* '((:equal "year")(:type integer :between (2100 1900))
                                 ((:or ("cookie" "country"))(:type string :maxlen 50))))

(defun handle-type (entry type)
  (check-type type symbol)
  (typep entry type))

(defun handle-minlen (entry minlen)
  (check-type minlen integer)
  (when (less-than 0 minlen)
    (error "minlen is less than 0"))
  (greater-than (length entry) minlen))

;; (lisp-unit:define-test test-minlen
;;   (lisp-unit:assert-true (handle-minlen "abc" 2))
;;   (lisp-unit:assert-true (handle-minlen "abc" 3))
;;   (lisp-unit:assert-false (handle-minlen "" 3))
;;   (lisp-unit:assert-error 'simple-error (handle-minlen 1 1))
;;   (lisp-unit:assert-error 'simple-error (handle-minlen "1" -1)))


(defun handle-maxlen (entry maxlen)
  (check-type maxlen integer)
  (unless (greater-than-or-equal maxlen 1)
    (error "maxlen is not 1 or greater"))
  (greater-than maxlen (length entry)))

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
  (check-type func function)
  (funcall func entry))

(defun handle-between (entry between-list)
  (check-type entry number)
  (check-type between-list list)
  (let ((max (reduce #'max between-list))
        (min (reduce #'min between-list)))
    (greater-than max entry min)))

(defun map-plist (func plist)
  "maps a plist and calls a func that accepts two arguments. returns a list of
 (list key funcall-result)"
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
    (:otherwise    (error "invalid keyword"))));;change to a condition

(defun process-template-entry (template-entry entry)
  ;;(:equal "key")
  ;;or  (:type string :maxlen 40)
  ;;and the entry "key" or a string as examples
  (map-plist (lambda (sym arg)
               (call-right-fun-from-sym sym arg entry))
             template-entry))


(defun validate-list (list template)
  (when (/= (length list)(length template))
    (error "list and template are not the same length"));;replace with a condition or maybe just a nil
  (labels ((rec (list template acc)
             (format t "~&entry: ~A~%"list)
             (format t "~&template ~A~%" template)
             (cond ((or (null list)
                        (null template))
                    nil)
                   ((listp (first template))
                    (append (rec (first list) (first template) acc)
                            (rec (rest list) (rest template) acc)))
                   (t (process-template-entry template list)))))
    (not (some #'null (rec list template '())))))
  ;; (mapcar (lambda (template-entry list-entry)
  ;;           (process-template-entry template-entry list-entry))
  ;;         template list))











