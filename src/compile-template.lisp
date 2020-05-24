(in-package #:validate-list)
(defparameter *test-template9-expanded* '((:EQUAL "year") (:TYPE INTEGER :OR (96 97 98))
                                          ((:OR ("cookie" "country"))
                                           ((:TYPE STRING :MAXLEN 6 :MINLEN 2)
                                            (:TYPE NUMBER :BETWEEN (0 100))
                                            (:TYPE STRING :MAXLEN 6 :MINLEN 2)
                                            (:TYPE NUMBER :BETWEEN (0 100))
                                            (:TYPE STRING :MAXLEN 6 :MINLEN 2)
                                            (:TYPE NUMBER :BETWEEN (0 100))))
                                          ((:TYPE NUMBER :SATISFIES (#'EVENP #'ODDP))
                                           (:TYPE NUMBER :SATISFIES (#'EVENP #'ODDP))
                                           (:TYPE NUMBER :SATISFIES (#'EVENP #'ODDP)))))

(defparameter *test1* '("year"))
(defparameter *temp1* '((:type string :equal "year")))
(defparameter *test1-compiled* (lambda (list)
                                 (let ((first (first list)))
                                   (progn (and (funcall #'handle-type first 'string)
                                               (funcall #'handle-equal first "year"))))))

(defparameter *test2* '("year" 2000))
(defparameter *temp2* '((:type string :equal "year")
                        (:type integer :between (1900 2100))))

(defparameter *test3* '("year" 2000 ("anyway" "boof") "beef"))
(defparameter *temp3* '((:type string :equal "year")
                        (:type integer :between (1900 2100))
                        ((:type string :equal "anyway")
                         (:type string :equal "boof"))
                        (:type string :equal "beef")))



;;the compile could be a loop that steps through both at the same time
;;calling the correct function on the other list

(defun compile-template (template)
  `(lambda (list)
     (handler-case
         (let ((temp-as-funs ',(process-template-funs template)))
           (labels ((func (lst lst1)
                      (format t "~&lst: ~A~%~&lst1: ~A~%" lst lst1)
                      (mapcar (lambda (ele1 ele2)
                                (if (and (not (functionp (first ele1)))
                                         (listp ele2))
                                    (func  ele1 ele2)
                                    ;;  (when (listp (first ele1))
                                    (mapcar (lambda (fun-and-arg)
                                              (let ((fun (fun fun-and-arg))
                                                    (arg (argument fun-and-arg)))
                                                (call-fun-check-true fun ele2 arg)))
                                            ele1)))
                              lst lst1)))
             (func temp-as-funs list))
           t)
       (failed-to-validate () nil))))

(defun compile-template-entry (template-entry)
  `,(map-plist (lambda (key val)
                 (list (keyword->function key) val))
               template-entry))

(defun fun (template-fun)
  (let ((f (first template-fun)))
    (if (functionp f)
        f
        (error "not a template function"))))

(defun argument (template-fun)
  (second template-fun))

(defun process-template-funs (template)
  (labels ((rec (list acc)
             (cond ((null list)
                    nil)
                   ((listp list)
                    (if (and (listp (first list)) (keywordp (first (first list))))
                        (cons (compile-template-entry (first list))
                              (rec (rest list) acc))                        
                        (cons (rec (first list) acc)
                              (rec (rest list) acc)))))))
    (rec template nil)))

(defun call-fun-check-true (fun arg1 arg2)
  (if (funcall fun arg1 arg2)
      t
      (signal-failed-to-validate fun arg2 arg1 (format nil "failed to validate calling ~A" fun))))
