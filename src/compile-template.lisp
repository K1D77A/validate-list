(in-package #:validate-list)

(defun compile-template (template &optional (see-code nil))
  "Takes in a valid TEMPLATE and returns a compiled 1 argument function. This function is called
with a list, ie the one you wish to validate. This function will signal a BAD-TEMPLATE-FORMAT condition if the template is bad."
  (when (is-valid-template template)
    (let ((code
            `(lambda (list)
               (check-type list list)
               (handler-case
                   (let ((temp-as-funs (the list ',(process-template-funs template))))
                     (if (/= (the fixnum
                                  ,(template-nested-length template))
                             (the fixnum
                                  (nested-list-length list)))
                         nil
                         (labels ((func (lst lst1)
                                    (check-type lst1 list)
                                    (loop :for ele1 list :in lst 
                                          :for ele2 :in lst1
                                          :if (and (not (functionp (first ele1)))
                                                   (listp ele2))
                                            :do  (func ele1 ele2)
                                          :else
                                            :do (loop :for fun-and-arg list :in ele1
                                                      :do (let ((fun (the function (fun fun-and-arg)))
                                                                (arg (argument fun-and-arg)))
                                                            (call-fun-check-true fun ele2 arg))))))
                           (func temp-as-funs list)
                           t)))
                 (failed-to-validate () nil)))))
      (when see-code
        (format t "~&~S~%" code))
      (compile nil code))))

(defun compile-template-entry (template-entry)
  (check-type template-entry list)
  `,(map-plist (lambda (key val)
                 (list (keyword->function key) val))
               template-entry))

(defun fun (template-fun)  
  (check-type template-fun list)
  (let ((f (first template-fun)))
    (if (functionp f)
        f
        (error "not a template function"))))

(defun argument (template-fun)  
  (check-type template-fun list)
  (second template-fun))

(defun process-template-funs (template)
  "Takes in a TEMPLATE and converts all the key args within each plist into the associated functions
ready to be called"
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

(declaim (inline call-fun-check-true))
(defun call-fun-check-true (fun arg1 arg2)
  (check-type fun function)
  (if (funcall fun arg1 arg2)
      t
      (signal-failed-to-validate fun arg2 arg1 (format nil "Failed to validate calling ~A" fun))))
