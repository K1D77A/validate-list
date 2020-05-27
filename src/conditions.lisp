(in-package #:validate-list)

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
    :accessor message
    :documentation "Message indicating what when wrong")))

(define-condition unknown-keyword (error)
  ((keyword
    :initarg :unknown-keyword-keyword 
    :accessor unknown-keyword-keyword)
   (message
    :initarg :unknown-keyword-message
    :accessor unknown-keyword-message
    :documentation "Message indicating what when wrong")))

(define-condition bad-template-format (error)
  ((template
    :initarg :bad-template-format-template
    :accessor bad-template-format-template)
   (signaller
    :initarg :bad-template-format-signaller
    :accessor bad-template-format-signaller
    :documentation
    "Contains the condition which was called that caused this condition to be signalled")
   (message
    :initarg :bad-template-format-message
    :accessor bad-template-format-message
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

(defun signal-bad-template-format (template message &optional (signaller nil))
  (error 'bad-template-format
         :bad-template-format-template template
         :bad-template-format-message message
         :bad-template-format-signaller signaller))

(defmethod print-object ((object failed-to-validate) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~&Failed to validate list~%Keyword: ~S~%Arg(s): ~S~%Entry: ~S~%Message: ~A~%"
            (key object)
            (arg object)
            (entry object)
            (message object))))

(defmethod print-object ((object unknown-keyword) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~&Unknown Keyword~%Keyword: ~A~%Message: ~A~%"
            (unknown-keyword-keyword object)
            (unknown-keyword-message object))))
            

(defmethod print-object ((object bad-template-format) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~&Bad Template~%Message: ~A~%Template: ~A~%Signaller: ~S~%"
            (bad-template-format-message object)
            (bad-template-format-template object)
            (let ((signaller (bad-template-format-signaller object)))
              (if signaller
                  signaller
                  "signaller not set")))))
