
# validate-list

## update notes

Now with a compiler macro that will convert constant templates straight into their
compiled form using `(compile-template ..)`, by constant it would mean something like
`(validate-list <my list> '((im a template)))` For a list to be satisfy
`(constantp <list>)` it should be quoted like `'(<list>)` so the code has been updated
to support validation and compilation of templates that are constructed with quotes
like `\`'(<list>)`.

I have integrated the tests with asdf so `(asdf:test-system :validate-list)`
should run seemlessly.

Compiled functions are now given a declaration of (speed 3) and (safety 1), making
them even faster than before.

## Download

grab from quicklisp
```lisp
(ql:quickload :validate-list)
(asdf:test-system :validate-list)
;inform me if any fail
(in-package :validate-list)
```

##

This is an attempt at using templates to validate the contents of a list. 
The idea came because when you parse json from an untrusted source you don't know for certain what 
has been sent, so the idea is that once parsed you can use a template to check if the parsed data 
(now a list) conforms to what you want. The primary function is 
``validate-list`` Which takes in two arguments, the list you wish to validate and a template.
Here is the docstring:<br> 

> "Takes in a LIST that you want to validate, and a TEMPLATE, the TEMPLATE is a list of lists,
> each list contains keywords and their values (a full list of keys can be found by calling
> CURRENT-KEYS), if TEMPLATE is 'invalid' then the condition BAD-TEMPLATE-FORM is signalled. Each list
> within the template represents 1 element in the LIST and is a 'description' of its contents. 
> For example given the template '((:equal \"key\") (:type string :maxlen 40)) this could be used
> to validate the list '(\"key\" \"abcdeegadfgfsdf\") because as the template says, the first item in 
> list is \"key\" and the second according to the template should be of type 'string and no longer
> than 40 characters long, which it is not, so this is valid and will return t, if a list fails when
> checked against the template then this func signals the condition FAILED-TO-VALIDATE, which
> will contain information about where validation failed. For a list of examples see 
> src/tests.lisp,the invalid templates are marked. 
> In the interests of speed no checks are done to validate the structure of your template
> before validation happens, 
> you can use 'is-valid-template' as a precursory check to make sure that the template is
> constructed with valid plists and valid keywords. There is also the possibility that when you try
> to validate a list where you expect a certain structure and get something else you will get a 
> BAD-TEMPLATE-FORM condition where the template is fine but the list is not, just treat this like 
> the validation failed."

<br>
*note it is important that you do not use keywords as the first entry in a list that is passed as 
an argument as this will cause undefined behaviour*
<br>

## Example templates to validate example lists

<br>
These are two example lists and their templates: <br>

```lisp
;;;trivial example:
(defparameter *test-list1* '("key" "abcdeegadfgfsdf"))
(defparameter *test-template1* '((:equal "key") (:type string :maxlen 40)))

;;;complicated example: 
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
->
((:EQUAL "year") (:TYPE INTEGER :OR (96 97 98))
 ((:OR ("keyvals" "time"))
  ((:TYPE STRING :MAXLEN 6 :MINLEN 2) (:TYPE NUMBER :BETWEEN (0 100))
   (:TYPE STRING :MAXLEN 6 :MINLEN 2) (:TYPE NUMBER :BETWEEN (0 100))
   (:TYPE STRING :MAXLEN 6 :MINLEN 2) (:TYPE NUMBER :BETWEEN (0 100)))
  (:TYPE NUMBER :SATISFIES (EVENP ODDP)) (:TYPE NUMBER :SATISFIES (EVENP ODDP))
  (:TYPE LIST :LENGTH 1)
  (:TYPE LIST :CONTENTS (:TYPE NUMBER :SATISFIES (EVENP ODDP)))
  (:TYPE LIST :MINLEN 1 :MAXLEN 5 :CONTENTS
   (:TYPE STRING :MAXLEN 5 :EQUAL "oof"))
  (:TYPE STRING :EQUAL "abc")))

```

## The helper functions
There are two helper functions ``repeat-pattern`` and ``repeat-test``. <br>
The purpose of these functions is to help validate a known length and a known pattern, for example
if you know that the list is going to be 10 elements long you don't want to write out (:type string) 10 times
so you can instead just use one of these functions to do it for you. 
* repeat-pattern - takes two arguments, the first how many times to repeat and the second a list of lists which you want repeated eg. 
```lisp
VALIDATE-LIST>  (repeat-pattern 3 '((:type string :maxlen 6 :minlen 2)
                                    (:type number :between (0 100))))
((:TYPE STRING :MAXLEN 6 :MINLEN 2) (:TYPE NUMBER :BETWEEN (0 100))
 (:TYPE STRING :MAXLEN 6 :MINLEN 2) (:TYPE NUMBER :BETWEEN (0 100))
 (:TYPE STRING :MAXLEN 6 :MINLEN 2) (:TYPE NUMBER :BETWEEN (0 100)))
 ```
* repeat-test - takes two arguments, the first how many times you want to repeat and the second
is a single list containing a pattern. eg. 
```lisp
VALIDATE-LIST> (repeat-test 3 '(:type number :satisfies (#'evenp #'oddp)))
((:TYPE NUMBER :SATISFIES (#'EVENP #'ODDP))
 (:TYPE NUMBER :SATISFIES (#'EVENP #'ODDP))
 (:TYPE NUMBER :SATISFIES (#'EVENP #'ODDP)))
 ```
As you can see in the examples I have used ` and , to control the evaluation of the functions. This 
is probably the easiest way to do this. It is very important to make sure you use ,@ where necessary as it will be a big cause of BAD-TEMPLATE-FORMAT conditions. <br>
 
## Keyword functions

Here are the keys and what they do (each key takes exactly 1 argument):

* :equal - checks if list entry is equalp to arg.
* :type - checks if list entry is of type arg.
* :between - check if list entry is between the numbers in the list arg ie between '(3 5)
* :minlen - checks if list entry is at least length arg.
* :maxlen - checks if list entry between length 0 and arg. 
* :less-than - checks if list entry is a number and is lower than arg.
* :greater-than - checks if list entry is a number and is greater than arg.
* :or - checks if list entry is equalp to any items within the list arg ie '("hello" "help" "house")
* :satisfies - takes a function or list of functions in arg and calls the functions on entry if any returns t, this returns t.

### Special keys

There is one special key which is an extension of :type and :contents which is used with :type list.
* :type list - eg (:type list .. ) in the case that the validator finds a (:type list) it will
not recurse through the next nested list but will instead treat it as an entry and that to the next
key.
* :contents - this is another special keyword that is used to describe the contents of the list used
as an entry. It's argument is a normal plist like (:maxlen 3 :minlen 0 :or ("abc" "def" "oof")) 
and each entry in the list passed to :contents will be validated against the :contents argument.
for example (:maxlen 3 :minlen 0 :or ("abc" "def" "oof")) would be used to validate all the contents 
of ("abc" "abc") with each "abc" being maxlen 3 and greater than 0. For examples of these in action
see `` *test-template16* `` and `` *test-template15* `` in src/tests.lisp

## Compiling templates

If you reckon you will be making heavy use of a template you can compile it using ``compile-template``
This function takes a template as an argument and returns a compiled function, this compiled function can be used to validate a list simply by calling the function with the list you wish to validate as the only argument.<br>
Here is an example: 
```lisp
VALIDATE-LIST> (compile-template *test-template8*)
#<FUNCTION (LAMBDA (LIST)) {10054100CB}>
NIL
NIL
VALIDATE-LIST> (funcall * *test-list8*)
T
VALIDATE-LIST> 
```

Compiling the templates offers quite the speed advantage.<br>
Here are some tests: 

```lisp

V-L-TESTS> (time (dotimes (i 1000000)
                   (validate-list *test-list8* *test-template8*)))
Evaluation took:
  4.643 seconds of real time
  4.658734 seconds of total run time (4.613572 user, 0.045162 system)
  [ Run times consist of 0.221 seconds GC time, and 4.438 seconds non-GC time. ]
  100.34% CPU
  12,035,600,724 processor cycles
  2,415,984,640 bytes consed
  
NIL
V-L-TESTS> (let ((fun (compile-template *test-template8*)))
             (time (dotimes (i 1000000)
                     (funcall fun *test-list8*))))
Evaluation took:
  2.020 seconds of real time
  2.021337 seconds of total run time (2.020652 user, 0.000685 system)
  [ Run times consist of 0.025 seconds GC time, and 1.997 seconds non-GC time. ]
  100.05% CPU
  5,237,150,378 processor cycles
  191,993,040 bytes consed
  
NIL
VALIDATE-LIST> 

```

## Defining your own symbols

If you find you need more functionality you can define your own symbols with the function ``define-key``
<br>
here is the doctsring:<br>
> Takes in a keyword and associates the keyword with the function. The function must accept two
> arguments, the first an entry ie a value in a list wanting to be validated and the second an object
> see any of the other uses of DEFINE-KEY in src/validate-list.lisp to get an idea what your λ should > look like. 
> Here is an example <br>

```lisp

(define-key :n= 
   (lambda (entry arg) 
              (check-type entry number)
              (check-type arg number)
              (= arg entry)))
                  
```

> Now with the new keyword :n= defined this can be used in a template like so where list is '(100)
> and the template is '((:n= 100)). 

Here are a couple of examples taken from Moonbot, my bot for Matrix.

```lisp
(validate-list:define-key :valid-user
  'validate-user)

(defun validate-user (entry x)
  "Given a list and an integer (X), takes the X position from the list and 
checks if it is a valid user"
  (declare (special community connection))
  (let ((user (elt entry x)))
    (if (find user (members community) :test #'string=)
        t 
        (valid-user-p connection (elt entry x)))))

(validate-list:define-key :valid-room
  'validate-room)

(defun validate-room (entry x)
  (declare (special community))
  (find (elt entry x) (rooms community) :test #'string=))

(validate-list:define-key :valid-community
  'validate-community)

(defun validate-community (entry x)
  (declare (special moonbot))
  (find (intern (string-upcase (elt entry x)) :keyword)
        (communities moonbot) :key #'name))
```

## Other

The condition ``unknown-keyword`` is signalled when you put an unknown keyword in your template.
<br>
It has two accessors
* unknown-keyword-keyword - Returns the keyword you tried to use.
* unknown-keyword-message - A description of what went wrong.
<br>

The condition ``bad-template-format`` is signalled when a template is poorly formed.
<br>
It has three accessors<br>
* bad-template-format-template - This is the broken template
* bad-template-format-signaller - The condition that caused ``bad-template-format`` to be signalled this is not always set.
* bad-template-format-message - A descriptive message stating what happened.
<br>
The condition  ``  failed-to-validate  ``  is signalled when validation fails when the structure of the
TEMPLATE is valid for the list passed. 
It has four accessors<br>

* failed-to-validate-key - The keyword that failed to validate.
* failed-to-validate-arg - The argument parsed to the failed keyword.
* failed-to-validate-entry - The entry that failed the validation.
* failed-to-validate-message - A message indicating the issues.

## License

MIT

