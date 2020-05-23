# validate-list

This is an attempt at using templates to validate the contents of a list. 
The idea came because when you parse json from an untrusted source you don't know for certain what 
has been sent, so the idea is that once parsed you can use a template to check if the parsed data 
(now a list) conforms to what you want. The primary function is 
``validate-list-p`` Which takes in two arguments, the list you wish to validate and a template.
Here is the docstring:<br> 
``
"takes in a list that you want to validate, and a template, the template is a list of lists,
each list contains keywords and their values (a list of the keywords is in *valid-syms*). Each list
within the template represents 1 element in the list and is a 'description' of its contents. 
For example given the template '((:equal \"key\") (:type string :maxlen 40)) this could be used
to validate the list '(\"key\" \"abcdeegadfgfsdf\") because as the template says, the first item in 
list is \"key\" and the second according to the template should be of type 'string and no longer
than 40 characters long, which it is not, so this is valid and will return t, if a list fails when
checked against the template then this func returns nil. another example list and template can be 
found in *test-list2* and *test-template2* respectively"
``
<br>
*note it is important that you do not use keywords as the first entry in a list that is passed as 
an argument as this will cause undefined behaviour*
<br>
These are two example lists and their templates: 
```lisp
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
;;;some more complicated examples
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
```

## The helper functions
There are two helper functions ``repeat-pattern`` and ``repeat-test``. <br>
The purpose of these functions is to help validate a known length and a known pattern, for example
if you know that the list is going to be 12 long you don't want to write out (:type string) 10 times
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
is probably the easiest way to do this. <br>

Here are the other keys and what they do (each key takes exactly 1 argument):

* :equal - checks if list entry is equalp to arg.
* :type - checks if list entry is of type arg.
* :between - check if list entry is between the numbers in the list arg ie between '(3 5)
* :minlen - checks if list entry is at least length arg.
* :maxlen - checks if list entry between length 0 and arg. 
* :less-than - checks if list entry is a number and is lower than arg.
* :greater-than - checks if list entry is a number and is greater than arg.
* :or - checks if list entry is equalp to any items within the list arg ie '("hello" "help" "house")
* :satisfies - takes a function or list of functions in arg and calls the functions on entry if any returns t, this returns t. 

## defining your own symbols
If you find you need more functionality you can define your own symbols with the function ``add-new-symbol``
<br>
here is the doctsring:<br>
  "Takes in a symbol and associates the symbol with the function. the function must accept two
arguments, the first an entry ie a value in a list wanting to be validated and the second an object
see any of the definitions of handle-* to get an idea what your lambda should look like. Here is 
an example <br>
```lisp
(add-new-symbol :n= 
                (lambda (entry arg) 
                  (check-type entry number)
                  (check-type arg number)
                  (= arg entry)))
```
Now with the new symbol :n= defined this can be used in a template like so where list is '(100)
and the template is '((:n= 100)). 
"
## other
The condition ``unknown-keyword`` is signalled when you put an unknown keyword in your template.
<br>
It has two accessors
* unknown-keyword-keyword - returns the keyword you tried to use.
* unknown-keyword-message - a description of what went wrong.
<br>
There is another condition that is signalled internally ``failed-to-validate`` if this condition is
signalled when using the normal library functions this is a bug and please report it. 
## License

MIT

