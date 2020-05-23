# validate-list

This is an attempt at using templates to validate the contents of a list. 
The idea came because when you parse json from an untrusted source you don't know for certain what 
has been sent, so the idea is that once parsed you can use a template to check if the parsed data 
(now a list) conforms to what you want. There is only one function and that is 
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

These are two example lists and their templates: 
```lisp
(defparameter *test-list1* '("key" "abcdeegadfgfsdf"))
(defparameter *test-template1* '((:equal "key") (:type string :maxlen 40)))

(defparameter *test-list2*  '("year" 2020 ("country" "USA")))
(defparameter *test-template2* '((:equal "year")(:type integer :between (2100 1900))
                                 ((:or ("cookie" "country"))(:type string :maxlen 50))))
```
Here are the other keys and what they do (each key takes exactly 1 argument):

* :equal - checks if list entry is equalp to arg.
* :type - checks if list entry is of type arg.
* :between - check if list entry is between the numbers in the list arg ie between '(3 5)
* :minlen - checks if list entry is at least length arg.
* :maxlen - checks if list entry between length 0 and arg. 
* :less-than - checks if list entry is a number and is lower than arg.
* :greater-than - checks if list entry is a number and is greater than arg.
* :or - checks if list entry is equalp to any items within the list arg ie '("hello" "help" "house")
* :satisfies - checks if the function in arg returns t when called with the list entry




## License

MIT

