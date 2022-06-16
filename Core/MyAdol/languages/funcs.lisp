
;;;{{{ Coercion to string
(defgeneric coerce-to-string (thing)
  (:documentation "Returns it's argument coerced to string (if possible)."))

(defmethod coerce-to-string ((thing number))
  "Returns it's argument coerced to string (if possible)."
  (write-to-string thing))

(defmethod coerce-to-string ((thing symbol))
  "Returns it's argument coerced to string (if possible)."
  (symbol-name thing))

(defmethod coerce-to-string ((thing string))
  "Returns it's argument coerced to string (if possible)."
  thing)
;;;}}}

;;;{{{ Symbols concatenation
(defun concat-as-symbols (&rest arguments)
  "Returns the concatenation of its arguments coerced to string.
   Syntax:
     (concat-as-symbols arg1 arg2 ... argn)
   argi are arguments than can be coerced to string using the
        generic function coerce-to-string."
  (loop for  argument in (rest arguments)
        with result = (coerce-to-string (first arguments))
        doing (cl:setf result (concatenate 'string result
					   (coerce-to-string argument)))
        finally (return (read-from-string result))))
;;;}}}

;;;{{{ concat
(defun concat (&rest strings)
    ;;{{{ Documentation
    "Concatenates the list of strings.
     Syntax:
       (concat &rest strings)
        strings is a list of strings."
  ;;}}}
    (format nil "~{~a~}" strings))
;;;}}}

;;;{{{ mapconcat
(defun mapconcat (list &key (separator "") (start "") (end ""))
  ;;{{{ Documentation
  "Mapcars the list concatenating the results as a string.  If the
list passed is nil, returns nil.
     Syntax:
       (mapconcat list &key (separator \"\") (start \"\") (end \"\"))
      list       is a list of string or symbols that will be concatenated.
      separator  is a string to use to separate the concatenated elements.
      start      is a string to concatenate to the start of the result.
      end        is a string to add to the end of the result.
The elements in the list can be anything can be coerced-to-string using the
function coerce-to-string.
`separator', `start', and `end' can contain format directives.
    Example:
      (mapconcat '(hello \"beautiful\" world) :separator \"~%\" :end \"!\")
    returns:
\"HELLO
beautiful
WORLD!\"

If the empty string is a member of the list, it is ignored and it is not
concatenated.
    Example:
      (mapconcat '(hello \"beautiful\" \"\" world) :separator \"~%\" :end \"!\")
    returns:
\"HELLO
beautiful
WORLD!\""
  ;;}}}
  (cl:if (null list) nil
      ;; else
      (let* ((result (coerce-to-string (first list)))
             (separator-string (format nil "~a" separator))
             (start-string (format nil "~a" start))
             (end-string (format nil "~a" end)))
        (loop for x in (rest list)
              when (not (string= (coerce-to-string x) ""))
                do (cl:setf result (concatenate 'string
                                             result
                                             (format nil separator-string)
                                             (coerce-to-string x))))
        (cl:setf result (concatenate 'string
                                     (format nil start-string)
                                     result
                                     (format nil end-string)))
        ;; return result
        result)))
;;;}}}

;;;{{{ mapconcatcar
(defun mapconcatcar (function list &key
                     (start "")
                     (separator "")
                     (end ""))
  ;;{{{ Documentation
  "This function is the result of a pattern arising from using many
times mapconcat with a mapcar inside to `preprocess' the list that
should be mapconcated.
  
Mapcars the list concatenating the results as a string.  If the
list passed is nil, returns nil.
     Syntax:
       (mapconcat function list &key (separator \"\") (start \"\") (end \"\"))
      function   is a function that can be funcalled.
      list       is a list of string or symbols that will be concatenated.
      separator  is a string to use to separate the concatenated elements.
      start      is a string to concatenate to the start of the result.
      end        is a string to add to the end of the result.
The elements in the list can be anything that can be coerced-to-string using the
function coerce-to-string.
`separator', `start', and `end' may contain format directives.
    Example:
      (mapconcatcar (lambda (x) (car x)) 
                   '((hello 1) (2 \"beautiful\") (eureka world)) 
                    :separator \"~%\"
                    :end \"!\")
    returns:
\"HELLO
2
EUREKA!\"

If the empty string is a member of the list, it is ignored and it is not
concatenated.
    Example:
      (mapconcatcar (lambda (x) x) 
                    '(hello \"beautiful\" \"\" world)
                    :separator \"~%\"
                    :end \"!\")
    returns:
\"HELLO
beautiful
WORLD!\""
  ;;}}}
  (mapconcat (mapcar function list)
             :start start 
             :separator separator
             :end end))
;;;}}}

;;;{{{ indentation-string 
(defun indentation-string (n)
  "Returns a string of n spaces."
  (make-string n :initial-element (code-char 32)))
;;;}}}

;;;{{{ print-new-line
(defun print-new-line (stream)
  (format stream "~%"))
;;;}}}


;;;{{{ generate-library
(defun generate-library (output-file-name library-node language indentation)
  (with-open-file (s (merge-pathnames output-file-name) :direction :output :if-exists :supersede)
    (generate-code library-node language s indentation)))
;;;}}}
