(load "D:/Development/GeneratorMultiLanguage/Core/MyAdol/languages/funcs")

(defun make-slots-for-basic-language (slot-list)
  (loop for (prefix symbol name) in slot-list
        for node-name = (concat-as-symbols prefix "-symbol")
        for initform = (format nil "~a" symbol)
        for initarg = (concat-as-symbols ":" node-name)
        for docstring = (format nil "The symbol used for the ~a." name)
        collecting `(,node-name
                      :accessor ,node-name :initarg ,initarg
                      :initform ,initform
                      :documentation ,docstring)))
;;}}}

;;{{{ macro define-basic-language
(defmacro define-basic-language (operators-list)
  ;;{{{ Documentation
  "Creates the basic-language class with all its operators.
   Syntax:
     (define-basic-language operators-list)
    operators-list is a list where each element is of the form:
                (node-prefix symbol-used operation-name).
     Example:
       (define-basic-language ((plus \"+\" \"addition\") 
                               (minus \"-\" \"substraction\")))
      expands into
        (defclass basic-language ()
          ((plus-symbol :accessor plus-symbol :initarg :plus-symbol
                        :initform \"+\"
                        :documentation \"The symbol used for the addition.\")
           (minus-symbol :accessor minus-symbol :initarg :minus-symbol
                         :initform \"-\"
                         :documentation \"The symbol used for the addition.\"))
          (:documentation \"The basic class for languages with arithmetic opeators.\"))"
  ;;}}}
  `(defclass basic-language ()
     ,(make-slots-for-basic-language (if (symbolp operators-list)
                                         (symbol-value operators-list)
                                         operators-list))
     (:documentation "The basic class for languages with arithmetic operators.")))
;;}}}

;;{{{ list with the slots for basic-language
(defvar *basic-language-binary-operators*
  '((sum  + "addition")
    (minus - "substraction")
    (multiplication * "multiplication")
    (division / "division")
    (assignment = "assignment")
    (less-than < "less than comparison")
    (less-than-equal <= "less than or equal to")
    (great-than > "greater than comparison")
    (great-than-equal >= "greater than or equal to")
    (equal == "equality comparison")
    (not-equal != "inequality comparison"))
  "A list with the operators used by the basic language.")

(define-basic-language *basic-language-binary-operators*)

(defmacro generate-code-for-binary-operators (language format-string list-with-order)
    (let* ((defmethods-definitions
            (loop for (op symbol description) in *basic-language-binary-operators*
                  with left = `(generate-code (left-hand node)
                                              language
                                              nil
                                              0)
                  with right = `(generate-code (right-hand node)
                                               language
                                               nil
                                               0)
                  for node-name = (concat-as-symbols op "-node")
                  for node-symbol = (concat-as-symbols op "-symbol")
                  for operator = `(,node-symbol language)
                  for actual-order = (loop for elt in list-with-order
                                           collecting (cond
                                                        ((eq elt 'operator) operator)
                                                        ((eq elt 'left) left)
                                                        ((eq elt 'right) right)))
                  for docstring = (format nil "Generate infix code for node ~a in language ~a."
                                          node-name
                                          language)
                  collecting `(defmethod generate-code ((node ,node-name)
                                                        (language ,language)
                                                        stream
                                                        indentation)
                                ,docstring
                                (format stream ,format-string ,@actual-order)))))
     ;;}}}
     `(progn
        ,@defmethods-definitions)))
    
(gcode (eql nil) basic-language (""))

(defnode-instance (list-of-instructions ()
  ()
  :documentation "A class that represents a list of instructions.") (list-of-instructions))

(defnode-instance (list-of-function-args () ()
  :documentation "A class that represents a list of arguments for a function.") (list-of-funtion-args))

(defnode-instance (list-of-method-args (list-of-function-args) ()
  :documentation "A class that represents a list of arguments for a method definition.") (list-of-method-args))


