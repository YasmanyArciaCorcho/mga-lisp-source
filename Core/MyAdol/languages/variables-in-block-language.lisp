
;;;{{{ Language definition

(defnode variables-in-block-language ()
  ()
  (:documentation "A variable in which the variables should be declared in a given block in the code. Examples of these languages are Pascal and Lisp."))


(defvar *list-with-variable-declarations* nil
  "A list with the variable declarations found in the definition of a function.")


(defmethod generate-code :before ((node variable-declaration-node)
                                  (language variables-in-block-language)
                                  stream indentation)
  (declare (ignore stream indentation))
  (declare (special *list-with-variable-declarations*))
  (push node *list-with-variable-declarations*))

(defmethod generate-code :around ((node function-definition-node)
                                  (language variables-in-block-language)
                                  stream indentation)
  (let* ((*list-with-variable-declarations* nil))
   (declare (special *list-with-variable-declarations*))
   (call-next-method node language stream indentation)))
