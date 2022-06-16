
(defnode instruction-container ()
  ()
  :documentation "Represents a statement that contains a list of instructions inside. Like a for, while, function or class statement.")

(defnode type-node ()
  (type-name)
  :documentation "A class to represent a type in adolstar.")

(defnode type-array-node (type-node)
  (array-type)
  :documentation "Node that represents an array type.")

(defnode type-list-node (type-node)
  (list-type)
  :documentation "Node that represents a list type.")

;;un macro dependiente al DSL pero aqui se puede implementar facil sin necesidad de construirlo.

                                        ;preguntar a fernando

(defnode list-lenght-node ()
  (list-name)
  :documentation "Node that represents a calling to a function 
                   or property that computes the length of the list."
  :ctr-name list-length1)

(defnode-instance (type-void-node ()
  ()
  :documentation "Node that represents the void type.")
    (avoid))

(defnode-instance
    (null-node () ()
               :documentation "Node that represents a NULL literal of a language.")
    (anull))

(defnode-instance
    (true-node () ()
               :documentation "Node that represents a TRUE literal of a language.")
    (atrue))

(defnode-instance
    (false-node () ()
                :documentation "Node that represents a FALSE literal of a language.")
    (afalse))

(defnode-instance
    (self-node () ()
               :documentation "Node that represents a reference to the 
                   instance of the class inside the class.")
    (aself))

(defnode variable-node ()
  (name)
  :documentation "A node to make reference to a variable.")

(defun publish (s)
  (setf (symbol-value s) (make-instance 'variable-node 
					:name s)))

(defnode variable-declaration-node ()
  (variable-type variable-name)
  :documentation "A node for the variable definition."
  :ctr-type macro
  :ctr-name declare-variable
  :ctr-body (let* ((var (gensym)))
              `(let* ((,var (variable-node',variable-name)))
                 (setf (symbol-value ',variable-name) ,var) ,make-ctr)))


(defnode field-access-node ()
  (instance field-name)
  :documentation "Node to access a property in a class."
  :ctr-name fget)

(defnode field-setting-node ()
  (instance field-name value)
  :documentation "Node to modify a field in a class."
  :ctr-name fset)


(defnode array-creation-node ()
  (arr-type arr-length)
  :documentation "Node that represents an array creation."
  :ctr-name new-array)

(defnode array-length-node ()
  (arr-name)
  :documentation "The name of the array.")

(defnode array-access-node ()
  (array-name array-index)
  :documentation "The index that is being accessed."
  :ctr-name ref)

(defnode array-setting-node ()
  (array-name array-index new-value)
  :documentation "A node to represent the modification of the element of an array.")

;ejemplo (new casa (4 2))
(defnode new-object-instance-node ()
  (object-name object-creation-args)
  :documentation "A node for the creation of the object."
  :ctr-name new)


;;tambien pasa lo de los paquetes
(defnode return-node ()
  (return-values)
  :ctr-name return1
  :lambda-key &rest
  :documentation "A return node.")

(defabsnode unary-operator-node ()
  (argument)
  :documentation "A class to represent unary operators.")

(defnode unary-minus-node (unary-operator-node) ())

(defabsnode binary-op-node ()
  (left-hand right-hand operator)
  :documentation "The right hand of the sum.")

(defnode sum-node (binary-op-node) ((operator :initform "+")))
(defnode minus-node (binary-op-node) ())
(defnode multiplication-node (binary-op-node) ())
(defnode division-node (binary-op-node) ())
(defnode assignment-node (binary-op-node) ())
(defnode less-than-node (binary-op-node) ())
(defnode great-than-node (binary-op-node) ())
(defnode less-than-equal-node (binary-op-node) ())
(defnode great-than-equa-node (binary-op-node) ())
(defnode equal-node (binary-op-node) ())
(defnode not-equal-node (binary-op-node) ())


; igual se creo un macro estrano. Pero se resuelve igual.
(defnode min-node () (a b) :ctr-name mina)
(defnode max-node () (a b) :ctr-name maxa)
(defnode sin-node () (x) :ctr-name sina)
(defnode cos-node () (x) :ctr-name cosa)
(defnode tan-node () (x) :ctr-name tana)
(defnode exp-node () (x) :ctr-name expa)
(defnode sqrt-node () (x) :ctr-name sqrta)
(defnode pow-node () (x p) :ctr-name powa)

(defnode if-else-node (instruction-container)
  (boolean-expression then-body else-body)
  :lambda-list (boolean-expression then-body &optional else-body)
  :documentation "Node that represents an if-else statement.")

; (setf f (for-node a 1 1 ((sum-node 23 23 3) (sum-node 1 2 3))))
(defnode for-node (instruction-container)
  (var-name inital-value final-value instructions)
  :documentation "The AST node for the for loop."
  :ctr-type macro
  :ctr-body  `(let*
                  ((,var-name (variable-node ',var-name)))
                (make-instance 'for-node :var-name ',var-name
                               :inital-value ,inital-value
                               :final-value ,final-value
                               :instructions (list ,@instructions))))


(defnode while-node (instruction-container)
  (boolean-expression body))

(defnode slot-definition-node (instruction-container)
  (slot-type slot-name)
  :ctr-name def-slot
  :ctr-type macro
  :lambda-list (slot-name &key slot-type)
  :ctr-body `(prog1 ,make-ctr
  (publish ',slot-name)))

;ejemplo (define-func  funtion-definition-node suma ((int a) (int v)) '(int) '(+ a b))
(defnode function-definition-node (instruction-container)
  (func-name func-params return-types body)
  :documentation "A node to represent the function declaration."
  :ctr-name define-func
  :ctr-type macro
  :lambda-list (ast-function-node func-name func-params return-types &body body)
  :ctr-body (let ((publish-params-name (loop for p in func-params
                                collect `(,(second p)
                                           (make-instance 'variable-node
							  :name ',(second p)))))
        (params-definitions (loop for p in func-params
                               collect `(list ,(first p)
					      ,(second p)))))
    `(let* ((current-return-types (if (list ,@return-types)
					 (list ,@return-types)
					 (list avoid)))
            ,@publish-params-name)
       (progn  (make-instance ',ast-function-node
                        :func-name ',func-name
                        :func-params (list ,@params-definitions)
                        :return-types current-return-types
                        :body (list ,@body))))))


                                        ;ejemplo (setf f (def-func nil suma (('int a) ('int b)) ('int) (sum-node a b "+")))
;luego se puede decir (suma 3 4) y se crea un function-call.
(defnode function-call-node ()
  (instance name params)
  :documentation "A node for function calling."
  :ctr-type macro
  :ctr-name def-func
  :lambda-list (instance func-name func-params return-types &body body)    
  :ctr-body(let* ((params (loop for p in func-params
                             collect `,(second p))))            
             `(progn       
                (defun ,func-name (,@params)	 
                  (make-instance 'function-call-node 
                                 :instance ',instance
                                 :name ',func-name
                                 :params '(,@params)))
                (define-func function-definition-node
                    ,func-name	 
                  ,func-params
                  ,return-types
                   ,@body))))

                                        ;llamada igual que a define-func
;(def-overload overload (('int x) ('double y)) ('adouble) ((sum-node y 'z 5) (sum-node x y 2)))
(defnode operator-overload-node (function-definition-node)
  ()
  :documentation "A node to represent the overload of operators."
  :ctr-name def-overload
  :ctr-type macro
  :ctr-body    `(define-func operator-overload-node
                    ,func-name
                  ,func-params
                  ,return-types
                  ,@body))
                                        ;llamada igual que a las funciones y operator-overload, pero con def-method
;se llama igual que a la funcion ctr de function-definition-node pero como el constructor de este nodo es una funcion sus parametros se avaluaran por lo que pienso que se deben pasar como simbolos.
(defnode method-definition-node (function-definition-node)
  (in-class)
  :documentation "A node to represent the methods definitions."
  :ctr-type macro
  :ctr-name def-method
  :lambda-list (func-name func-params return-types in-class &body body)
  :ctr-body (let* ((params (loop for p in func-params
                    collect `,(second p)))
	 (*ret-method* nil))
    `(progn 
       (defun ,func-name (instance ,@params)
         (make-instance 'function-call-node 
                        :instance instance
                        :name ',func-name
                        :params (list ,@params))
       (setf *ret-method* (define-func method-definition-node
			      ,func-name
			    ,func-params
			    ,return-types
			    ,@body))
       ;(setf (in-class *ret-method*) ,intance)
       *ret-method*))))

;(def-constructor suma1 (('int a) ('int b)) (sum-node 2 3 3) (sum-node 4 5 4))
(defnode class-constructor-node (function-definition-node)
  ()
  :documentation "A node to represent the class constructor definition."
  :ctr-name def-constructor
  :ctr-type macro
  :lambda-list (constructor-name ctr-params &body body)
  :ctr-body  `(define-func class-constructor-node
                  ,constructor-name
                ,ctr-params
		   (',constructor-name)
		   ,@body))

(defnode globals-definition-node ()
  (globals-definitions)
  :ctr-name def-globals
  :lambda-key &rest)


(defnode global-function-definition-node (function-definition-node)
  ()
  :documentation "A node to represent a global function definition."
  :ctr-name def-global-func
  :ctr-body (let* ((params (loop for p in func-params
                    collect `,(second p))))
    `(progn 
       (defun ,func-name (,@params)
         (make-instance 'function-call-node 
                        :instance 'globals
                        :name ',func-name
                        :params (list ,@params)))
       (define-func global-function-definition-node
                       ,func-name
                       ,func-params
                       ,return-types
                       ,@body))))

(defnode global-variable-definition-node ()
  ()
  :documentation "A node to represent a global variable definition.")



(defun find-definitions (def-list definition)
  (let* ((definitions (loop 
                            for i in def-list
                            do (eval i)
                            collect (first i))))
    (loop 
          for d in def-list
          for e in definitions
          when (eq e definition)	
       collect (eval d))))

(defnode class-definition-node (instruction-container)
  (class-name
   class-parent
   slots-definition
   constructor
   operator-overloads
   functions-definition
   methods-definition)
  :ctr-name def-class
  :lambda-list (class-name (&optional parent) constructor &body definitions)
  :ctr-type macro
  :ctr-body  (let* ((array-type-symbol (concat-as-symbols (concat "array-of-" (symbol-name class-name))))
                    (list-type-symbol (concat-as-symbols (concat "list-of-" (symbol-name class-name)))))
               (defparameter current-class class-name)
               `(progn
                  (publish ',class-name)
                  (setf current-class ,class-name)
                  (publish-with-this-value ',array-type-symbol (make-instance 'type-array-node
                                                                              :array-type ',class-name))
                  (publish-with-this-value ',list-type-symbol (make-instance 'type-list-node
                                                                             :list-type ',class-name))
                  (let* ((slots-definition (find-definitions '(,@definitions) 'def-slot))
                         (constructor-definition (first (find-definitions '(,@definitions) 'def-constructor)))
                         (operator-overloads (find-definitions '(,@definitions) 'def-overload))
                         (functions-definition (find-definitions '(,@definitions) 'def-func))
                         (methods-definition (find-definitions '(,@definitions) 'def-method)))	 
                    make-ctr))))


(defnode library-node (instruction-container)
  (name members)
  :ctr-type macro
  :ctr-name def-library
  :ctr-body (let* ((*library* nil))
    `(progn       
       (setf *inside-adolstar* t)
       (publish ',name)
       (setf *library* (make-instance 'library-node 
				      :name ',name
				      :members ,members))
       (setf *inside-adolstar* nil)       
       *library*)))
