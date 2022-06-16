(load "macro-core.lisp")
(defclass language-with-arithmetic-operators ()
  ()
  (:documentation "A language with sum and product arithmetic operators"))
(defclass prefix-operator-language (language-with-arithmetic-operators)
  ()
  (:documentation "A prefix operator language"))

(defclass infix-operator-language (language-with-arithmetic-operators)
  ()
  (:documentation "A infix operator language"))

(defclass common-language()
  ()   (:documentation "A class to represent the commom language"))

(defclass common-c-language(common-language infix-operator-language)
  ()   (:documentation "A class to represent the C family language")) 

(defclass java-language(common-c-language)
  ()(:documentation "A class to represent the Java language"))

(defclass lisp-language (prefix-operator-language)
  ()(:documentation "A class to represent the Lisp language"))
(class-instance lisp lisp-language)

(defclass  binary-op ()
  ((left-hand :accessor left-hand :initarg :left-hand)
   (right-hand :accessor right-hand :initarg :right-hand)))
(defclass assignment-class (binary-op)())
(defclass sum-class(binary-op)())
(defclass minus-class(binary-op)())
(defclass mult-class(binary-op)())
(defclass div-class(binary-op)())

(defun binary-op(def-class left-hand right-hand)
  (make-instance def-class :left-hand left-hand :right-hand right-hand))
(defun assignmet-class(left-hand right-hand)
  (binary-op 'assignment-class left-hand right-hand))
(defun sum-class (left-hand right-hand)
  (binary-op 'sum-class left-hand right-hand))
(defun minus-class(left-hand right-hand)
  (binary-op 'minus-class left-hand right-hand))
(defun mult-class(left-hand right-hand)
  (binary-op 'mult-class left-hand right-hand))
(defun div-class(left-hand right-hand)
  (binary-op 'div-class left-hand right-hand))
					;----------------------------------------------------------------------------
(defclass statement-class()
  ((code :accessor code :initarg :code)))

(defclass conditional-class (statement-class)
  ((condition-slot :accessor condition-slot :initarg :condition-slot)))

(defclass for-class (conditional-class)
  ((var-name :accessor var-name :initarg :var-name)
   (initial-value :accessor initial-value :initarg :initial-value)
   (evolution :accessor evolution :initarg :evolution)))

(defun for-class (var-name initial-value condition evolution code)
  (make-instance 'for-class :var-name var-name :initial-value initial-value :condition-slot condition :evolution evolution :code code))

(defclass while-class (conditional-class)())
(defclass if-class(conditional-class)())

;Lenguajes en cuales los  metodos retornan.
;;-->return-class: codigo que se desee retornar.
(defclass return-class (statement-class)())
(defun return-class (code)
  (make-instance 'return-class :code code))

(defclass variable-declaration()
  ((name :accessor name :initarg :name)))
(defun variable-declaration(name)
  (make-instance 'variable-declaration :name name))

;;-->parameters: lista de tuplas (name type).
(defclass method-class(statement-class)
  ((name :accessor name :initarg :name)
   (parameters :accessor parameters :initarg :parameters)))
(defun method-class(name parameters code)
  (make-instance 'method-class :name name :parameters parameters :code code))
(defmethod print-object((obj method-class) stream)
  (format stream "(method-class (name ~a) (parameters ~a) (code ~a))"
	  (name obj)
	  (parameters obj)
	  (code obj)))

;;-->return-type: tipo de retorno del metodo. 
(defclass method-type-class(method-class)
  ((return-type :accessor return-type :initarg :return-type)))
(defun method-type-class (name return-type parameters code)
  (make-instance 'method-type-class :name name :return-type return-type :parameters parameters :code code))

;;Para invocar(llamar,call) un metodo.
;;-->nombre:nombre del metodo a invocar.
;;-->parameters parametros para invocar el metodo.
(defclass method-invoke-class(method-class)())
(defun method-invoke-class (name parameters)
  (make-instance 'method-invoke-class :name name :parameters parameters))

;;Clase para definir la sobrecarga de los operadores en los lenguajes que lo soporten.
;;-->operator-in:operador al cual se le realizara la sobrecarga.
(defclass op-overload()
  ((operator :accessor operator-in :initarg :operator-in)))

(defun op-overload(operator-in)
  (make-instance 'op-overload :operator-in operator-in))

;;Propiedades para las clases.
;;-->nombre de la propiedad.
(defclass property-lang ()
  ((name :accessor name :initarg :name)))
(defun property-lang(name)
  (make-instance 'property-lang :name name))

;;Definicion de una clase.
;;--> properties: son las propiedades de las clases,deben de ser de tipo property-lang.
(defclass def-class()
  ((name :accessor name :initarg :name)
   (properties :accessor properties :initarg :properties)))
(defun def-class(name properties)
  (make-instance 'def-class :name name :properties properties))

;;Clases con metodos incuidos
;;-->properties: las propiedades de las clases,deben de ser de tipo property-lang.
;;-->methods: metodos de la clase,deben de ser de tipo method-class.
(defclass def-class-with-methods( def-class)
  ((methods :accessor methods :initarg :methods)))
(defun  def-with-method-class(name properties methods)
  (make-instance ' def-with-method-class :name name :properties properties :methods methods))
;;Una implementacion para los constructores de las clases
;;-->los constructores son metodos que no retornan nada(method-class)
;;-->(defclass ctr-def-class(method-class)())
;;-->  (defun ctr-def-class(name-class parameters code)
;;-->    (make-instance 'ctr-def-class :name name-class :parameters parameters :code code))

;;Invoca la creacion de una clase.
;;-->properties :lista de dos elementos (nombre-propiedad valor)
(defclass invoke-class(def-class)())
(defun invoke-class(name properties)
  (make-instance 'invoke-class :name name :properties properties))

;;Acceder a las propiedades de las clases.
;;-->name-class: nombre de la clase en la que se desea el valor de la propiedad.
;;-->name:nombre de la propiedad en la que se desea conocer el valor.
(defclass property-value(property-lang)
  ((name-class :accessor name-class :initarg :name-class)))
(defun property-value(name-class name)
  (make-instance 'property-value :name name :name-class name-class))

(defgeneric generate-code (obj lang stream))

(defmethod generate-code ((obj t) lang stream)
  (declare (ignore lang))
  (format stream "~a" obj))
;;------------------------------- assignmet--------------------------------------------------
(defmethod generate-code ((obj assignment-class) (lang common-language) stream)
  (format stream "~a = ~a"
	  (generate-code(left-hand obj) lang nil)
	  (generate-code(right-hand obj) lang nil)))

(defmethod generate-code((obj assignment-class)(lang lisp-language) stream)
  (format stream "(setf ~a ~a)"
	  (generate-code(left-hand obj) lang nil)
	  (generate-code (right-hand obj) lang nil)))
;;--------------------------------sum-------------------------------------------
(defmethod generate-code((obj sum-class) (lang common-language) stream)
  (format stream "~a + ~a"
	  (generate-code(left-hand obj) lang nil)
	  (generate-code(right-hand obj) lang nil)))

(defmethod generate-code((obj sum-class) (lang lisp-language) stream)
  (format stream "(+ ~a ~a)"
	  (generate-code(left-hand obj) lang nil)
	  (generate-code(right-hand obj) lang nil)))
;;-----------------------------minus--------------------------------------------
(defmethod generate-code((obj minus-class) (lang common-language) stream)
  (format stream "~a - ~a"
	  (generate-code (left-hand obj) lang nil)
	  (generate-code ( right-hand obj)lang nil)))
(defmethod generate-code((obj minus-class) (lang lisp-language) stream)
  (format stream "(- ~a ~a)"
	  (generate-code (left-hand obj) lang nil)
	  (generate-code (right-hand obj) lang nil)))
;;--------------------------------mult---------------------------------------
(defmethod generate-code((obj mult-class) (lang common-language) stream)
  (format stream "~a * ~a"
	  (generate-code(left-hand obj) lang nil)
	  (generate-code(right-hand obj) lang nil)))

(defmethod generate-code((obj mult-class) (lang lisp-language) stream)
  (format stream "(* ~a ~a)"
	  (generate-code(left-hand obj) lang nil)
	  (generate-code(right-hand obj) lang nil)))
;;---------------------div------------------------------------------------------
(defmethod generate-code((obj div-class) (lang common-language) stream)
  (format stream "~a / ~a"
	  (generate-code(left-hand obj) lang nil)
	  (generate-code(right-hand obj) lang nil)))
(defmethod generate-code((obj div-class) (lang lisp-language) stream)
  (format stream "(/ ~a ~a)"
	  (generate-code(left-hand obj) lang nil)
	  (generate-code(right-hand obj) lang nil)))
;;----------------------------- for---------------------------------------------
(defmethod generate-code((obj for-class) (lang common-c-language) stream)
  (format stream  "for(int ~a = ~a; ~a ; ~a){~a}"
	  (generate-code (var-name obj) lang nil)
	  (generate-code (initial-value obj) lang nil)
	  (generate-code (condition-slot obj) lang nil)
	  (generate-code (evolution obj) lang nil)
	  (generate-code (code obj) lang nil)))

(defmethod generate-code((obj for-class) (lang lisp-language) stream)
    (format stream "(DO ((~a ~a ~a))((not ~a)) ~a)"
	    (generate-code (var-name obj) lang nil)
	    (generate-code (initial-value obj) lang nil)
	    (generate-code (evolution obj) lang nil)
	    (generate-code (condition-slot obj) lang nil)
	    (generate-code (code obj) lang nil)))

;;---------------------list-----------------------------------------------------------
(defmethod generate-code((obj list) lang stream)
  (format stream "~{ ~a~%~} "
	  (loop for elem in obj
		collecting (with-output-to-string (s)(generate-code elem lang s) s))))

(defun process-list (list lang stream separator)
  (let* ((format-string (format nil "~~{~~a~a~~}" separator)))
    
    (format stream format-string
	    (mapcar (lambda (x) (generate-code x lang nil)) list)
	    )))

;;-----------------------method------------------------------------------------------

(defmethod generate-code((obj method-class) (lang common-c-language) stream)
  (format stream "public void ~a (~a) ~%{~%~a~%}"
	  (name obj)
	  (print-common-c-parameters (parameters obj) nil)
	  (generate-code (code obj) lang nil)))

(defmethod generate-code((obj method-type-class) (lang common-c-language) stream)
  (format stream "public ~a ~a (~a) ~%{~%~a~%}"
	  (return-type obj)
	  (name obj)
	  (print-common-c-parameters (parameters obj) nil)
	  (generate-code (code obj) lang nil)))

;;todo:(como convertir de simbolo a string) acordar con lo nuevo de los macros si hago un macro para que el transpaso de los parametros no sea tan feo como es
;;ahora con los string.
;;ejemplo de parametros (("int" "edad") ("string" "nombre"))
(defun print-common-c-parameters (parameters stream)
(format stream "~a ~{~a~}"(concatenate 'string (string (first (car parameters))) " " (string (second (car parameters))))
	(loop for (type name) in (cdr parameters)
	      collecting (concatenate 'string "," (string type) " "  (string name)))))


;;----------------------return--------------------------------------------------------
(defmethod generate-code((obj return-class) (lang common-c-language) stream)
  (format stream "return ~a"
	  (generate-code (code obj) lang nil)))

  
;;------------------------------------property value-----------------------------
(defmethod generate-code((obj property-value) (lang common-c-language) stream)
  (format stream "~a.~a"
	  (name-class obj)
	  (generate-code (name obj) lang nil)))
;;---------------------------------invoke method-------------------------------------
(defmethod generate-code((obj method-invoke-class) (lang common-c-language) stream)
  (format stream " ~a (~a~{~a~})"
	  (name obj)
	  (car (parameters obj))
	  (loop for name in (cdr (parameters obj))
	      collecting (concatenate 'string "," name))))
