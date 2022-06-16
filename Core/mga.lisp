
;; los diccionarios para las propiedades de las clases:
(defparameter slots-dict()) ;alamcena toda la inforamcion de una clase. Este diccionario es una lista de listas.
                                        ;Cada elemento del diccionario hace referencia a la informacion de una clase declarada con defnode. El primer elemento de una lista es el nombre
                                        ;de la clase; el segundo, una lista con la herencia y el resto son todos los nuevos slots que define el tipo de la clase. No estan todos los slots almacenados porque se pueden recobrar a partir de sus super clases.
(defparameter initarg-dict()); en este diccionario se alamcenan los initarg de las clases, es una lista de listas.
                                        ;El primer parametro es el nombre de la clases;el y el resto son los valores de sus ititarg.
(defparameter accessor-dict());igual que el diccionario initarg-dic,pero almacena el accessor.
(defparameter ctr-func-name-dict ())
;;;ver la herencia (si el usiario debe ponerla o no).
;;;print-obj-creo un print standard para todas las clases.
;;;instruction-name es para el usuario,pero para el desarrollador es equivalente a class-name
;;(defmacro generate-instruction (instruction-name  args)
;; `(create-class-data ,instruction-name () (,(format nil "~~{~~a~~}") ,args) ,args))
;;ejemplo -------------------> (generate-instruction suma "~~a + ~~b" A B)
(defun update-dictionary-properties (dict instruction-name inherit args)
  (setf dict (insert-class-data dict instruction-name inherit args))
  dict)


;;region para el trabajo con las llaves del print object
(defparameter print-object-left-key "(")
(defparameter print-object-right-key ")")

(defun update-left-key (new-key)
  (setf print-object-left-key new-key))
(defun update-right-key (new-key)
  (setf print-object-right-key new-key))
(defun update-key (l-key r-key)
  (update-left-key l-key)
  (update-right-key r-key))
;;endregion

;;chek-parameters-func:convierte los simbolos que pertenecen a los parametros en
  ;;simbolos cuyo valor es el nombre del simbolo;para que la funcion pueda guardar su valor. 
  (defun chek-parameters-func(parameters code)
    (let* ((result ()))
  (loop for element in code
	doing
	(if
	    (member element parameters)
	    (setf result (append result (list(read-from-string (format nil "'~a" element)))))
	  (if
	      (atom element)
	      (setf result (append result (list element)))
	    (setf result (append result (list (chek-parameters-func parameters element)))))))
  result))

;;convierte los parametros de una funcion de un lenguaje de tipado estatico a
;;los parametros de una funcion en lenguajes de tipado dinamico.
;;-->ejemplo ((int a) (int b))-->(a b)
;; para pasarselos como argumetos a la funcion chek-parameters-func
(defun remove-type-to-type-parameters(parameters)
  (loop for (type name) in parameters
	collect  name))

;;normalize-code:
;;-->parameters: son los parametros de la funcion para la cual se dessea generar el codigo code en el cual se quiere tener el valor de los simbolos como su nombre.
(defun normalize-code (parameters code)
  (if (not (atom (car parameters)))
      (setf parameters (remove-type-to-type-parameters parameters)))
    (chek-parameters-func parameters code))
  

(defun standard-print-object (name args separator funtion)
  (let* ((format-string (format nil "~a ~a ~~{~~a~a~~} ~a" print-object-left-key name separator print-object-right-key)))
    (format nil format-string
	    (loop for arg in args
		  collecting  (list arg funtion)))))
;;crea una  funcion para crear funciones:
;;por ejemplo (g-def-void-func defineFun).luego podemos hacer algo en nuestro lenguaje como (definefun suma (x y z) (+ x (+ y z)))
;;ver que en la generacion de la funcion todos los nodos internos deben estar previamente definidos en el lenguaje para poder exportarlos.
;;g-def-void-func solo es para funciones que no retornan.
;; en csharp serian los metodos void,
;; en python son metodos que no retornan
;; en lisp como siempre se retorna algo hago incapie en que son funciones que no tienen un return o return-from en su cuerpo.
;;alternativa y me gusta porque utilizo las clases basicas implementadas.
;;no asustarme porque puedo recorrer los parametros aunqe sean simbolos normal.
;;->ejemplo de entrada con tipado estatico
;;-->(generate-def-void-func-c def): indica que se quiere definir funciones con la funcion def 
;;----------------------------------------------
;;-->(def suma ((int a)(int b)) (sum-class a b))
;;-> ejemplo de entrada con tipado dinamico
;;-->(def suma ( a  b) (sum-class a b))
(defmacro g-def-void-func (funtion-name)
  `(defmacro ,funtion-name (name parameters code)
     (setf code-aux (normalize-code parameters code))
     `(method-class ',name ',parameters ,code-aux)))

;; dado una lista de elementos, si el elemento es una lista guarda el first
;; o si es un atom tambien lo guarda
;; ejemplo '((1 2 3 4) (5 6) (7) 8)----> (1 5 7 8)
(defun only-first-element-in-list (args_trio)
  (loop for x in args_trio
     collect (if (atom x)
                 x
                 (car x))))

(defmacro defabsnode (class-name inherit (&rest args) &key (documentation "No any documentation for this node.")
                                            (ctr-type 'abstract ctr-type-p)
                                            (lambda-list nil lambda-list-p)
                                            (ctr-body nil ctr-body-p)
                                            (string-obj nil string-obj-p))
  `(defnode ,class-name ,inherit ,args :documentation ,documentation :ctr-type ,ctr-type :string-obj ,string-obj))
;;standard print object
;;un ejemplo de print-object :string-obj ("string" slot1 slot2 ... slotn)
;; el rest de los argumentos debe de tener el siguiente formato
;; es una lista con los posibles elementos
;;                                     : si se quiere que se genere todo solo el nombre del slots
;;                                     : si se quiere modificar y poner por defecto es una lista como la siguiente
;;                                     (nombre-slot  :accesor nombre-para-accesor :initarg :nombre_initarg :initform nombre initform :documentation "doc") el orden no es relevante
;; ------------------super importante el initarg se delcara de la siguiente forma ":initarg :nombre_initarg"
(defmacro defnode (class-name inherit (&rest args) &key (documentation "No any documentation for this node.")
                                                     (ctr-type 'function ctr-type-p)
                                                     (ctr-name class-name)
                                                     (lambda-list nil lambda-list-p)
                                                     (lambda-key nil lambda-key-p)
                                                     (ctr-body  'make-ctr ctr-body-p) ;para indicar donde se debe llamar al make-instance, se pone la palabra make-ctr
                                                     (before nil before-p);;before de la funcion constructora
                                                     (string-obj nil string-obj-p))
  (let* ((filter-args (only-first-element-in-list args))
         (slots-inherit (get-all-slots-from-inherit inherit slots-dict))
         (filter-args (set-difference filter-args slots-inherit))
         (slots-def (loop for data in args
                       collect (if (atom data)
                                   (list data ':accessor data ':initarg (make-keyword ":~a" data) ':allocation ':instance)
                                   (let* ((name (first data)))                    
                                     (destructuring-bind (slot-name
                                                          &key (accessor name)
                                                          (initarg (make-keyword ":~a" name))
                                                          (documentation "empty doc")
                                                          (initform nil)
                                                          (allocation (make-keyword ":~a" "instance"))
                                                          (optional nil));optional es una lista para modificadores que se quieran agregar por ejemplo
                                                     ;;en la definicion del slots x (x :accesor superx :optional (:read 34))
                                         data
                                       (append (list slot-name :accessor accessor
                                                     :initarg initarg :allocation allocation
                                                     ;esto lo cambie en el commit remove conditionl
                                                     :initform initform  :documentation documentation)
                                               optional)))))))    ;;;recordar poner que sea cualquier modificador, es solo pasarselo por parametor y append a la lista de slots-def
    (let* ((accessor-def (loop for data in slots-def
                            collecting (list (car data) (third data))))
           (initarg-def (loop for data in slots-def
                           collecting  (list (car data) (make-keyword "~a" (fifth data))))))
      (setf slots-dict (update-dictionary-properties slots-dict class-name inherit filter-args))
      (setf accessor-dict (update-dictionary-properties accessor-dict class-name inherit accessor-def))
      (setf initarg-dict (update-dictionary-properties initarg-dict class-name inherit initarg-def)))
    (let* ((all-accessor (get-all-properties class-name accessor-dict))
           (all-initarg (get-all-properties class-name initarg-dict))
           ;;El comentario siguiente es valido solo para las variables que se definen a continuacion.         
           ;;Esta region del let* es solo para asignar valores por defecto a los parametros con el modificador &key activo.
           (ctr-type (if (eq ctr-type 'abstract)
                         'abstract
                         (if (eq ctr-type 'macro)
                             'defmacro
                             (if (eq ctr-type 'function)
                                 'defun
                                 'defmethod))))
           (parameters (if (not lambda-list-p)
                           (if (not lambda-key-p)
                               all-initarg
                               (append (list lambda-key) all-initarg))
                                  lambda-list))
           (new-ctr-body (let* ((body-aux (if (eq ctr-type 'defun)
                                              `(make-instance ',class-name
                                                              ,@(loop for initarg-name in all-initarg
                                                                                collecting (make-keyword ":~a" initarg-name)
                                                                   collecting (make-keyword "~a" initarg-name)))
                                              (if  (eq ctr-type 'defmacro)
                                                    `(make-instance ',class-name ,@(loop for initarg-name in all-initarg
                                                                       collecting (make-keyword ":~a" initarg-name)
                                                                         collecting (read-from-string (concatenate 'string "`," (symbol-name initarg-name)))))))))
                           `(symbol-macrolet ((make-ctr ,body-aux)) ,ctr-body)))
           (n-string-obj (if (and string-obj-p
                                  (not (eq nil string-obj)))
                             string-obj
                             (append (list (standard-print-object class-name all-accessor " "  "~a")) all-accessor)))
           (ctr-func (if (eq ctr-type 'abstract)
                         nil
                         `(,ctr-type ,ctr-name ,parameters ,new-ctr-body))))
      (setf ctr-func-name-dict (append (list (list class-name ctr-name))  (remove-class-data class-name ctr-func-name-dict)))
      `(create-class-data ,class-name ,documentation ,inherit ,slots-def ,ctr-func ,n-string-obj))))

;;;Macro para crear y  instanciar objetos.
;;;-->name-istance: nombre de la instancia.
;;-->class-name: clase tipo de la nueva instancia.
;;--> data tiene el formato del lambda-list de defnode
;; con este macro se crea toda la definicion de un nodo y se instancia otro
(defmacro defnode-instance ((class-name inherit (&rest args) &key (documentation "No any documentation for this node.")
                                            (ctr-type 'function ctr-type-p)
                                            (ctr-name nil)
                                            (lambda-list nil lambda-list-p)
                                            (ctr-body  nil ctr-body-p)
                                            (before nil before-p)
                                            string-obj) (&rest value))
  `(prog2(defnode ,class-name ,inherit ,args :ctr-name ,(if ctr-name
                                                            ctr-name
                                                            class-name)  :documentation ,documentation :ctr-type ,ctr-type)
    (defparameter ,(first value) (,class-name ,@(cdr value)))))
  


(defun get-all-ctr-func-name (dict)
    (loop for (class-name func-name) in dict
         collecting func-name))


(defmacro dsl-syntax-highlighting (dsl-name 
                                        (&key
                                         keywords
                                         builtin-face
                                         comment-delimiter-face 
                                         comment-face
                                         constant-face
                                         doc-face
                                         function-name-face
                                         negation-char-face
                                         prepocessor-face
                                         regexp-grouping-backslash 
                                         regexp-grouping-construct 
                                         string-face
                                         type-face
                                         variable-name-face
                                         warning-face)
                                &optional (file ""))
  (if (equal keywords nil)
      (setf keywords (get-all-ctr-func-name ctr-func-name-dict)))
  `(language-syntax-highlighting ,dsl-name (:keywords ,keywords
                                                    :builtin-face ,builtin-face
                                                    :comment-delimiter-face ,comment-delimiter-face 
                                                    :comment-face ,comment-face
                                                    :constant-face ,constant-face
                                                    :doc-face ,doc-face
                                                    :function-name-face ,function-name-face
                                                    :negation-char-face ,negation-char-face
                                                    :prepocessor-face ,prepocessor-face
                                                    :regexp-grouping-backslash ,regexp-grouping-backslash 
                                                    :regexp-grouping-construct ,regexp-grouping-construct 
                                                    :string-face ,string-face
                                                    :type-face ,type-face
                                                    :variable-name-face ,variable-name-face
                                                    :warning-face ,warning-face)
                                ,file))


;; en estos ejemplos hay que cambiar el orden de los par'ametros.
; ejemplo de uso del gcode
;(defnode persona () () (nombre edad))
;(defnode ch () () ())
;(class-instance c ch)
;(gcode persona ch ("persona nombre ~a edad ~a" (print 4) " nombre de nuevo ~a") ((nombre edad) (nombre)))


; (class-instance (ch (indentable-languaje) () ()) (c))
;(gcode person ch ("nombre ~a" `(,(FORMAT T "hola")) "edad ~a")  (nombre edad)) -- el codigo del format t hola se ra evaluado en la espancion del macvor
;(gcode person ch ("nombre ~a" (FORMAT T "hola") "edad ~a")  --  el codigo no se evalua, se evalua en un llamado a la funcion generate-code


;(defnode variable () (value))
;(defnode def-var () (var-name value) :ctr-type defmacro :ctr-body (let* ((var (gensym)))`(let* ((,var (variable ,value))) (setf (symbol-value ',var-name) ,var) ,make-ctr)))

