(defnode indentable-language () 
  ((indent-increment :accessor ind-increment :initarg :ind-increment
                  :initform 2
                  :documentation "How many spaces to 
                  add with each indentation increment")
   (indentation :accessor indentation :initarg :indentation
           :initform 0
           :documentation "The current indentation for the language"))
  :documentation "An language indentable")


;;las siguientes funciones generan cadedas de indentacion
;;son llamadas por el before de los generate-code para generar un string indentado.
(defgeneric make-ind-str (obj)
  (:documentation "A generic function create an indentation string."))

(defmethod make-ind-str ((obj number))
  "Returns a string of length n formed by spaces"
  (make-string obj :initial-element #\Space))

(defmethod make-ind-str ((obj indentable-language))
  "Returns a string formed by (indentation obj) spaces"
  (make-string (indentation obj) :initial-element #\Space))


;Funciones para el trabajo con la indentacion de los lenguajes
                                        ; estas funciones permiten icrementar y decrementar la indentacion de los lenguajes.
(defmacro increment-indentation (instance &optional amount)
  `(let* ((amount (or ,amount (ind-increment ,instance))))
     (incf (indentation ,instance) amount)))

(defmacro decrement-indentation (instance &optional amount)
  `(let* ((amount (or ,amount (ind-increment ,instance))))
     (decf (indentation ,instance) amount)))

;; a macro to deal with indentation
(defmacro with-increased-indentation ((lang &optional amount) &body body)
  (let* ((current-indentation (or amount `(ind-increment ,lang))))
    `(let* ((actual-ind ,current-indentation))
       (increment-indentation ,lang actual-ind)
       ,@body
       (decrement-indentation ,lang actual-ind))))

;; macro para identificar un nodo como indentable.
;; Cuando un nodo es indentable la generacion de codigo se vera modificada por la indentacion del lenguaje, concatenando los simbolos de indentacion con la generacion por defecto del nodo en el lenguaje de salida.

 (defmacro mark-node-as-indentable (class-name indentable-language)
  `(defmethod generate-code :before ((node ,class-name) 
                                     (lang ,indentable-language)
                                     stream)
              (format stream  (make-ind-str lang))))


  (defmethod generate-code ((obj list) (lang indentable-language) stream)
  (let* ((list (mapcar (lambda (x) (with-output-to-string (s)
                                     (generate-code x lang s)))
                       obj)))
    (format stream "~{~a~%~}"
            list)))


