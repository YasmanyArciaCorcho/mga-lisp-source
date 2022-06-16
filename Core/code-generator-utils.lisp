
(defmacro gcodenil (slot-name)
  "More than a macro this is just an abbreviation."
  `(with-output-to-string (s) (generate-code (,slot-name node) lang s)))

(defmacro gcodenil-exp (expresion)
  `(with-output-to-string (s) (generate-code ,expresion lang s)))

                                        ;el ultimo paratetro de gcode es un &arg, donde cada arg es una lista.
                                        ; los elementos de esa lista pueden ser simbolos o listas
                                        ;cuando es un simbolo se hace gcodenil.cuando es una lista, se realiza la operacion
                                        ;indicada por gcode-slots-options cuyo primer elemento coincide con el primero elemento de una lista en gcode-options.
                                        ;supuestamente es un codigo que requiere todo el resto de la lista que se le paso en la lista cuyo primer elemento era el nombre de la accion a realizar
                                        ; poner ejemplo
;; como si fuera un delegado. todos los segundos argumentos son funciones que resiven una lista de argumento llamada data.
(defparameter gcode-slots-options
  '(
    (:optional (let* ((result (loop for x in args
                                 collect (if (x node)
                                             (gcodenil x) ;; yo lo quiero sim backquote
                                             ""))))
                 (format nil "~{~a~%~}" result))) ;esta opcion es para los slots cuya generacion de codigo es opcional
    ))


                                        ;esta opcion es para los slots cuya generacion de codigo es opcional
                                        ;util cuando se esta generando se especifican los slots a generar en una lista, si se realiza un llamado a gif con el nombre de un slots
                                        ;la condicion del if, el then-code y el else-code se macro espande en:
                                        ;(if cond then-code else-code)
                                        ; y por defecto se puede hacer algo como: (gif edad) y se espande en:
                                        ;------------------------------------
                                        ;(if (slot-value node 'edad)
                                        ;----------(generate-code (edad node))
                                        ;---------- "")
(defmacro gif (slot-name &key
                           (cond-code `(slot-value node ',slot-name))
                           (then-code `(gcodenil ,slot-name))
                           (else-code ""))
  `(if ,cond-code
       ,then-code
       ,else-code))


                                        ;cuando se quiere realizar un format con un grupo de slots opcionales se pasa un llamado a gifs por la lista del format del macro gcode.
                                        ;por ejemplo la clase persona que tiene un campo casa, pareja y auto. Como todos estos camos pueden ser opcionales si se quiere generar el codigo de cada
(defmacro gifs (string-format &rest data)
  (let ((result `(format stream ,string-format)))
    (loop for element in data
       doing (destructuring-bind (slot-name &key
                                            (cond-code `(slot-value node ',slot-name))
                                            (then-code `(gcodenil ,slot-name))
                                            (else-code ""))
                 element
               (setf result (append result (list`(gif ,slot-name
                                                      ,(make-keyword ":~a" 'cond-code) ,cond-code
                                                      ,(make-keyword ":~a" 'then-code) ,then-code
                                                      ,(make-keyword ":~a" 'else-code) ,else-code))))))
    result))


(defun add-new-patterns (pattern)
  (setf  gcode-slots-options (append  recognize-patterns (list pattern))))

(defun make-gcodenil-list (list-of-symbols)
  "Given a list of symbols, return a list where each element is of the form (gcodenil symbol).
      Syntax:
       (make-gcodenil-list list-of-symbols)
       list-of-symbols: is a list of symbols"
  (loop for data in list-of-symbols
     collecting (if (eq (type-of data) 'symbol)
                    `(gcodenil ,data)
                    (if (eq (type-of data) 'cons)
                        (let* ((code (second (assoc (first data) gcode-slots-options))))
                          (if (eq code nil)
                              data
                              `(symbol-macrolet ((args ',(cdr data))) ,code)))))))

(defmacro gformat (stream format-string &rest slots)
  (let* ((gcodenil-list (make-gcodenil-list slots)))
    `(format ,stream ,format-string ,@gcodenil-list)))

(defparameter recognize-patterns
  `((indent (increment-indentation lang))
    (deindent (decrement-indentation lang))))

                                        ;pattern tiene que ser un simbolo y la lista por la que se quiere cambiar.
(defmacro add-new-patterns (symbol pattern)
  `(setf recognize-patterns (append  recognize-patterns (list (cons ',symbol ,pattern)))))

;; si se desea pasar una funcion que escriba en el string y la concatene al resultado se utiliza
;;sformat de la siguiente manera: (gcode person csharp ("a imprimir una persona" (sformat stream "nombre ~a edad ~a" (nombre node) (edad node)) "se imprimio la persona"))
(defun make-gformat-instructions (format-strings  args)
`(let* ((result-make-gformat ()))
  ,@(loop for element in format-strings
     collecting (if (stringp element)
                    `(setf result-make-gformat (concatenate 'string
                                                            result-make-gformat
                                                            (gformat nil ,element ,@(pop args))))
                    (if (and (listp element)  (eq (first element) 'sformat))
                    `(setf result-make-gformat (concatenate 'string
                                                            result-make-gformat
                                                            (format nil ,@(cdr element))))
                    element)))
  (format stream  result-make-gformat)))

(defmacro gindformat (stream (&rest format-strings) &rest format-args)
  "The comment and rationale for this macro (in spanish) can be found in the file macro-para-gformat.org"
  (let* ((gformat-list (make-gformat-instructions format-strings format-args)))
    `(macrolet ((indent-str (node) (declare (ignore node)) `(make-ind-str lang)))
       (symbol-macrolet ,recognize-patterns
         ,gformat-list))))

