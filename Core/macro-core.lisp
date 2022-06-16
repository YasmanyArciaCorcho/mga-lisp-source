;;; pdict:lista de propiedades para las clases declaradas.
;;;---Cada elemento contenido en pdict es una lista, con el nombre de la clase declarada como primer elemento.
;;;---Una lista con los padres de la clase (herencia) como segundo elemento y el resto de la lista son las nuevas propiedades declaradas en su definicion.

(defmacro mac(expr)
  `(macroexpand-1 ',expr))

;;;Rellena el diccionario (pdict-ejemplo) con el formato correcto.
(defun fill-pdict (property-name dict)
  (let ((aux (cdr dict)))
    (prog2
        (setf dict (append (first dict) property-name))
        (setf dict (append (list dict) aux)))))

;;;Insertar una nueva definicion de tipo.
(defun insert-class-data(dict class-name inherit &rest slots)
  (if (not (eq dict nil))
      (setf dict (remove-class-data class-name dict)))
  (push (list class-name inherit) dict)
  (loop for property-name in slots
     doing (setf dict (fill-pdict property-name dict)))
  dict)


(defun remove-class-data(class-name dict)
  (loop for  element in dict
     doing (if(eq (car element) class-name)
              (setf dict (remove element dict))))
  dict)
;;;Util para trabajo dentro de macros.
(defun make-keyword (string symbol)
  (read-from-string (format nil string symbol)))

;;:Retorna los datos completos de una definicion. 
(defun get-data-from-dictionary(class-name dictionary)
  (assoc class-name dictionary))

;;;Retorna las propiedades de una definicion en el diccionario seleccionado.
(defun get-slots-from-class (class-name dictionary)
  (let* ((result(cddr (Get-data-from-dictionary class-name dictionary))))
    (mapcan #'(lambda (x) (if (eq x nil) nil (list x))) result)))

;;;Problema clasico de aplanar una lista en un lenguaje funcional(LP)
(defun flatten-list (list)
  (let* (result)
    (loop for e in list
       doing (if (and (atom e) (not (eq e nil)))
                 ;;(pushnew e result :test #'equal) cambio porque debe insertarse en orden
                 (setf result (append result (list e)))
                 (setf result (append result (flatten-list e)))))
    result))

(defun get-all-slots-from-class (class-name dictionary)
  (let* ((result (let ((inherit (second (Get-data-from-dictionary class-name dictionary))))
                   (flatten-list (union;siempre fue append pero lo cabie por union. antes del commin del 12-6 a las 22.3
                                  (get-all-slots-from-inherit inherit dictionary)
                                  (get-slots-from-class class-name dictionary))))))
    (if (equal result '(nil))
        (setf result nil))
    result))

(defun class-inherit (class-name dictionary)
  (second (get-data-from-dictionary class-name dictionary)))

(defun get-all-slots-from-inherit (inherit dictionary)
  (loop for inherit-act in inherit
     collecting(Get-All-Slots-From-Class inherit-act dictionary)))


(defun second-elements (list)
  (let* ((count 0) (result ()))
    (loop for element in list
       doing(if (eq count 1)
                (prog2 (setf count 0)(setf result (union result (list element))))
                (setf count 1)))
    result))

(defun get-all-properties (class-name dictionary)
  (second-elements(get-all-slots-from-class class-name dictionary)))

(defun clear-properties-dict()
  (setf slots-dict nil)
  (setf initarg-dict nil)
  (setf accessor-dict nil))

;;;Macro para crear la definicion completa de una clase.
;;;-->diccionario para el trabajo de las propiedades de la nueva definicion 
;;;-->class-name: nombre de la clase.
;;;-->inherit: Lista con las clases padres (herencia).
;;;-->string-obj: Slots para de definicion del Print-Object (string-obj slots).
;;;-->new slots: los nuevos slots que define la clase, para tener una distincion entre los
;;----->slots heredados y los nuevos definidos por la clase para su definicion.
;;;-->all-slots:todos los Slots de definicion de la clase.
;;;-->ejemplo de entrada (create-class-data humano ()("El humano ~a tiene ~a annos" name age) (name age other-properties))
;;;-->otro ejemplo (create-class-data humano-con-casa (humano)("La persona ~a tiene ~a annos y tiene ~a casas" name age home)(home))
(defmacro create-class-data (class-name documentation inherit slots-def ctr-funtion  string-obj)
  `(progn
     ;;;Definiendo clase.
     (defclass ,class-name
         ,inherit
       ;;esta es la definicion de los slots
       ,slots-def
       (:documentation ,documentation))
     ;;;Definiendo funcion constructora.
     ,ctr-funtion
      ;;;Print-Object de la clase definida.
     ;;;string-object: formato para definir la cadena a imprimir.
     ;;;stream salida del print-object
     (defmethod print-object((node ,class-name) stream)
       (format stream ,(car string-obj)
               ,@(loop for slot in (cdr string-obj)
                    collecting `(,slot node))))))

;;;Macro definidor de los metodos generadores de codigo.
;;;-->los hans se escriben en el orden que se quieren generar.
;;;-->format: es el codigo que se quiere generar relacionado con los hans.
;; si la lista args es nil se rellena con una lista que tiene todos los slots del tipo class-name
(defmacro gcode (class-name language (&rest format) &rest args)
  (if (eq args nil)
      (setf args (list (get-all-properties class-name accessor-dict))))
  `(defmethod generate-code ((node ,class-name) (lang ,language) stream)
     (gindformat stream ,format ,@args)))

;;;Macro para ayudar a la generacion de codigo de varias clases, generar el codigo de un conjunto de clases.e
(defmacro gcodes(list-names-def language)
  `(progn ,@(loop for name-class in list-names-def
               collect `(gcode ,(first name-class) ,language ,(second name-class) ,(second name-class)))))
                                        ;ejemplo de un elemento de list-names(es la lista de los parametros de mac-generate-code)
                                        ;((sum-class ("~a + ~a") ((left-hand right-hand)))  
                                        ;(displaystyle-class ("\\displaystyle{~a}") ((unic-hand)))) latex-language)  

