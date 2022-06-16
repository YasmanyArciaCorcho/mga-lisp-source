(defnode symbol-separated-language ()
         ((separator :accessor separator
                     :initarg :separator
                     :initform ";"
                     :documentation "The current separator for the language"))
         )

                                        ;Funciones para el trabajo con los terminales de instrucciones.

(defmacro node-ends-with-separator (class-name)
  `(defmethod generate-code :after ((node ,class-name) 
                                    (lang symbol-separated-language)
                                    stream)
              (format stream "~a" (separator lang))))
