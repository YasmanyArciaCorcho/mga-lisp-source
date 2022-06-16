;; Cuando una instruccion crea un scope y se quiere identificar con simbolos
;;se puede heredar de esta clase y tener toda la generacion de codigo para instoduccir los simbolos automaticamente en el codigo generado.
(defnode context-limit-language () (open-symbol close-symbol)
  :documentation "Language with limited scope.")

;;para identifcar que en la generacion de codigo para el nodo se tiene que automatizar la generacion de los simbolos limitadores del scope

(defmacro context-limit-languaje (class-name)
`(defmethod generate-code :before ((node ,class-name) 
                                     (lang context-limit-language)
                                     stream)
              (format stream "~a" (open-simbol lang)))

`(defmethod generate-code :after ((node ,class-name) 
                                  (lang context-limit-language)
                                  stream)
              (format stream "~a" (close-simbol lang))))
