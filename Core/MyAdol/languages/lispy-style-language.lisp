
(defnode lispy-style-language
  () ()
  :documentation "A language with names of funcs and vars in lisp style. Example: adol-star.")

(defmethod generate-code ((node symbol)
                          (language lispy-style-language)
                          stream )
  (declare (ignore indentation))
  (let* ((symbol-string (string-downcase (symbol-name node))))    
    (format stream "~a" symbol-string)))


