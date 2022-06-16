
(defnode underscore-language () ())

(defmethod generate-code ((node symbol)
                          (language underscore-language)
                          stream indentation)
  (declare (ignore indentation))
  (let* ((symbol-string (string-downcase (symbol-name node)))
         (underscored-string ""))
     (loop for c across symbol-string
            do (setf underscored-string
                     (concat underscored-string
                             (if (char-equal c #\-) "_" c))))
    ;; return underscored-string
    (format stream "~a" underscored-string)))

;;;}}}
