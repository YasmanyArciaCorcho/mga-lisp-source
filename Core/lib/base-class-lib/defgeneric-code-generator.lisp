(defgeneric generate-code (node lang stream))

(defmethod generate-code (node lang stream)
           (format stream "~a" node))

(defmethod generate-code ((node list) lang stream)
  (let* ((list (mapcar (lambda (x) (with-output-to-string (s)
                                     (generate-code x lang s)))
                       node)))
    (format stream "~{~a~%~}"
            list)))
