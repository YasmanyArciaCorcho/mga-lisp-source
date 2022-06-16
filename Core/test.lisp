(load "init-mga")

(defnode csharp (indentable-language) ())
(defnode sum-node () (a b))
(setf c (csharp 4 4))
(setf a (sum-node 4 (sum-node 6 (sum-node 7 8))))

(DEFMETHOD GENERATE-CODE ((NODE SUM-NODE) (LANG CSHARP) STREAM) (MACROLET ((INDENT-STR (NODE) (DECLARE (IGNORE NODE)) '(MAKE-IND-STR LANG)))
                                                                
                                                                  (LET* ((RESULT-MAKE-GFORMAT NIL))
                                                                   (SETF RESULT-MAKE-GFORMAT (CONCATENATE 'STRING RESULT-MAKE-GFORMAT (FORMAT NIL "~a " (with-output-to-string (s) (GENERATE-CODE (A NODE) LANG s)))))
                                                                   (SETF RESULT-MAKE-GFORMAT (CONCATENATE 'STRING RESULT-MAKE-GFORMAT (FORMAT NIL "~a " (with-output-to-string (s) (GENERATE-CODE (B NODE) LANG s)))))
                                                                   (FORMAT STREAM RESULT-MAKE-GFORMAT))))
(DEFMETHOD GENERATE-CODE :before ((NODE SUM-NODE) (LANG CSHARP) STREAM)
           (FORMAT stream (concatenate 'string "hola"(MAKE-IND-STR LANG))))
