
(load "myAdol-ast-language")
(load "languages/load")
(gcode variable-node T ("~a"))


(defun print-common-c-parameters (parameters)
  (format nil "~a~{~a~}"(concatenate 'string (string-downcase (first (car parameters))) " " (string-downcase (name (second (car parameters)))))
          (loop for (type var-name) in (cdr parameters)
             collecting (concatenate 'string ", " (string-downcase type) " "  (string-downcase (name var-name))))))

;;ver lo que quedo en generate-code.lisp de adol
(defnode-instance (csharp (indentable-language
                           camel-case-language
                           infix-language) ()) (c 2 2))

(gcode list csharp ((sformat (let* ((obj (mapcar (lambda (x) (with-output-to-string (s)
                                                      (generate-code x lang s)))
                                        node)))
                          (format nil "~{~a~%~}"
                                  obj)))))


; built-in-type-bool-node
;(gcode built-in-type-bool csharp ("bool"))
(gcode self-node csharp ("this"))
(mark-node-as-indentable self-node csharp)

(gcode type-array-node csharp ("~a[]"))
(mark-node-as-indentable type-array-node csharp)

(gcode type-list-node csharp ("List<~a>"))
(mark-node-as-indentable type-list-node csharp)

(gcode  list-lenght-node csharp ("~a.Count"))

(gcode type-void-node csharp ("void"))

(gcode null-node csharp ("null"))

(gcode true-node csharp ("true"))

(gcode false-node csharp ("false"))


(gcode library-node csharp ( "using System;~%using System.Collections.Generic;~%using System.Linq;~%using System.Text;~%~%namespace ~a{~%" indent "~a}" deindent) (name) (members))

(gcode class-definition-node csharp ("public class ~a ~a{~%" indent "~a ~%~a ~% ~a ~%~a}" deindent)
       (class-name (bif class-parent :then-code (concatenate 'string ": " (gcodenil class-parent)))
                   (slots-definition functions-definition methods-definition operator-overloads)))

(mark-node-as-indentable type-list-node csharp)

(gcode  list-lenght-node csharp ("~a.Count"))

(gcode type-void-node csharp ("void"))

(gcode null-node csharp ("null"))

(gcode true-node csharp ("true"))

(gcode false-node csharp ("false"))

;library-node
(gcode library-node csharp ( "using System;~%using System.Collections.Generic;~%using System.Linq;~%using System.Text;~%~%namespace ~a{~%" indent "~a}" deindent) (name) (members))

(gcode class-definition-node csharp ("public class ~a ~a{~%" indent "~a ~%~a ~% ~a ~%~a}" deindent)
       (class-name (bif class-parent :then-code (concatenate 'string ": " (gcodenil class-parent)))
                   (slots-definition functions-definition methods-definition operator-overloads)))
(mark-node-as-indentable class-definition-node csharp)

;;slot-definition-node
(gcode slot-definition-node csharp ("public ~a ~a {get; set;}"))
(mark-node-as-indentable slot-definition-node csharp)

(defun generate-list (list lang stream)
  (let* ((data
          (concatenate 'string  (format nil "~a" (generate-code (first list) lang nil))
                       (format nil "~{,~a~}" (mapcar (lambda (x) (with-output-to-string (s)
                                                                   (generate-code x lang s)))
                                                     (cdr list))))))
    (format stream data)))


(gcode function-call-node csharp ("~a~a(~a)")
       ((gif instance :then-code (gformat nil "~a." instance))
        name (generate-list (params node) lang nil)))

;;function-definition-node
(gcode function-definition-node csharp ("public static ~a ~a (~a){~%" indent "~a}" deindent)
       ((gcodenil-exp (first (return-types node)))
        func-name
        (print-common-c-parameters (func-params node))) (body))
(mark-node-as-indentable function-definition-node csharp)

;;method-definition-node
(gcode method-definition-node csharp ("public ~a ~a (~a){~%" indent "~a}" deindent)
       ((gcodenil-exp (first (return-types node)))
        func-name
        (print-common-c-parameters (func-params node))) (body))
(mark-node-as-indentable method-definition-node csharp)

;;overload-definition-node
(gcode operator-overload-node csharp ("public static ~a operator ~a (~a){~%" indent "~a}" deindent)
       ((gcodenil-exp (first (return-types node)))
        func-name
        (print-common-c-parameters (func-params node))) (body))

(mark-node-as-indentable operator-overload-node csharp)

(gcode class-constructor-node csharp ("public ~a(~a){~%" indent "~a}")
       (func-name (print-common-c-parameters (func-params node))) (body))

(gcode field-access-node csharp ("~a.~a"))

(gcode field-setting-node csharp ("~a.~a = ~a"))

(gcode array-setting-node csharp ("~a[~a] = ~a"))

                                        ;ver con el profesor
(gcode return-node csharp ((sformat "return ~a" (gcodenil-exp (first (re  turn-values node))))))

(gcode unary-minus-node csharp ("- ~a"))

(gcode variable-declaration-node csharp ("~a ~a"))

(gcode variable-node csharp ("~a"))

(gcode new-object-instance-node csharp ("new ~a" (sformat "(~a)" (generate-list (object-creation-args node) lang nil))) (object-name))

(gcode for-node csharp ("for (int ~a = ~a; ~a <= ~a;a++){~%" indent "~a}" deindent)
       (var-name inital-value final-value var-name) (instructions))

(gcode while-node csharp ("while (~a){~%" indent "~a}" deindent) (boolean-expression) (body))

(gcode array-creation-node csharp ("new ~a[~a]"))

(gcode array-length-node csharp ("~a.Lnegth"))

(gcode if-else-node csharp ("if(~a){~%" indent "~a}else{~%" "~a}" deindent) (boolean-expression) (then-body) (else-body)) 

(gcode min-node csharp ("Math.Min(~a, ~a)"))

(gcode max-node csharp ("Math.Max(~a, ~a)"))

(gcode sin-node csharp ("Math.Sin(~a)"))

(gcode cos-node csharp ("Math.Cos(~a)"))

(gcode tan-node csharp ("Math.Tan(~a)"))

(gcode exp-node csharp ("Math.Exp(~a)"))

(gcode sqrt-node csharp ("Math.Sqrt(~a)"))

(gcode pow-node csharp ("Math.Pow(~a, ~a)"))
