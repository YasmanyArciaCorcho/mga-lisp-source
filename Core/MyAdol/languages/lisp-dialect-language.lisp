
(defnode lisp-dialect-language (basic-language)
  ()
  :documentation "A language with prefix notation.")

(gcode binary-op-node  lisp-dialect-language
        ("(~a ~a ~a)")
        (operator left right))
