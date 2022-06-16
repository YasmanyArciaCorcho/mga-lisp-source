
(defnode infix-language (basic-language) ())


(gcode binary-op-node infix-language
                                    ("(~a ~a ~a)")
                                    (left-hand operator right-hand))
;;;}}}

;;;{{{ generate-code for assignment-node
(defmethod generate-code ((node assignment-node)
                          (language infix-language)
                          stream)
  (let* ((assignment-symbol (assignment-symbol language))
         (left (generate-code (left-hand node) language nil 0))
         (right (generate-code (right-hand node) language nil 0)))
    (format stream "~a ~a ~a"
            left assignment-symbol right)))
;;;}}}
;;;}}}
