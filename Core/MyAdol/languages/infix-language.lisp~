;;;;;{{{ Initial documentation
;;;;{{{ History, TODO, and the like
;;;{{{ History
;;;
;;;   2012-04-21
;;;     - Added the code generation for the assignment node,
;;;       to avoid the extra parenthesis.
;;;
;;;   2012-04-16
;;;     - Added infix-language
;;;       
;;;    2012-04-02
;;;      - Started everything.
;;;  
;;;
;;;
;;;}}}
;;;;}}}
;;;;;}}}

(in-package :adol*)

(defclass infix-language (basic-language)
  () )


(gcode operator-binary infix-language
                                    "(~a ~a ~a)"
                                    (left operator right))
;;;}}}

;;;{{{ generate-code for assignment-node
(defmethod generate-code ((node assignment-node)
                          (language infix-language)
                          stream indentation)
  (declare (ignore indentation))
  (let* ((assignment-symbol (assignment-symbol language))
         (left (generate-code (left-hand node) language nil 0))
         (right (generate-code (right-hand node) language nil 0)))
    (format stream "~a ~a ~a"
            left assignment-symbol right)))
;;;}}}
;;;}}}
