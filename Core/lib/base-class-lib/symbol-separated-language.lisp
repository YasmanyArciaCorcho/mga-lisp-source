;;;;;{{{ Documentation
;;;;{{{ History, TODO, and the like
;;;{{{ History
;;;
;;;   2012-04-16
;;;     - Added new language:       
;;;       - symbol-separated-language
;;;
;;;   2012-04-16
;;;      - Wrote the process-list-as
;;;
;;;}}}
;;;;}}}
;;;;;}}}

(in-package :adol*)

;;;{{{ Language definition
(defclass symbol-separated-language ()
  ((separator :accessor separator
              :initarg :separator
              :initform ";"
              :documentation "The symbol to separate statements")))
;;;}}}

;;;{{{ Code generation

(defmethod process-list-as ((type list-of-instructions)
                            (language symbol-separated-language)
                            (instructions list)
                            indentation)
  ;;{{{ Documentation
  "Process the given list as a list of instructions in common-lisp."
  ;;}}}
  (let* ((indentation-string (indentation-string indentation))
         (instruction-separator (separator language))
         (instructions-string
          (mapconcat (mapcar
                      (lambda (x)
                        (generate-code x
                                       language
                                       nil
                                       indentation))
                      instructions)
                     :start indentation-string
                     :separator (concat instruction-separator "~%" indentation-string)
                     :end instruction-separator)))
      instructions-string))

;;;}}}