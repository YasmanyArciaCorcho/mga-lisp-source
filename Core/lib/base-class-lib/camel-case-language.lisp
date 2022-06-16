;;;;;{{{ Documentation
;;;;{{{ History, TODO, and the like
;;;{{{ History
;;;
;;;   2012-04-16
;;;     - Added camel-case-language
;;;      
;;;   2012-04-16
;;;     - Wrote the generate-code-for-symbols
;;;
;;;
;;;}}}
;;;;}}}
;;;;;}}}

(in-package :adol*)

;;;{{{ Language definition
(defclass camel-case-language ()
  ()
  (:documentation "A language with camelCase notation. Example: adolStar."))
;;;}}}

;;;{{{ Code generation

(defmethod generate-code ((node symbol())
                          (language camel-case-language)
                          stream indentation)
  (declare (ignore indentation))
  (let* ((symbol-string (string-downcase (symbol-name node)))
         (camel-case-string "")
         (up-case nil))
    (if (> (length symbol-string) 1)
	(loop for c across symbol-string
	   when (not (char-equal c #\-))
	   do (setf camel-case-string
		    (concat camel-case-string
			    (if up-case (string-upcase c) c)))
	   do (setf up-case (char-equal c #\-)))
	(setf camel-case-string symbol-string))
    (format stream "~a" camel-case-string)))

;;;}}}
