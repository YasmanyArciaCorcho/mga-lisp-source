(defun under-scored(symbol)
  (let* ((symbol-string (string-downcase (symbol-name symbol)))
         (underscored-string ""))
    (loop for i across symbol-string
       do (setf underscored-string
                (concatenate 'string underscored-string
                             (if (char-equal i #\-) "_" (string i)))))
    underscored-string))

(defun make-diagram-class (in-slots-dict &optional (output-file "diagram.dot"))
  (with-open-file (stream output-file :direction :output)
    (let* ((nodes ()))
      (setf nodes  (loop for element in in-slots-dict
                      collecting (let* ((name-class (under-scored (first element)))
                                        (inherit (second element))
                                        (paint (if inherit
                                                   (concatenate 'string (under-scored (first inherit))  " -> " name-class "; ")
                                                   (concatenate 'string name-class "; "))))
                                   (loop for i in (cdr inherit)
                                    collect(setf paint (concatenate 'string paint (under-scored i) " -> " name-class  ";"))) paint)))
      (format stream "digraph A{ ~{~a~}}" nodes))))

(defparameter *dot-program* "C:/Program Files (x86)/Graphviz2.16/bin/dot.exe"
    "The path to the dot executable.")


(defun make-diagram-view (&optional (input-file "diagram.dot") (output-file "digraph.png"))
(let* ((output-file-param (format nil "-o~a" output-file)))
  #+sbcl (sb-ext:run-program *dot-program* `("-Tpng" ,output-file-param  ,input-file))
    #+clisp (ext:run-program *dot-program* :arguments `("-Tpng"  ,output-file-param ,input-file))))


(defun make-diagram-png (dict &optional (output-file "diagram.dot"))
  (make-diagram-class dict output-file)
  (make-driagram-view input-file  output-file))
