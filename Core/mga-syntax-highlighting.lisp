
(defmacro concatenate-symbol (symbol1 symbol2)
  `(read-from-string (concatenate 'string (symbol-name ,symbol1) (symbol-name ,symbol2))))

(defmacro concatenate-string-symbol2 (string symbol1 symbol2)
  `(read-from-string (concatenate 'string ,string (symbol-name ,symbol1) (symbol-name ,symbol2))))

(defmacro concatenate-string-symbol (string symbol)
  `(read-from-string (concatenate 'string ,string (symbol-name ,symbol))))

(defun convert-to-list-string (list-symbol)
  (loop for symbol in list-symbol
       collecting (concatenate 'string "\""(symbol-name symbol) "\"")))


(defun ctr-face (mode-name language optional-face)
  `( ,(format nil ";Initialize ~a-~a" mode-name optional-face)
    (setq ,(concatenate-string-symbol (format nil "~a-" mode-name) optional-face) ',(convert-to-list-string language))
  (setq ,(concatenate-string-symbol2 (format nil "~a-"  mode-name) optional-face  '-regexp) (regexp-opt ,(concatenate-string-symbol (format nil "~a-" mode-name) optional-face) 'words))))


(defun clear-memory (mode-name optional-face)
  `( ,(format nil  ";Cleaning memory of: ~a-~a" mode-name optional-face)
    (setq ,(concatenate-string-symbol (format nil "~a-" mode-name) optional-face) nil)
  (setq ,(concatenate-string-symbol2 (format nil "~a-"  mode-name) optional-face  '-regexp) nil)))


(defmacro language-syntax-highlighting (mode-name 
                                        (&key
                                          keywords
                                          builtin-face
                                          comment-delimiter-face 
                                          comment-face
                                          constant-face
                                          doc-face
                                          function-name-face
                                          negation-char-face
                                          prepocessor-face
                                          regexp-grouping-backslash 
                                          regexp-grouping-construct 
                                          string-face
                                          type-face
                                          variable-name-face
                                          warning-face)
                                           &optional (file ""))
    (prog2 (if (equal file "")
             (setf file (format nil "~a-mode.el" (string-downcase(string mode-name)))))
      (with-open-file (stream file :direction :output)
        (let* ((ctr-value  (append
                            (ctr-face mode-name keywords 'keywords)
                            (ctr-face mode-name builtin-face 'builtin)
                            (ctr-face mode-name comment-delimiter-face 'comment-delimiter) 
                            (ctr-face mode-name comment-face 'comment)
                            (ctr-face mode-name constant-face  'constant)
                            (ctr-face mode-name doc-face 'doc-face)
                            (ctr-face mode-name function-name-face 'function-name)
                            (ctr-face mode-name negation-char-face 'negation-char)
                            (ctr-face mode-name prepocessor-face 'prepocessor)
                            (ctr-face mode-name regexp-grouping-backslash 'regexp-grouping-backslash)
                            (ctr-face mode-name regexp-grouping-construct 'regexp-grouping-construct)
                            (ctr-face mode-name string-face 'string)
                            (ctr-face mode-name type-face 'type)
                            (ctr-face mode-name variable-name-face 'variable-name)
                            (ctr-face mode-name warning-face 'warning)))
               (clear (append
                            (clear-memory mode-name  'keywords)
                            (clear-memory mode-name  'builtin)
                            (clear-memory mode-name  'comment-delimiter) 
                            (clear-memory mode-name  'comment)
                            (clear-memory mode-name  'constant)
                            (clear-memory mode-name  'doc-face)
                            (clear-memory mode-name  'function-name)
                            (clear-memory mode-name  'negation-char)
                            (clear-memory mode-name  'prepocessor)
                            (clear-memory mode-name  'regexp-grouping-backslash)
                            (clear-memory mode-name  'regexp-grouping-construct)
                            (clear-memory mode-name  'string)
                            (clear-memory mode-name  'type)
                            (clear-memory mode-name  'variable-name)
                            (clear-memory mode-name  'warning)))
          (result `(
                      (setq ,(concatenate-symbol mode-name '-mode-hook) nil)
                      
                      (setq ,(concatenate-symbol mode-name '-default-tab-width) 3)
                      
                      (add-to-list 'auto-mode-alist  '(,(format nil "\"\\\\.~a\\\\'\"" mode-name) . ,(concatenate-symbol mode-name '-mode)))

                      ,@ctr-value

                      ,(format nil ";valores por defecto al font-look de ~a-mode" mode-name)
                      (setq ,(concatenate-symbol mode-name '-font-lock-keywords)
                            (list (cons ,(concatenate-symbol mode-name '-keywords-regexp)  `font-lock-keyword-face)
                                  (cons ,(concatenate-symbol mode-name '-builtin-regexp)  `font-lock-builtin-face)
                                  (cons ,(concatenate-symbol mode-name '-comment-delimiter-regexp)  `font-lock-comment-delimiter-face)
                                  (cons ,(concatenate-symbol mode-name '-comment-regexp)  `font-lock-comment-face)
                                  (cons ,(concatenate-symbol mode-name '-constant-regexp)  `font-lock-constant-face)
                                  (cons ,(concatenate-symbol mode-name '-doc-face-regexp)  `font-lock-doc-face)
                                  (cons ,(concatenate-symbol mode-name '-function-name-regexp)  `font-lock-function-name-face)
                                  (cons ,(concatenate-symbol mode-name '-negation-char-regexp)  `font-lock-negation-char-face)
                                  (cons ,(concatenate-symbol mode-name '-prepocessor-regexp)  `font-lock-preprocessor-face)
                                  (cons ,(concatenate-symbol mode-name '-regexp-grouping-backslash-regexp)  `font-lock-regexp-grouping-backslash)
                                  (cons ,(concatenate-symbol mode-name '-regexp-grouping-construct-regexp)  `font-lock-regexp-grouping-construct)
                                  (cons ,(concatenate-symbol mode-name '-string-regexp)  `font-lock-string-face)
                                  (cons ,(concatenate-symbol mode-name '-type-regexp)  `font-lock-type-face)
                                  (cons ,(concatenate-symbol mode-name '-variable-name-regexp)  `font-lock-variable-name-face)
                                  (cons ,(concatenate-symbol mode-name '-warning-regexp)  `font-lock-warning-face)))
                      ";derived from  lisp-mode"
                      (define-derived-mode ,(concatenate-symbol mode-name '-mode) lisp-mode
                        (run-hooks ',(concatenate-symbol mode-name '-mode-hook))
                        (setq font-lock-defaults '((,(concatenate-symbol mode-name '-font-lock-keywords)))))
                      
                      ,@clear
                    
                    (provide ',(concatenate-symbol mode-name '-mode)))
            ))(format stream "~{~a~%~}"  (mapcar (lambda (x) (string-downcase(format nil "~a" x))) result))))
  ))




;ejemplo
;(language-syntax-highlighting a (:keywords (a b c) :builtin-face (builtin) :comment-delimiter-face(delimiter) :comment-face (comm) :constant-face (const) :doc-face (doc) :function-name-face (funtion-name) :negation-char-face (negation) :prepocessor-face (prepo) :regexp-grouping-backslash (reg1) :regexp-grouping-construct (construct) :string-face (string) :variable-name-face (variable-name) :warning-face (string) :variable-name-face (variable-name) :type-face (defclass) :warning-face (warning)))
