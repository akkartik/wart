;; handlers are functions of the input s-expr
(defvar *wc-special-form-handlers* (make-hash-table))
(defvar *wc-type-handlers* (make-hash-table))

(defmacro wc-let(var val &body body)
  `(funcall (lambda(,var) ,@body) ,val))

(defun wrepl()
  (loop
    (format t "w> ")(finish-output)
    (format t "~a~%" (eval (wc (read))))))

(defun wc(sexp)
  (if (consp sexp)
    (wc-let handler (lookup-handler sexp)
      (if handler
        (funcall handler sexp)
        sexp))
    sexp))

(defun lookup-handler(sexp)
  (or (gethash (car sexp) *wc-special-form-handlers*)
      (gethash (type-of (car sexp)) *wc-type-handlers*)))



(defmacro def(name args &rest body)
  `(defun ,name ,@(compile-args args body)))

(defmacro mac(name args &rest body)
  `(defmacro ,name ,@(compile-args args body)))

; . for &rest
(defun compile-args(args body)
  `(,(wc-restify args) ,@body))



(defun wc-restify (arglist)
  (if (null arglist)
      '()
      (if (atom arglist)
          `(&rest ,arglist)
          (compile-dots arglist))))

(defun compile-dots (xs)
  (mapcar (lambda(_)
            (if (consp _)
              (compile-dots _)
              _))
          (append (butlast xs)
                  (wc-let x (last xs)
                    (if (cdr x)
                        `(,(car x) &rest ,(cdr x))
                        x)))))
