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

;; handlers are functions of the input s-expr
(defvar *wc-special-form-handlers* (make-hash-table))
(defvar *wc-type-handlers* (make-hash-table))



(defmacro def(name args &rest body)
  `(defun ,name ,args ,@body))

(defmacro mac(name args &rest body)
  `(defmacro ,name ,args ,@body))
