;; handlers are functions of the input s-expr
(defvar *wc-special-form-handlers* (make-hash-table))
(defvar *wc-type-handlers* (make-hash-table))

(defun wrepl()
  (loop
    (format t "w> ")(finish-output)
    (format t "~a~%" (eval (wc (read))))))

(defun wc(sexp)
  (let ((handler (lookup-handler sexp)))
    (if handler
      (funcall handler sexp)
      sexp)))

(defun lookup-handler(sexp)
  (or (gethash (car sexp) *wc-special-form-handlers*)
      (gethash (type-of (car sexp)) *wc-type-handlers*)))

(setf (gethash 'def *wc-special-form-handlers*)
      (lambda(sexp)
        (cons 'defun (cdr sexp))))

(setf (gethash 'mac *wc-special-form-handlers*)
      (lambda(sexp)
        (cons 'defmacro (cdr sexp))))
