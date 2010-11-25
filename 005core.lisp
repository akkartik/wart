(mac wc-let(var val . body)
  `(call (fn(,var) ,@body) ,val))
(special-form let wc-let)

(mac wc-if args
  (if (oddp (length args)) ; there's an else
    `(cond ,@(tuples (insert-t-in-penultimate-position args) 2))
    `(cond ,@(tuples args 2))))
(special-form if wc-if)

(mac each(var vals . body)
  `(loop for ,var in ,vals do ,@body))

(mac wc-do args `(call (fn() ,@args)))
(special-form do wc-do)

; with can be a macro since we aren't overriding an existing keyword
(defmacro with(binds &body body)
  `(let ,(tuples binds 2)
     ,@body))



;; Internals

(defun insert-t-in-penultimate-position(sexp)
  (if (and (consp sexp) (no (cdr sexp)))
    (cons t sexp)
    (cons (car sexp) (insert-t-in-penultimate-position (cdr sexp)))))
