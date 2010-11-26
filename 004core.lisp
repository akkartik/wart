(special-form type type-of) ; reserved keyword. bad dog in the manger CL!

(mac wc-if args
  (if (oddp (length args)) ; there's an else
    `(cond ,@(tuples (insert-t-in-penultimate-position args) 2))
    `(cond ,@(tuples args 2))))
(special-form if wc-if)

(mac wc-do args `(call (fn() ,@args)))
(special-form do wc-do)

(def singlep(x)
  (and (consp x)
       (no (cdr x))))



;; Internals

(defun insert-t-in-penultimate-position(sexp)
  (if (singlep sexp)
    (cons t sexp)
    (cons (car sexp) (insert-t-in-penultimate-position (cdr sexp)))))
