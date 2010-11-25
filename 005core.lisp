(mac wc-if args
  (if (oddp (length args)) ; there's an else
    `(cond ,@(tuples (insert-t-in-penultimate-position args) 2))
    `(cond ,@(tuples args 2))))
(special-form if wc-if)

(mac wc-do args `(call (fn() ,@args)))
(special-form do wc-do)



;; Internals

(defun insert-t-in-penultimate-position(sexp)
  (if (and (consp sexp) (no (cdr sexp)))
    (cons t sexp)
    (cons (car sexp) (insert-t-in-penultimate-position (cdr sexp)))))
