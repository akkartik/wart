(defover type type-of) ; reserved keyword. bad dog in the manger CL!

(mac wart-if args
  (if (oddp (length args)) ; there's an else
    `(cond ,@(tuples (insert-t-in-penultimate-position args) 2))
    `(cond ,@(tuples args 2))))
(defover if wart-if)

(mac wart-do args `(call (fn() ,@args)))
(defover do wart-do)

(defover = setf)
(defover ++ incf)

(defover load wt-load)



;; Internals

(defun insert-t-in-penultimate-position(sexp)
  (if (singlep sexp)
    (cons t sexp)
    (cons (car sexp) (insert-t-in-penultimate-position (cdr sexp)))))
