(defover type type-of) ; reserved keyword. bad dog in the manger CL!

(mac wart-if args
  (if (oddp (length args)) ; there's an else
    `(cond ,@(tuples 2 (insert-t-in-penultimate-position args)))
    `(cond ,@(tuples 2 args))))
(defover if wart-if)

(mac wart-do args `(call (fn() ,@args)))
(defover do wart-do)

(defover = setf)
(defover ++ incf)

(defover load wart-load)



;; Internals

(defun insert-t-in-penultimate-position(sexp)
  (if (singlep sexp)
    (cons t sexp)
    (cons (car sexp) (insert-t-in-penultimate-position (cdr sexp)))))
