;; Miscellaneous primitives straddling lisp and wart universes

(mac wart-if args
  (if (oddp (length args)) ; there's an else
    `(cond ,@(tuples 2 (insert-t-in-penultimate-position args)))
    `(cond ,@(tuples 2 args))))
(defover if wart-if)

(mac wart-do args `(call (fn() ,@args)))
(defover do wart-do)

(defover ++ incf)

(defover load wart-load)



;; Internals

(def insert-t-in-penultimate-position(sexp)
  (if (singlep sexp)
    (cons t sexp)
    (cons (car sexp) (insert-t-in-penultimate-position (cdr sexp)))))
