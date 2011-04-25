;; Miscellaneous primitives straddling lisp and wart universes

(mac wart-if args
  (if (oddp (length args)) ; there's an else
    `(cond ,@(tuples 2 (insert-t-in-penultimate-position args)))
    `(cond ,@(tuples 2 args))))
(defover if wart-if)

(defover do progn)
(macro-alias do1 prog1)

; don't try to print complex things at the repl
(mac perform body
  `(do1 nil ,@body))

(defover ++ incf)
(macro-alias -- decf)

(defover load wart-load)

(def wart-car(x)
  (if (consp x)
    (car x)
    x))
(defsetf wart-car rplaca)
(defover car wart-car)
(def wart-cdr(x)
  (errsafe (cdr x)))
(defsetf wart-cdr rplacd)
(defover cdr wart-cdr)



;; Internals

(def insert-t-in-penultimate-position(sexp)
  (if (singlep sexp)
    (cons t sexp)
    (cons (car sexp) (insert-t-in-penultimate-position (cdr sexp)))))
