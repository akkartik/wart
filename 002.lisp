(synonym no null
         iso equal)

(defun isnt(x y)
  (no (eq x y)))

(defmacro while(test &body body) ; define before we defover do
  `(loop while ,test do ,@body))
