(synonym no null
         iso equal)

(defun isnt(x y)
  (no (eq x y)))

(defun match(a b)
  (or (equal a b)
      (equal b '_)
      (and (consp a) (consp b)
           (or (equal (car b) '_..)
               (and (match (car a) (car b))
                    (match (cdr a) (cdr b)))))))

(defmacro while(test &body body) ; define before we defover do
  `(loop while ,test do ,@body))
