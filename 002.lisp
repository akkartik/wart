(synonym no null
         iso equal)

(defun isnt(x y)
  (no (eq x y)))

(defmacro while(test &body body) ; define before we defover do
  `(loop while ,test do ,@body))

(defun match(a b)
  (or (equal a b)
      (and (consp a) (consp b)
           (match (car a) (car b))
           (match (cdr a) (cdr b)))))

(extend match(a b) :if (equal b '_)
  t)
(extend match(a b) :if (and (consp a) (consp b)
                         (equal (car b) '_..))
  t)
