(synonym no null
         iso equal)

(defun isnt(x y)
  (no (eq x y)))

(defmacro while(test &body body) ; define before we defover do
  `(loop while ,test do ,@body))



(defun match(a b)
  (or (equal a b)
      (equal b '_)
      (and (consp a) (consp b)
           (equal (car b) '_..))
      (and (consp a) (consp b)
           (match (car a) (car b))
           (match (cdr a) (cdr b)))))

(defun allf(&rest tests)
  (if (no tests)
    (lambda(&rest args) t)
    (lambda(&rest args)
      (and (apply (car tests)
                  args)
           (apply (apply 'allf (cdr tests))
                  args)))))
