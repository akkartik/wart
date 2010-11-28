;; Shorter names for some lisp primitives.

(defmacro synonym(&rest args)
  `(setf ,@(mapcar (lambda (x) `(symbol-function ',x))
                   args)))

(synonym no null
         call funcall
         is eq
         iso equal
         uniq gensym
         macex macroexpand
         macex1 macroexpand-1
         err error)

(defun pr(arg)
  (format t "~a" arg))

(defun prn(&rest args)
  (map 'list #'pr args)
  (format t "~%")
  (car args))

(defun idfn(x) x)

(synonym len length
         rev reverse
         cut subseq
         join append)

(defun singlep(x)
  (and (consp x)
       (no (cdr x))))

(defun pairp(x)
  (and (consp x)
       (consp (cdr x))
       (no (cddr x))))
