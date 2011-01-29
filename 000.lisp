(defmacro ignore-redef(&body body)
  `(handler-bind (#+sbcl(sb-kernel:redefinition-warning 'muffle-warning))
     ,@body))

(defun pr1(arg)
  (format t "~a" arg))
(defun pr(&rest args)
  (map 'list 'pr1 args)
  (car args))
(defun prn(&rest args)
  (apply 'pr args)
  (format t "~%")
  (car args))

(defun writeln(&rest args)
  (map 'list 'write args)
  (format t "~%")
  (car args))



(defmacro synonym(&rest args)
  `(setf ,@(mapcar (lambda (x) `(symbol-function ',x))
                   args)))

(synonym no null
         is eq
         iso equal
         uniq gensym
         macex macroexpand
         macex1 macroexpand-1
         err error)



(defmacro macro-alias(a b)
  (let ((args (uniq)))
    `(defmacro ,a(&rest ,args)
       `(,',b ,@,args))))

(macro-alias errsafe ignore-errors)

(defun match(a b)
  (or (is a b)
      (is b '_)
      (and (consp a) (consp b)
           (match (car a) (car b))
           (match (cdr a) (cdr b)))))

(defmacro while(test &body body) ; define before we defover do
  `(loop while ,test do ,@body))
