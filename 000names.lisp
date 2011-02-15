;; Better names

(defmacro ignore-redef(&body body)
  `(handler-bind (#+sbcl(sb-kernel:redefinition-warning 'muffle-warning))
     ,@body))

(defmacro synonym(&rest args)
  `(setf ,@(mapcar (lambda (x) `(symbol-function ',x))
                   args)))

(synonym uniq gensym)

(defmacro macro-alias(a b)
  (let ((args (uniq)))
    `(defmacro ,a(&rest ,args)
       `(,',b ,@,args))))
