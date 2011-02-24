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

(defmacro extend(name params If test &body body)
  (let ((args (uniq)))
    `(let ((orig (symbol-function ',name)))
       (setf (symbol-function ',name)
             (lambda(&rest ,args)
               (if (apply (lambda ,params ,test) ,args)
                 (apply (lambda ,params ,@body) ,args)
                 (apply orig ,args)))))))
