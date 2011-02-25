(defmacro extend(name params Case test &body body)
  (let ((args (uniq)))
    `(let ((orig (symbol-function ',name)))
       (setf (symbol-function ',name)
             (lambda(&rest ,args)
               (if (apply (lambda ,params ,test) ,args)
                 (apply (lambda ,params ,@body) ,args)
                 (apply orig ,args)))))))

(defmacro extend-macro(name params Case test body)
  (let ((args (uniq))
        (orig (uniq)))
    `(progn
       (setf (macro-function ',orig)
             (macro-function ',name))
       (defmacro ,name(&rest ,args)
         (destructuring-bind ,params ,args
           (if ,test
             ,body
             (cons ',orig ,args)))))))
