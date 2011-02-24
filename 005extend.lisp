(defmacro extend(name params If test &body body)
  (let ((args (uniq)))
    `(let ((orig (symbol-function ',name)))
       (setf (symbol-function ',name)
             (lambda(&rest ,args)
               (if (apply (lambda ,params ,test) ,args)
                 (apply (lambda ,params ,@body) ,args)
                 (apply orig ,args)))))))

(defmacro extend-macro(name params If test body)
  (let ((orig (uniq)))
    `(progn
       (setf (macro-function ',orig)
             (macro-function ',name))
       (defmacro ,name ,params
         (if ,test
           ,body
           (,orig ,@params))))))
