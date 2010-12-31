;; Override call and apply using extensible coerce

(ignore-redef
  (defun call(f &rest args)
    (apply (wart-coerce f 'function)
           args)))

(defmacro wart-apply(f &rest args)
  `(call (apply-fn (wart-coerce ,f 'function))
         ,@args))
(defover apply wart-apply)

(defmacro defcall(type arg &rest body)
  `(defcoerce ',type 'function
     (lambda(,arg)
       ,@body)))

(defmacro call*(f &rest args)
  (cond
    ((and (atom f) (macp f))  `(,f ,@args))
    ((and (atom f) (fboundp f))  `(call (function ,f) ,@args))
    (t  `(call ,f ,@args))))

(defcoerce 'macro 'function
  'idfn)



;; Internals

(defun apply-fn(f)
  (lambda(&rest args)
    (apply f (inline-last args))))

(defun inline-last(xs)
  (if (not (consp xs))
    xs
    (if (cdr xs)
      (cons (car xs) (inline-last (cdr xs)))
      (if (consp (car xs))
        (car xs)
        xs))))
