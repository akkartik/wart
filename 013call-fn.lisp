;; call-fn and apply using extensible coerce

(defun call-fn(f &rest args)
  (apply (wart-coerce f 'function)
         args))

(defmacro wart-apply(f &rest args)
  `(call-fn (apply-fn (wart-coerce ,f 'function))
            ,@args))
(defover apply wart-apply)

(defmacro defcall(type arg &rest body)
  `(defcoerce ,type function
     (lambda(,arg)
       ,@body)))

(defcoerce macro function
  'idfn)

(defun function-value(f)
  (cond
    ((isa f 'function)  f)
    ((function-name-p f)  (eval `(function ,f)))))

(defun idfn(x) x)

(defmacro thunk(&body body)
  `(lambda() ,@body))



;; Internals

(defun function-name-p(f)
  (and (symbolp f)
       (eval `(fboundp ',f))))

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
