;; call-fn and apply using extensible coerce

(defun call-fn(f &rest args)
  (apply (wart-coerce f 'function)
         args))

(defmacro wart-apply(f &rest args)
  `(call-fn (apply-fn (wart-coerce (fslot ,f) 'function))
            ,@args))
(defover apply wart-apply)

(defmacro defcall(type args &rest body)
  `(defcoerce ,type function
     (lambda ,args
       ,@body)))



(defmacro fslot(f)
  (or (function-value f)
      f))

(defun function-value(f)
  (cond
    ((isa f 'function)  f)
    ((function-name-p f)  (eval `(function ,f)))))

(defun function-name-p(f)
  (and (symbolp f)
       (eval `(fboundp ',f))))



(defcoerce macro function
  'idfn)

(defun idfn(x) x)

(defmacro thunk(&body body)
  `(lambda() ,@body))



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
