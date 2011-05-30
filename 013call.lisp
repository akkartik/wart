;; call and apply using extensible coerce

(defmacro call(f &rest args)
  `(call-fn ,f ,@args))

(extend-macro call(f &rest args) :if (function-name-p f)
  `(,f ,@args))

(extend-macro call(f &rest args) :if (macp f)
  `(,f ,@args))

(defmacro wart-apply(f &rest args)
  `(apply (wart-coerce (fslot ,f) 'function)
          ,@args))
(defover apply wart-apply)

(defmacro defcall(type args &rest body)
  `(defcoerce ,type function ; coercer: function returning a function
     (lambda(,(car args)) ; instance of type
       (fn ,(cdr args) ; fn not defined yet
         ,@body))))



(defmacro fslot(f)
  (if (function-name-p f)
    (eval `(function ,f))
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

(defun call-fn(f &rest args)
  (apply (wart-coerce f 'function)
         args))

(defun inline-last(xs)
  (if (not (consp xs))
    xs
    (if (cdr xs)
      (cons (car xs) (inline-last (cdr xs)))
      (if (consp (car xs))
        (car xs)
        xs))))
