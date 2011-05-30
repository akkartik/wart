;; call and apply using extensible coerce

(defmacro call(f &rest args)
  `(funcall (wart-coerce ,f 'function)
            ,@args))

(extend-macro call(f &rest args) :if (function-name-p f)
  `(,f ,@args))

(extend-macro call(f &rest args) :if (macp f)
  `(,f ,@args))

(defmacro wart-apply(f &rest args)
  `(apply (wart-coerce (fslot ,f) 'function)
          ,@args))
(defover apply wart-apply)

; defcall syntax is restricted defun:
;   name must be a type
;   no destructured, optional or rest args
(defmacro$ defcall(type args &rest body)
  (if (not (gethash type (gethash 'function wart-coercions*)))
  `(defcoerce ,type function ; coercer: function returning a function
     (lambda(,(car args)) ; instance of type
       (fn ,(cdr args) ; fn not defined yet
         ,@body)))
  `(defcoerce ,type function
     (let ((,$oldfn (gethash ',type (gethash 'function wart-coercions*))))
       (lambda(,(car args))
         (fn ,$args
           (if (iso (len ',(cdr args)) (len ,$args))
             (destructuring-bind ,(cdr args) ,$args
               ,@body)
             (apply (call ,$oldfn ,(car args)) ,$args))))))))



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
