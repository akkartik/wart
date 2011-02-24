(setf wart-signatures* (table))

(defmacro$ def-case(name params test &body body)
  `(progn
    (push (list (allf (length-matcher ',params)
                      (fn ,params ,test))
                (fn ,params
                  (handling-$vars ,@body)))
          (gethash ',name wart-signatures*))
    (defun$ ,name(&rest ,$args)
      (call-correct-variant (gethash ',name wart-signatures*)
                            ,$args))))

(defmacro$ def-normal(name params &body body)
  `(progn
    (push (list (length-matcher ',params)
                (fn ,params
                  (handling-$vars ,@body)))
          (gethash ',name wart-signatures*))
    (defun$ ,name(&rest ,$args)
      (call-correct-variant (gethash ',name wart-signatures*)
                            ,$args))))

(defmacro$ def(name params &body body)
  (case (car body)
    (:case  `(def-case ,name ,params ,(cadr body) ,@(cddr body)))
    (otherwise `(def-normal ,name ,params ,@body))))

(defmacro proc(name args . body)
  `(def ,name ,args ,@body nil))



;; Internals

(defun call-correct-variant(variants args)
  (if variants
    (destructuring-bind (test func) (car variants)
      (if (or (singlep variants)
              (apply test args))
        (apply func args)
        (call-correct-variant (cdr variants) args)))))

(defun length-matcher(params)
  (fn args
    (>= (length (remove-if [kwargp params _] args)) ; len will be overridden
        (length (required-params params)))))

(defun allf(&rest tests)
  (if (no tests)
    (lambda(&rest args) t)
    (lambda(&rest args)
      (and (apply (car tests)
                  args)
           (apply (apply 'allf (cdr tests))
                  args)))))
