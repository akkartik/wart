(setf wart-signatures* (table))

(defmacro$ def(name params &body body)
  `(progn
    (push (list (args-matcher ',params)
                (fn ,params
                  (handling-$vars ,@body)))
          (gethash ',name wart-signatures*))
    (defun$ ,name(&rest ,$args)
      (call-correct-variant (gethash ',name wart-signatures*)
                            ,$args))))

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

(defun args-matcher(params)
  (fn args
    (>= (length args) ; len will be overridden
        (length (required-params params)))))
