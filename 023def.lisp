(setf wart-signatures* (table))

(defmacro$ def(name params &body body)
  (destructuring-bind (test &rest body)   (if (iso :case (car body))
                                            (cdr body)
                                            (cons t body))
    `(progn
      (push (list (fn args
                    (and (apply (length-matcher ',params) args)
                         (apply (fn ,params ,test) args)))
                  (fn ,params
                    (handling-$vars ,@body)))
            (gethash ',name wart-signatures*))
      (defun$ ,name(&rest ,$args)
        (call-correct-variant (gethash ',name wart-signatures*)
                              ,$args)))))

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
