(setf wart-signatures* (table))

(defmacro$ def(name params &body body)
  `(progn
    (unless (gethash ',name wart-signatures*)
      (setf (gethash ',name wart-signatures*)
            (list nil)))
    (push (list (args-matcher ',params) (fn ,params ,@body))
          (gethash ',name wart-signatures*))
    (defun$ ,name(&rest ,$args)
      (call-correct-variant (gethash ',name wart-signatures*)
                            ,$args))))

(defmacro proc(name args . body)
  `(def ,name ,args ,@body nil))



;; Internals

(defun call-correct-variant(variants args)
  (if (car variants)
    (destructuring-bind (test func) (car variants)
      (if (apply test args)
        (apply func args)
        (call-correct-variant (cdr variants) args)))))

(defun args-matcher(params)
  (let* ((min-args  (len (required-params params))))
    (fn args
      (>= (len args)  min-args))))
