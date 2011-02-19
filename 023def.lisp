(setf wart-signatures* (table))

(defmacro$ def(name params &body body)
  `(progn
    (setf (gethash ',name wart-signatures*)
          ',params)
    (defun$ ,name(&rest ,$args)
      ,(compile-params params $args body))))

(defmacro proc(name args . body)
  `(def ,name ,args ,@body nil))
