(defmacro$ def(name params &body body)
  `(defun$ ,name(&rest ,$args)
     ,(compile-params params $args body)))

(defmacro proc(name args . body)
  `(def ,name ,args ,@body nil))
