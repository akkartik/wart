(defmacro$ mac(name params &rest body)
  (wt-transform
    `(defmacro$ ,name(&rest ,$args)
       ,(compile-params params body $args))))
