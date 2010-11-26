;; Shorter names for some lisp primitives.

(defmacro synonym(&rest args)
  `(setf ,@(mapcar (lambda (x) `(symbol-function ',x))
                   args)))

(synonym no null
         call funcall
         is eq
         iso equal
         uniq gensym
         macex macroexpand
         macex1 macroexpand-1
         err error)
