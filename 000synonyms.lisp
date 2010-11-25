;; Better names for existing lisp primitives

(defmacro synonym(&rest args)
  `(setf ,@(mapcar (lambda (x) `(symbol-function ',x))
                   args)))

(synonym no null
         call funcall
         uniq gensym
         is eq
         iso equal)
