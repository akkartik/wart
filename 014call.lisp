; like funcall, but can handle macros
(defmacro call(f &rest args)
  `(call-fn (fslot ,f) ,@args))

(extend-macro call(f &rest args) :if (macp f)
  `(,f ,@args))
