;; Extensible coercion

(setf *wart-coercions* (table))

(defun wart-coerce(val dest)
  (fa (indexing *wart-coercions* (dest (type-of val))
        (funcall it val))
      (funcall #'coerce val dest)))
(defover coerce wart-coerce)

(defmacro defcoerce(src dest converter)
  `(progn
     (unless (gethash ,dest *wart-coercions*)
       (setf (gethash ,dest *wart-coercions*) (table)))
     (setf (gethash ,src (gethash ,dest *wart-coercions*))
           ,converter)))

(defun isa(elem typ)
  (is typ (type-of elem)))
