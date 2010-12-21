;; Extensible coercion

(setf *wart-coercions* (table))

(defun wart-coerce(val dest)
  (fa (indexing *wart-coercions* (dest (type* val))
        (funcall it (val* val)))
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



;; Internals

(defun type*(val)
  (if (and (isa val 'symbol) (not (fboundp val)))
    (type* (eval val))
    (type-of val)))

(defun val*(val)
  (if (isa val 'symbol)
    (val* (eval val))
    val))
