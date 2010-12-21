;; Extensible coercion

(setf *wart-coercions* (table))

(defun wart-coerce(val dest)
  (prn val " " dest)
  (fa (la (prn (gethash dest *wart-coercions*) "-<")
          (prn (gethash (prn (type* val) ":") it) "<-")
          (prn (values (prn it " <=") t) "<--")
          (values (funcall it (val* val)) t))
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
