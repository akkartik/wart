;; Extensible coercion

(defun wart-type(x)
  (if (match x '(tagged _ _))
    (cadr x)
    (let ((full-type (type-of x)))
      (if (consp full-type)
        (car full-type)
        full-type))))
(defover type wart-type) ; reserved keyword. bad dog in the manger CL!

(defun annotate(type val)
  (list 'tagged type val))

(defun rep(x)
  (if (match x '(tagged _ _))
    (elt x 2)
    x))



(setf *wart-coercions* (table))

(defun wart-coerce(val dest)
  (fa (indexing *wart-coercions* (dest (wart-type val))
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
  (is typ (wart-type elem)))
