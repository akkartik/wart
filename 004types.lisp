;; Extensible coercion

(defun wart-type(x)
  (cond
    ((and (symbolp x) (macp x))  'macro)
    ((match x '(tagged _ _))  (cadr x))
    (t  (generalized-common-lisp-type-specifier (type-of x)))))
(defover type wart-type) ; reserved keyword. bad dog in the manger CL!

(defun annotate(type val)
  (list 'tagged type val))

(defun rep(x)
  (if (match x '(tagged _ _))
    (elt x 2)
    x))

(defun setrep(x y)
  (if (match x '(tagged _ _))
    (setf (elt x 2) y)
    (setf x y)))

(defsetf rep setrep)



(setf *wart-coercions* (table))

(defun wart-coerce(val dest)
  (fa (indexing *wart-coercions* (dest (wart-type val))
        (funcall it val))
      (funcall 'coerce val dest)))
(defover coerce wart-coerce)

(defmacro defcoerce(src dest converter)
  `(progn
     (unless (gethash ,dest *wart-coercions*)
       (setf (gethash ,dest *wart-coercions*) (table)))
     (setf (gethash ,src (gethash ,dest *wart-coercions*))
           ,converter)))

(defun isa(elem typ)
  (is typ (wart-type elem)))

(defcoerce 'macro 'function
  'idfn)



;; Internals

; add to this as we run into issues
(defun generalized-common-lisp-type-specifier(full-type)
  (cond
    ((match full-type '(integer))  'integer)
    ((match full-type '(integer _))  'integer)
    ((match full-type '(integer _ _))  'integer)
    ((match full-type '(simple-array character))  'string)
    ((match full-type '(simple-array character _))  'string)
    (t  full-type)))
