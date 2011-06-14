;; Extensible coercion

(defun wart-type(x)
  (cond
    ((macp x)   'macro)
    ((characterp x)  'character)
    ((stringp x)  'string)
    ((integerp x)   'integer)
    ((floatp x)   'float)
    ((keywordp x)   'symbol)
    ((match x '(tagged _ _))  (cadr x))
    (t  (generalized-common-lisp-type-specifier (type-of x)))))
(defover type wart-type) ; reserved keyword. bad dog in the manger CL!

(defmacro annotate(type val)
  `(list 'tagged ',type ,val))

(defun rep(x)
  (if (match x '(tagged _ _))
    (elt x 2)
    x))

(defun setrep(x y)
  (if (match x '(tagged _ _))
    (setf (elt x 2) y)
    (setf x y)))
(defsetf rep setrep)

(defun macp(f)
  (and (symbolp f)
       (macro-function f)))



(setf wart-coercions* (table))

(defover cl-coerce coerce)
(defun wart-coerce(val dest)
  (fa (indexing wart-coercions* (dest (wart-type val))
        (funcall it val))
      (funcall 'coerce val dest)))
(defover coerce wart-coerce)

(defmacro as(type val)
  `(wart-coerce ,val ',type))

(defmacro defcoerce(src dest converter)
  `(progn
     (unless (gethash ',dest wart-coercions*)
       (setf (gethash ',dest wart-coercions*) (table)))
     (setf (gethash ',src (gethash ',dest wart-coercions*))
           ,converter)))

(defmacro isa(elem typ)
  `(eq ',typ (wart-type ,elem)))

(extend-macro isa(elem typ) :if (consp typ)
  `(member (wart-type ,elem) ',typ))



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
