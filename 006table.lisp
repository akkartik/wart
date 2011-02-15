(defun table()
  (make-hash-table :test 'equal))

; 'first available' - like or, but uses multiple values to indicate unavailable
(defmacro$ fa(&rest args)
  (cond
    ((no (cdr args))  (car args))
    (t  `(multiple-value-bind (,$val ,$unavailable) ,(car args)
           (if ,$unavailable
             (fa ,@(cdr args))
             ,$val)))))

; 'last available' - like and, but uses multiple values to indicate available
; Returns multiple values to indicate all args not eval'd.
; Currently only makes sense to combine as (fa (la ..) (la ..) (la ..)). Do we need and-of-ors?
(defmacro$ la(&rest args)
  (if (no (cdr args))
    `(values ,(car args) nil)
    `(multiple-value-bind (it ,$available) ,(car args)
      (multiple-value-bind (,$next-val ,$next-available) ,(cadr args)
        (cond
          ((not ,$available)   (values nil t))
          ((not ,$next-available)  (values it ,$available))
          (t  (la (values ,$next-val ,$next-available)
                  ,@(cddr args))))))))

(defmacro guarded-gethash(key table)
  `(if ,table
    (gethash ,key ,table)))

(defmacro indexing(tab inds expr)
  `(la (guarded-gethash ,(car inds) ,tab)
       ,@(map 'list (lambda(x) `(guarded-gethash ,x it)) (cdr inds))
       (values it 'never-returned) ; just a sentinel la can safely evaluate
       (values ,expr t)))
