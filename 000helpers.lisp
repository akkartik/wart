(defmacro synonym(&rest args)
  `(setf ,@(mapcar (lambda (x) `(symbol-function ',x))
                   args)))

(synonym no null
         call funcall ; bootstrap version
         is eq
         iso equal
         uniq gensym
         macp macro-function
         macex macroexpand
         macex1 macroexpand-1
         err error
         errsafe ignore-errors
         spawn sb-thread:make-thread)

(defun pr(arg)
  (format t "~a" arg))

(defun prn(&rest args)
  (map 'list #'pr args)
  (format t "~%")
  (car args))

(defun idfn(x) x)

(defun isnt(x y) (no (is x y)))

(synonym len length
         rev reverse
         cut subseq
         join append
         keep remove-if-not
         trunc truncate)

(defun singlep(x)
  (and (consp x)
       (no (cdr x))))

(defun pairp(x)
  (and (consp x)
       (consp (cdr x))
       (no (cddr x))))

(defun pos(test s)
  (if (functionp test)
    (position-if test s)
    (position test s)))

(synonym table make-hash-table)

(defun match(a b)
  (or (is a b)
      (is b '_)
      (and (consp a) (consp b)
           (match (car a) (car b))
           (match (cdr a) (cdr b)))))

(defmacro ignore-redef(&body body)
  `(handler-bind (#+sbcl(sb-kernel:redefinition-warning 'muffle-warning))
     ,@body))

(defmacro aand(&rest args)
  (cond
    ((no args)   t)
    ((no (cdr args))   (car args))
    (`(let ((it ,(car args)))   (and it (aand ,@(cdr args)))))))

; 'first available' - like or, but uses multiple values to indicate unavailable
(defmacro fa(&rest args)
  (cond
    ((no (cdr args))  (car args))
    (t  (let* ((val (uniq))
               (unavailable (uniq)))
           `(multiple-value-bind (,val ,unavailable) ,(car args)
             (if ,unavailable
               (fa ,@(cdr args))
               ,val))))))

; 'last available' - like and, but uses multiple values to indicate available
(defmacro la(&rest args)
  (cond
    ((no (cdr args))  (car args))
    (t  (let* ((next-val (uniq))
               (next-available (uniq)))
          `(let* ((it ,(car args)))
            (multiple-value-bind (,next-val ,next-available) ,(cadr args)
              (if ,next-available
                (la ,@(cdr args))
                ,(car args))))))))

(defmacro refxy(tab inds expr)
  `(la (gethash ,(car inds) ,tab)
       ,@(map 'list (lambda(x) `(gethash ,x it)) (cdr inds))
       (values it t) ; never returned; sentinel la can evaluate
       (values it t) ; never returned; sentinel la can evaluate
       (values ,expr t)))
