(defmacro ignore-redef(&body body)
  `(handler-bind (#+sbcl(sb-kernel:redefinition-warning 'muffle-warning))
     ,@body))

(defun pr(arg)
  (format t "~a" arg))

(defun prn(&rest args)
  (map 'list 'pr args)
  (format t "~%")
  (car args))

(defmacro synonym(&rest args)
  `(setf ,@(mapcar (lambda (x) `(symbol-function ',x))
                   args)))



(synonym no null
         call funcall ; bootstrap version
         is eq
         iso equal
         uniq gensym
         macex macroexpand
         macex1 macroexpand-1
         err error
         errsafe ignore-errors
         spawn sb-thread:make-thread)

(defun macp(f)
  (and (symbolp f)
       (macro-function f)))

(defun idfn(x) x)

(defun isnt(x y) (no (is x y)))

(defmacro while(test &body body) ; define before we defover do
  `(loop while ,test do ,@body))

(defmacro thunk(&body body)
  `(lambda() ,@body))

(defmacro aand(&rest args)
  (cond
    ((no args)   t)
    ((no (cdr args))   (car args))
    (`(let ((it ,(car args)))   (and it (aand ,@(cdr args)))))))

(defun match(a b)
  (or (is a b)
      (is b '_)
      (and (consp a) (consp b)
           (match (car a) (car b))
           (match (cdr a) (cdr b)))))



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

(defun tuples(n xs &optional acc)
  (if (no xs)
    (nreverse acc)
    (tuples n (nthcdr n xs) (cons (firstn n xs) acc))))

(defun pair(xs)
  (tuples 2 xs))

(defun pos(test s)
  (if (functionp test)
    (position-if test s)
    (position test s)))

(defun firstn(n xs)
  (if (or (= n 0) (no xs))
      nil
      (cons (car xs) (firstn (1- n) (cdr xs)))))

(defun flatten(tree)
  (let ((result '()))
    (labels ((scan (item)
               (if (listp item)
                 (map nil #'scan item)
                 (push item result))))
      (scan tree))
    (nreverse result)))

(defun alref(key alist)
  (cdr (assoc key alist)))

; helper for certain kinds of recursive functions
(defun append-or-afresh(f x xss)
  (if (call f x)
    (cons (cons x (car xss))
          (cdr xss))
    (cons (list x)
          xss)))

(defun group-by(f xs &optional acc)
  (if xs
    (group-by f (cdr xs)
              (append-or-afresh (lambda(x)
                                  (is (call f x)
                                      (call f (car (car acc)))))
                                (car xs)
                                acc))
    (nreverse acc)))



(synonym table make-hash-table)

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
; Returns multiple values to indicate all args not eval'd.
; Currently only makes sense to combine as (fa (la ..) (la ..) (la ..)). Do we need and-of-ors?
(defmacro la(&rest args)
  (if (no (cdr args))
    `(values ,(car args) nil)
    (let* ((available (uniq))
           (next-val (uniq))
           (next-available (uniq)))
      `(multiple-value-bind (it ,available) ,(car args)
        (multiple-value-bind (,next-val ,next-available) ,(cadr args)
          (cond
            ((not ,available)   (values nil t))
            ((not ,next-available)  (values it ,available))
            (t  (la (values ,next-val ,next-available)
                    ,@(cddr args)))))))))

(defmacro guarded-gethash(key table)
  `(if ,table
    (gethash ,key ,table)))

(defmacro indexing(tab inds expr)
  `(la (guarded-gethash ,(car inds) ,tab)
       ,@(map 'list (lambda(x) `(guarded-gethash ,x it)) (cdr inds))
       (values it 'never-returned) ; just a sentinel la can safely evaluate
       (values ,expr t)))
