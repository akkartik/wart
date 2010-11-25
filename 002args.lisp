;; Functions support complex arg lists in wart

(defmacro def(name args &rest body)
  `(defun ,name ,@(compile-args args body)))

(defmacro mac(name args &rest body)
  (wc `(defmacro ,name ,@(compile-args args body))))

(defmacro fn(args &rest body)
  `(lambda ,@(compile-args args body)))

(synonym cut subseq)

(defun tuples(xs n &optional acc)
  (if (no xs)
    (nreverse acc)
    (tuples (nthcdr n xs) n (cons (firstn n xs) acc))))

(defun alist(xs &optional acc)
  (if (no xs)
    acc
    (alist (cddr xs) (cons (cons (car xs) (cadr xs))
                            acc))))

(defun firstn (n xs)
  (if (or (= n 0) (no xs))
      nil
      (cons (car xs) (firstn (1- n) (cdr xs)))))



;; Internals

; Don't risk using CL let anywhere in this file; we're about to override it.
(defmacro _let(var val &body body)
  `(call (lambda(,var) ,@body) ,val))

(defmacro wc-complex-bind(vars vals &body body)
  (_let restified-vars (wc-restify vars)
    (_let simplified-vars (strip-default-values restified-vars)
      `(destructuring-bind (positional-vals keyword-alist)
                           (partition-keywords ,vals (rest-var ',restified-vars))
        (_let optional-alist (optional-vars ',restified-vars)
          (wc-destructuring-bind ,simplified-vars
              (merge-keyword-vars positional-vals keyword-alist optional-alist
                                  ',simplified-vars)
            ,@body))))))

(defmacro wc-destructuring-bind(vars vals &body body)
  (_let gval (gensym)
    `(_let ,gval ,vals
       (wc-destructuring-bind-1 ,vars ,gval ,@body))))
(defmacro wc-destructuring-bind-1(vars vals &body body)
  (cond ((no vars)  `(progn ,@body))
        ((atom vars)  `(_let ,vars ,vals ,@body))
        ((is '&rest (car vars))  `(wc-destructuring-bind-1 ,(cadr vars) ,vals
                                     ,@body))
        (t `(wc-destructuring-bind-1 ,(car vars) (car ,vals)
                  (wc-destructuring-bind-1 ,(cdr vars) (cdr ,vals)
                        ,@body)))))

; handles destructuring, . for rest
; returns a list whose first element is a common lisp lambda-list
; (cltl2, 5.2.2)
(defun compile-args(args body)
  (_let args (wc-restify args)
    (_let gargs (gensym)
      `((&rest ,gargs)
         (wc-complex-bind ,args ,gargs
           ,@body)))))

(defun partition-keywords(vals &optional rest-var alist)
  (if (consp vals)
    (if (keywordp (car vals))
      (if (is rest-var (keyword->symbol (car vals)))
        (list nil
              (cons (cons (keyword->symbol (car vals)) (cdr vals))
                    alist))
        (destructuring-bind (var val &rest rest) vals
          (destructuring-bind (new-vals new-alist) (partition-keywords rest rest-var alist)
            (list new-vals
                  (cons (cons (keyword->symbol var) val) new-alist)))))
      (destructuring-bind (new-vals new-alist)
                          (partition-keywords (cdr vals) rest-var alist)
        (list (cons (car vals) new-vals)
              new-alist)))
    (list () alist)))

(defun rest-var(vars)
  (if (consp vars)
    (if (is '&rest (car vars))
      (cadr vars)
      (rest-var (cdr vars)))))

; strip the colon
(defun keyword->symbol(k)
  (intern (symbol-name k)))

(defun merge-keyword-vars(positional-vals keyword-alist optional-alist args)
  (cond
    ((no args)  nil)
    ; dotted rest
    ((atom args)   (or positional-vals (eval (alref args optional-alist))))
    ((is '&rest (car args))   (or positional-vals
                                  (alref (cadr args) keyword-alist)
                                  (eval (alref (cadr args) optional-alist))))
    ((assoc (car args) keyword-alist)  (cons (alref (car args) keyword-alist)
                                             (merge-keyword-vars positional-vals
                                                                 keyword-alist
                                                                 optional-alist
                                                                 (cdr args))))
    ((no positional-vals)  (cons (eval (alref (car args) optional-alist))
                                 (merge-keyword-vars nil keyword-alist optional-alist (cdr args))))
    (t  (cons (car positional-vals)
              (merge-keyword-vars (cdr positional-vals) keyword-alist optional-alist
                                  (cdr args))))))

(defun optional-vars(vars)
  (if (consp vars)
    (alist (cdr (cut-at vars '(? &rest))))))

(defun strip-default-values(args)
  (if (consp args)
    (destructuring-bind (required optional rest) (_partition args '(? &rest))
      (if rest
        (append required (map 'list #'car (tuples optional 2)) '(&rest) rest)
        (append required (map 'list #'car (tuples optional 2)))))
    args))

(defun alref(key alist)
  (cdr (assoc key alist)))

(defun wc-restify (arglist)
  (if (no arglist)
      '()
      (if (atom arglist)
          `(&rest ,arglist)
          (compile-dots arglist))))

(defun compile-dots (xs)
  (mapcar (lambda(_)
            (if (consp _)
              (compile-dots _)
              _))
          (append (butlast xs)
                  (_let x (last xs)
                    (if (cdr x)
                        `(,(car x) &rest ,(cdr x))
                        x)))))

(defun _partition(s delims)
  (destructuring-bind (x xs) (cut-at-first-available s delims)
    (cons x
          (_partition-after xs delims))))

(defun _partition-after(s delims)
  (if (consp delims)
    (if (is (car s) (car delims))
      (destructuring-bind (x xs) (cut-at-first-available (cdr s) (cdr delims))
        (cons x
              (_partition-after xs (cdr delims))))
      (cons nil (_partition-after s (cdr delims))))))

(defun cut-at-first-available(s delims)
  (_let pos (position-first-available s delims)
    (if pos
      (list (cut s 0 pos) (cut s pos))
      (list s nil))))

(defun position-first-available(s elems)
  (if (consp elems)
    (_let pos (position (car elems) s)
      (if pos
        pos
        (position-first-available s (cdr elems))))))

(defun cut-at(s delims)
  (_let positions (map (type-of s) (lambda(x) (position x s)) delims)
    (if (car positions)
      (apply #'cut s positions))))
