(defun prn(x &optional (msg "")) (format t "~a- ~a~%" msg x) x)

(defmacro wc-let(var val &body body)
  `(funcall (lambda(,var) ,@body) ,val))



(defun wrepl()
  (loop
    (format t "w> ")(finish-output)
    (format t "~a~%" (eval (wc (read))))))

(defun wc(sexp)
  (if (consp sexp)
    (wc-let handler (lookup-handler sexp)
      (if handler
        (funcall handler sexp)
        sexp))
    sexp))

(defun lookup-handler(sexp)
  (cond ((function-name (car sexp))   (lambda(sexp) (cons 'funcall sexp)))
        ((function-form (car sexp))   (lambda(sexp) (cons 'funcall sexp)))
        (t
          (or (gethash (car sexp) *wc-special-form-handlers*)
              (gethash (type-of (car sexp)) *wc-type-handlers*)))))

;; handlers are functions of the input s-expr
(defvar *wc-special-form-handlers* (make-hash-table))
(defvar *wc-type-handlers* (make-hash-table))



(defmacro def(name args &rest body)
  `(defun ,name ,@(compile-args args body)))

(defmacro mac(name args &rest body)
  `(defmacro ,name ,@(compile-args args body)))

(defmacro fn(args &rest body)
  `(lambda ,@(compile-args args body)))

(defmacro wc-complex-bind(vars vals &body body)
  `(destructuring-bind (positional-vals keyword-alist) (partition-keywords ,vals)
    (wc-destructuring-bind ,vars
        (merge-keyword-vars positional-vals keyword-alist
                            (strip-lambda-keywords ',vars))
      ,@body)))

(defmacro wc-destructuring-bind(vars vals &body body)
  (wc-let gval (gensym)
    `(wc-let ,gval ,vals
       (wc-destructuring-bind-1 ,vars ,gval ,@body))))
(defmacro wc-destructuring-bind-1(vars vals &body body)
  (cond ((null vars)  `(progn ,@body))
        ((atom vars)  `(wc-let ,vars ,vals ,@body))
        ((equal (car vars) '&rest)  `(wc-destructuring-bind-1 ,(cadr vars) ,vals
                                       ,@body))
        (t `(wc-destructuring-bind-1 ,(car vars) (car ,vals)
                  (wc-destructuring-bind-1 ,(cdr vars) (cdr ,vals)
                        ,@body)))))

; handles destructuring, . for rest
; returns a list whose first element is a common lisp lambda-list
; (cltl2, 5.2.2)
(defun compile-args(args body)
  (wc-let args (wc-restify args)
    (wc-let gargs (gensym)
      `((&rest ,gargs)
         (wc-complex-bind ,args ,gargs
           ,@body)))))

(defun partition-keywords(vals &optional alist)
  (if (consp vals)
    (if (keywordp (car vals))
      (destructuring-bind (var val &rest rest) vals
        (destructuring-bind (new-vals new-alist) (partition-keywords rest alist)
          (list new-vals
                (cons (cons (keyword->symbol var) val) new-alist))))
      (destructuring-bind (new-vals new-alist)
                          (partition-keywords (cdr vals) alist)
        (list (cons (car vals) new-vals)
              new-alist)))
    (list () alist)))

; strip the colon
(defun keyword->symbol(k)
  (intern (symbol-name k)))

(defun merge-keyword-vars(positional-vals keyword-alist args)
  positional-vals)

(defun strip-lambda-keywords(args)
  (remove-if-cons (lambda(arg) (find arg '(&rest &optional &key))) args))

(defun remove-if-cons(f l)
  (cond ((null l)   nil)
        ((atom l)   (if (not (funcall f l)) l))
        (t  (if (funcall f (car l))
              (remove-if-cons f (cdr l))
              (cons (car l) (remove-if-cons f (cdr l)))))))



(defun wc-restify (arglist)
  (if (null arglist)
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
                  (wc-let x (last xs)
                    (if (cdr x)
                        `(,(car x) &rest ,(cdr x))
                        x)))))



; [..] => (lambda(_) (..))
; http://arclanguage.org/item?id=11551
(set-macro-character #\] (get-macro-character #\)))
(set-macro-character #\[
  #'(lambda(stream char)
      (declare (ignore char))
      (apply #'(lambda(&rest args)
                 `(fn(_) (,@args)))
             (read-delimited-list #\] stream t))))



(defun function-name(f)
  (and (atom f)
       (ignore-errors (eval `(functionp ,f)))))

(defun function-form(s)
  (and (consp s)
       (find (car s) '(fn lambda))))
