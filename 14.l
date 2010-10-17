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
  (wc-let simplified-vars (simplify-arg-list vars)
    `(destructuring-bind (positional-vals keyword-alist)
                         (partition-keywords ,vals (rest-var (wc-restify ',vars)))
      (wc-let optional-alist (optional-vars ',vars)
        (wc-destructuring-bind ,simplified-vars
            (merge-keyword-vars positional-vals keyword-alist optional-alist
                                ',simplified-vars)
          ,@body)))))

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

(defun simplify-arg-list(args)
  (strip-default-values (wc-restify args)))

(defun partition-keywords(vals &optional rest-var alist)
  (if (consp vals)
    (if (keywordp (car vals))
      (if (equal (keyword->symbol (car vals)) rest-var)
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
    (if (equal (car vars) '&rest)
      (cadr vars)
      (rest-var (cdr vars)))))

; strip the colon
(defun keyword->symbol(k)
  (intern (symbol-name k)))

(defun merge-keyword-vars(positional-vals keyword-alist optional-alist args)
  (cond
    ((null args)  nil)
    ; dotted rest
    ((atom args)   (or positional-vals (alref args optional-alist)))
    ((equal (car args) '&rest)   (or positional-vals
                                     (alref (cadr args) keyword-alist)
                                     (alref (cadr args) optional-alist)))
    ((assoc (car args) keyword-alist)  (cons (alref (car args) keyword-alist)
                                             (merge-keyword-vars positional-vals
                                                                 keyword-alist
                                                                 optional-alist
                                                                 (cdr args))))
    ((null positional-vals)  (cons (alref (car args) optional-alist)
                                   (merge-keyword-vars nil keyword-alist optional-alist (cdr args))))
    (t  (cons (car positional-vals)
              (merge-keyword-vars (cdr positional-vals) keyword-alist optional-alist
                                  (cdr args))))))

(defun optional-vars(vars)
  (if (consp vars)
    (if (optional-var (car vars))
      (destructuring-bind (sym val) (car vars)
        (cons (cons sym val)
              (optional-vars (cdr vars))))
      (optional-vars (cdr vars)))))

(defun strip-default-values(args)
  (if (consp args)
    (cons (if (optional-var (car args))
            (caar args)
            (car args))
          (strip-default-values (cdr args)))))

(defun optional-var(var &optional alist)
  (if (and (consp var)
           (not (assoc (car var) alist)))
    (destructuring-bind (sym val &rest rest) var
      (and (symbolp sym)
           (atom val)
           (not (and val (symbolp val)))
           (null rest)))))

(defun alref(key alist)
  (cdr (assoc key alist)))



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
