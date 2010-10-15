;; handlers are functions of the input s-expr
(defvar *wc-special-form-handlers* (make-hash-table))
(defvar *wc-type-handlers* (make-hash-table))

(defun prn(x &optional (msg "")) (format t "~a- ~a~%" msg x) x)

(defun wrepl()
  (loop
    (format t "w> ")(finish-output)
    (format t "~a~%" (eval (wc (read))))))

(defun wc(sexp)
  (if (consp sexp)
    (let ((handler (lookup-handler sexp)))
      (if handler
        (funcall handler sexp)
        sexp))
    sexp))

(defun lookup-handler(sexp)
  (cond ((function-name (car sexp))   (lambda(sexp) (cons 'funcall sexp)))
        (t
          (or (gethash (car sexp) *wc-special-form-handlers*)
              (gethash (type-of (car sexp)) *wc-type-handlers*)))))

(defmacro wc-let(var val &body body)
  `(funcall (lambda(,var) ,@body) ,val))



(defmacro def(name args &rest body)
  `(defun ,name ,@(compile-args args body)))

(defmacro mac(name args &rest body)
  `(defmacro ,name ,@(compile-args args body)))

(defmacro fn(args &rest body)
  `(lambda ,@(compile-args args body)))



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
    (if (flatp (subseq args 0 (or (position '&optional args) (length args))))
        `(,args ,@body)
        (wc-let gargs (gensym)
          `((&rest ,gargs)
             (wc-destructuring-bind ,args ,gargs
               ,@body))))))



(defun flatp(xs)
  (or (null xs)
      (and (consp xs)
           (atom (car xs))
           (flatp (cdr xs)))))

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
                  (let ((x (last xs)))
                    (if (cdr x)
                        `(,(car x) &rest ,(cdr x))
                        x)))))



; [..] => (lambda(_) (..))
(defmacro defdelim (left right parms &body body)
  `(ddfn ,left ,right #'(lambda ,parms ,@body)))

(let ((rpar (get-macro-character #\) )))
  (defun ddfn (left right fn)
    (set-macro-character right rpar)
    (set-macro-character left
                         #'(lambda (stream char)
                             (declare (ignore char))
                             (apply fn
                                    (read-delimited-list right stream t))))))

(defdelim #\[ #\] (&rest args)
  `(fn(_) (,@args)))



(defun function-name(f)
  (and (atom f)
       (ignore-errors (eval `(functionp ,f)))))
