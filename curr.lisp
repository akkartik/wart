(defun prn(x &optional (msg "")) (format t "~a- ~a~%" msg x) x)

(defmacro wc-let(var val &body body)
  `(funcall (lambda(,var) ,@body) ,val))



(defun wrepl()
  (loop
    (format t "w> ")(finish-output)
    (format t "~a~%" (wc-eval (read)))))

(defun wc-eval(sexp)
  (eval (wc sexp)))

(defun wc(sexp)
  (apply-to-all-subtrees #'wc-1 sexp))

(defun wc-1(sexp)
  (cond
    ((null sexp)  sexp)
    ((atom sexp)  sexp)
    ((and (equal (car sexp) 'quote) (null (caddr sexp)))  (list 'quote (lookup-quoted-handler (cadr sexp))))
    ((lookup-unquoted-handler sexp)   (funcall (lookup-unquoted-handler sexp) sexp))
    (t sexp)))

(defun lookup-unquoted-handler(sexp)
  (cond ((function-name (car sexp))   (lambda(sexp) (cons 'funcall sexp)))
        ((function-form (car sexp))   (lambda(sexp) (cons 'funcall sexp)))
        (t
          (or (gethash (car sexp) *wc-special-form-handlers*)
              (gethash (type-of (car sexp)) *wc-type-handlers*)))))

; In return for being able to override keywords you can't use keywords anywhere
; in the program. Not even inside quoted expressions.
(defun lookup-quoted-handler(name)
  (or (gethash name *wc-special-form-quoted-handlers*)
      (gethash (type-of name) *wc-type-quoted-handlers*)
      name))

;; handlers are functions of the input s-expr
(defvar *wc-special-form-handlers* (make-hash-table))
(defvar *wc-type-handlers* (make-hash-table))
;; quoted handlers are names of the handlers; all handlers must be named
(defvar *wc-special-form-quoted-handlers* (make-hash-table))
(defvar *wc-type-quoted-handlers* (make-hash-table))



(defmacro def(name args &rest body)
  `(defun ,name ,@(compile-args args body)))

(defmacro mac(name args &rest body)
  (wc `(defmacro ,name ,@(compile-args args body))))

(defmacro fn(args &rest body)
  `(lambda ,@(compile-args args body)))

(defmacro wc-complex-bind(vars vals &body body)
  (wc-let restified-vars (wc-restify vars)
    (wc-let simplified-vars (strip-default-values restified-vars)
      `(destructuring-bind (positional-vals keyword-alist)
                           (partition-keywords ,vals (rest-var ',restified-vars))
        (wc-let optional-alist (optional-vars ',restified-vars)
          (wc-destructuring-bind ,simplified-vars
              (merge-keyword-vars positional-vals keyword-alist optional-alist
                                  ',simplified-vars)
            ,@body))))))

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
    (alist (cdr (cut-at vars '(? &rest))))))

(defun strip-default-values(args)
  (if (consp args)
    (destructuring-bind (required optional rest) (partition args '(? &rest))
      (if rest
        (append required (map 'list #'car (tuples optional 2)) '(&rest) rest)
        (append required (map 'list #'car (tuples optional 2)))))
    args))

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



; To add a special form, write it as a macro with a different name, then
; register the macro with the right name.

(defmacro special-form(name new-name)
  `(progn
     (setf (gethash ',name *wc-special-form-handlers*)
           (lambda(_)
             (cons ',new-name (cdr _))))
     (setf (gethash ',name *wc-special-form-quoted-handlers*)
           ',new-name)))

(defmacro wc-let2(var val &body body)
  `(funcall (fn(,var) ,@body) ,val))
(special-form let wc-let2)

(defmacro wc-if(&rest args)
  (if (oddp (length args)) ; there's an else
    `(cond ,@(tuples (insert-t-in-penultimate-position args) 2))
    `(cond ,@(tuples args 2))))
(special-form if wc-if)

(defun insert-t-in-penultimate-position(sexp)
  (if (and (consp sexp) (null (cdr sexp)))
    (cons t sexp)
    (cons (car sexp) (insert-t-in-penultimate-position (cdr sexp)))))



(defun function-name(f)
  (and (atom f)
       (ignore-errors (eval `(functionp ,f)))))

(defun function-form(s)
  (and (consp s)
       (find (car s) '(fn lambda))))

(defun tuples(xs n &optional acc)
  (if (null xs)
    (nreverse acc)
    (tuples (nthcdr n xs) n (cons (firstn n xs) acc))))

(defun alist(xs &optional acc)
  (if (null xs)
    acc
    (alist (cddr xs) (cons (cons (car xs) (cadr xs))
                            acc))))

(defun firstn (n xs)
  (if (or (= n 0) (null xs))
      nil
      (cons (car xs) (firstn (1- n) (cdr xs)))))

(defun cut-at(s delims)
  (wc-let positions (map (type-of s) (lambda(x) (position x s)) delims)
    (if (car positions)
      (apply #'cut s positions))))

(defmacro each(var vals &body body)
  `(loop for ,var in ,vals do ,@body))

(defun partition(s delims)
  (destructuring-bind (x xs) (cut-at-first-available s delims)
    (cons x
          (partition-after xs delims))))

(defun partition-after(s delims)
  (if (consp delims)
    (if (eq (car s) (car delims))
      (destructuring-bind (x xs) (cut-at-first-available (cdr s) (cdr delims))
        (cons x
              (partition-after xs (cdr delims))))
      (cons nil (partition-after s (cdr delims))))))

(defun cut-at-first-available(s delims)
  (wc-let pos (position-first-available s delims)
    (if pos
      (list (cut s 0 pos) (cut s pos))
      (list s nil))))

(defun position-first-available(s elems)
  (if (consp elems)
    (wc-let pos (position (car elems) s)
      (if pos
        pos
        (position-first-available s (cdr elems))))))

(defun pos(test s)
  (if (isa test 'function)
    (position-if test s)
    (position test s)))

(defun isa(elem type)
  (eq (type-of elem) type))

(defun apply-to-all-subtrees(f sexp)
  (if (consp sexp)
    (funcall f (apply-to-all-leaves (lambda(_) (apply-to-all-subtrees f _)) sexp))
    sexp))

(defun apply-to-all-leaves(f sexp)
  (cond
    ((null sexp)  nil)
    ((atom sexp)  (funcall f sexp))
    ; consp sexp
    (t   (cons (funcall f (car sexp))
              (apply-to-all-leaves f (cdr sexp))))))



; with can be a macro since we aren't overriding an existing keyword
(defmacro with(binds &body body)
  `(let ,(tuples binds 2)
     ,@body))

(defmacro synonym(&rest args)
  `(setf ,@(mapcar (lambda (x)
                     `(symbol-function ',x))
                   args)))

(synonym no null
         trunc truncate
         len length
         uniq gensym
         cut subseq
         rev reverse
         is equal
         macex macroexpand
         macex1 macroexpand-1
         err error
         keep remove-if-not)

(defun wload(file)
  (with-open-file (f (merge-pathnames file))
    (loop with form = (read f)
          and eof = (gensym)
      do
        (wc-eval form)
        (setq form (read f nil eof))
      until (eq form eof))))
