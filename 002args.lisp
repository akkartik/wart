;; Functions support complex arg lists in wart

(defmacro def(name params &rest body)
  `(defun ,name ,@(compile-params params body)))

(defmacro mac(name params &rest body)
  (wc `(defmacro ,name ,@(compile-params params body))))

(defmacro fn(params &rest body)
  `(lambda ,@(compile-params params body)))

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

(defun firstn(n xs)
  (if (or (= n 0) (no xs))
      nil
      (cons (car xs) (firstn (1- n) (cdr xs)))))



;; Internals

; handles destructuring, . for rest
; returns a list whose first element is a common lisp lambda-list
; (cltl2, 5.2.2)
(defun compile-params(params body)
  (let* ((ra  (uniq))
         (non-keyword-args  (uniq))
         (keyword-alist   (uniq))
         (optional-alist  (optional-params params))
         (params-without-?  (params-without-defaults params))
         (rest-param  (rest-param params))
         (z   (wc-getargs-exprs params-without-? non-keyword-args keyword-alist optional-alist)))
    `((&rest ,ra)
      (let* ((,non-keyword-args  (strip-keyword-args ,ra ',rest-param))
             (,keyword-alist   (keyword-args ,ra ',rest-param)))
          (let* ,z
            ,@body)))))

(defun wc-getargs-exprs(params non-keyword-args keyword-alist optional-alist)
  (map 'list
       (lambda(param)
          (list param
                `(fa (get-arg ',param ',params ,non-keyword-args ,keyword-alist)
                     ,(alref param optional-alist))))
       (vars-in-paramlist params)))

; 'first available' - like or, but uses value to indicate unavailable
(defmacro fa(a b)
  (let* ((val (uniq))
         (empty (uniq)))
    `(multiple-value-bind (,val ,empty) ,a
      (if ,empty
        ,b
        ,val))))

(defun get-arg(var params arglist keyword-alist)
  (cond
    ((assoc var keyword-alist)  (alref var keyword-alist))
    ((no params)  (values nil 'no-arg))
    ((iso params var)  arglist)
    ((not (consp params))   (values nil 'no-arg))
    ((assoc (car params) keyword-alist)  (get-arg var (cdr params) arglist keyword-alist))
    ((no arglist)  (values nil 'no-arg))
    (t   (fa (get-arg var (car params) (car arglist) keyword-alist)
             (get-arg var (cdr params) (cdr arglist) keyword-alist)))))

(defun keyword-args(args rest-param)
  (cond
    ((no args)  ())
    ((not (consp args))  ())
    ((keywordp (car args))  (let ((param (keyword->symbol (car args))))
                              (cons (cons param
                                          (if (iso param rest-param)
                                            (cdr args)
                                            (cadr args)))
                                    (keyword-args (cddr args) rest-param))))
    (t   (keyword-args (cdr args) rest-param))))

(defun rest-param(params)
  (cond
    ((no params)  ())
    ((not (consp params))   params)
    (t   (rest-param (cdr params)))))

(defun optional-params(params)
  (partition-optional-params (extract-optional-params params)))

(defun extract-optional-params(params)
  (strip-required (strip-rest params)))

(defun strip-keyword-args(args rest-param)
  (cond
    ((no args)  ())
    ((keywordp (car args))  (if (iso (keyword->symbol (car args)) rest-param)
                              ()
                              (strip-keyword-args (cddr args) rest-param)))
    (t   (cons (car args) (strip-keyword-args (cdr args) rest-param)))))

(defun partition-optional-params(oparams)
  (cond
    ((not (consp oparams))  ())
    ((not (consp (cdr oparams)))  (list oparams))
    (t   (cons (cons (car oparams)
                     (cadr oparams))
               (partition-optional-params (cddr oparams))))))

(defun strip-required(params)
  (if (consp params)
    (let ((optargs (member '? params)))
      (if optargs
        (cdr optargs)
        ()))
    params))

(defun strip-rest(params)
  (cond
    ((no params) ())
    ((not (consp params))   ())
    (t   (cons (car params)
               (strip-rest (cdr params))))))

(defun vars-in-paramlist(params)
  (cond
    ((no params)  ())
    ((not (consp params))   (list params)) ; rest
    ((iso (car params) '?)   (vars-in-optional-paramlist (cdr params)))
    (t   (append (vars-in-paramlist (car params))
                 (vars-in-paramlist (cdr params))))))

(defun vars-in-optional-paramlist(params)
  (cond
    ((no params)  ())
    ((not (consp params))   (list params)) ; rest
    (t (cons (car params)
             (vars-in-optional-paramlist (cddr params)))))) ; skip default

; like vars-in-paramlist, but don't undot rest args
(defun params-without-defaults(params)
  (cond
    ((no params)  ())
    ((not (consp params))   params) ; rest
    ((iso (car params) '?)   (optional-params-without-defaults (cdr params)))
    (t   (cons (car params)
               (params-without-defaults (cdr params))))))

(defun optional-params-without-defaults(params)
  (cond
    ((no params)  ())
    ((not (consp params))   params) ; rest
    (t  (cons (car params)
              (optional-params-without-defaults (cddr params)))))) ; skip default

; like vars-in-paramlist, but stop at param
(defun prior-params(param params)
  (cond
    ((no params)  ())
    ((not (consp params))   (list params)) ; rest
    ((iso param (car params))  ())
    ((iso (car params) '?)   (prior-optional-params param (cdr params)))
    (t   (append (prior-params param (car params))
                 (prior-params param (cdr params))))))

(defun prior-optional-params(param params)
  (cond
    ((no params)  ())
    ((not (consp params))   (list params)) ; rest
    ((iso param (car params))  ())
    (t  (cons (car params)
              (vars-in-optional-paramlist (cddr params)))))) ; skip default

(defun alref(key alist)
  (let ((foo (assoc key alist)))
    (if (consp foo)
      (cdr foo)
      ())))

; strip the colon
(defun keyword->symbol(k)
  (intern (symbol-name k)))

(defun optional-vars(vars)
  (if (consp vars)
    (alist (cdr (cut-at vars '(? &rest))))))

(defun strip-default-values(params)
  (if (consp params)
    (destructuring-bind (required optional rest) (_partition params '(? &rest))
      (if rest
        (append required (map 'list #'car (tuples optional 2)) '(&rest) rest)
        (append required (map 'list #'car (tuples optional 2)))))
    params))

(defun alref(key alist)
  (cdr (assoc key alist)))

(defun wc-restify(arglist)
  (if (no arglist)
      '()
      (if (atom arglist)
          `(&rest ,arglist)
          (compile-dots arglist))))

(defun compile-dots(xs)
  (mapcar (lambda(_)
            (if (consp _)
              (compile-dots _)
              _))
          (append (butlast xs)
                  (let ((x (last xs)))
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
  (let ((pos (position-first-available s delims)))
    (if pos
      (list (cut s 0 pos) (cut s pos))
      (list s nil))))

(defun position-first-available(s elems)
  (if (consp elems)
    (let ((pos (position (car elems) s)))
      (if pos
        pos
        (position-first-available s (cdr elems))))))

(defun cut-at(s delims)
  (let ((positions (map (type-of s) (lambda(x) (position x s)) delims)))
    (if (car positions)
      (apply #'cut s positions))))
