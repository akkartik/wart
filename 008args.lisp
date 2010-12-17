;; Functions support complex arg lists in wart.

(defmacro def(name params &rest body)
  `(defun ,name ,@(compile-params params body)))

(defmacro mac(name params &rest body)
  (wt-transform `(defmacro ,name ,@(compile-params params body))))

(defmacro fn(params &rest body)
  `(lambda ,@(compile-params params body)))

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
;; Use let* everywhere here because wart will soon override let

; 'first available' - like or, but a uses multiple values to indicate unavailable
(defmacro fa(a b)
  (let* ((val (uniq))
         (empty (uniq)))
    `(multiple-value-bind (,val ,empty) ,a
      (if ,empty
        ,b
        ,val))))

; returns arglist and body suitable for insertion into defun or lambda
; new body understands keyword args
; params format (optionals* ? lazy-optionals* . rest)
; optionals can be destructured
; lazy optionals require keywords if rest is present
(defun compile-params(params body)
  (let* ((ra  (uniq))
         (non-keyword-args  (uniq))
         (keyword-alist   (uniq))
         (optional-alist  (optional-alist params))
         (params-without-?  (params-without-defaults params))
         (rest-param  (rest-param params))
         (z   (getargs-exprs params-without-? non-keyword-args keyword-alist optional-alist)))
    `((&rest ,ra)
      (let* ((,non-keyword-args  (strip-keyword-args ,ra ',rest-param))
             (,keyword-alist   (keyword-args ,ra ',rest-param)))
          (let* ,z
            ,@body)))))

(defun getargs-exprs(params non-keyword-args keyword-alist optional-alist)
  (map 'list
       (lambda(param)
          (list param
                `(fa (get-arg ',param ',params ,non-keyword-args ,keyword-alist)
                     ,(alref param optional-alist))))
       (vars-in-paramlist params)))

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
    ((keywordp (car args))  (let* ((param (keyword->symbol (car args))))
                              (cons (cons param
                                          (if (iso param rest-param)
                                            (cdr args)
                                            (cadr args)))
                                    (keyword-args (cddr args) rest-param))))
    (t   (keyword-args (cdr args) rest-param))))

(defun strip-keyword-args(args rest-param)
  (cond
    ((no args)  ())
    ((keywordp (car args))  (if (iso (keyword->symbol (car args)) rest-param)
                              ()
                              (strip-keyword-args (cddr args) rest-param)))
    (t   (cons (car args) (strip-keyword-args (cdr args) rest-param)))))

(defun optional-alist(params)
  (partition-optional-params (strip-required (strip-rest params))))



;; Slicing and dicing params

(defun strip-required(params)
  (if (consp params)
    (let* ((optargs (member '? params)))
      (if optargs
        (cdr optargs)
        ()))
    params))

(defun rest-param(params)
  (cond
    ((no params)  ())
    ((rest-param-p params)  params)
    (t   (rest-param (cdr params)))))

(defun strip-rest(params)
  (cond
    ((no params) ())
    ((rest-param-p params)  ())
    (t   (cons (car params)
               (strip-rest (cdr params))))))

(defun undot(params)
  (cond
    ((no params)  ())
    ((rest-param-p params)  (list params)) ; undot
    (t (cons (car params)
             (undot (cdr params))))))

(defun strip-defaults(params)
  (cond
    ((no params)  ())
    ((rest-param-p params)  params)
    ((is '? (car params))   (really-strip-defaults (cdr params)))
    (t  (strip-defaults (cdr params)))))

; strip '? and defaults, undot rest args
(defun vars-in-paramlist(params)
  (cond
    ((no params)  ())
    ((rest-param-p params)  (list params))
    ((iso (car params) '?)   (vars-in-optional-paramlist (cdr params)))
    (t   (append (vars-in-paramlist (car params))
                 (vars-in-paramlist (cdr params))))))

(defun vars-in-optional-paramlist(params)
  (cond
    ((no params)  ())
    ((rest-param-p params)  (list params))
    (t (cons (car params)
             (vars-in-optional-paramlist (cddr params))))))

; like vars-in-paramlist, but don't undot rest args
(defun params-without-defaults(params)
  (cond
    ((no params)  ())
    ((rest-param-p params)  params)
    ((iso (car params) '?)   (optional-params-without-defaults (cdr params)))
    (t   (cons (car params)
               (params-without-defaults (cdr params))))))

(defun optional-params-without-defaults(params)
  (cond
    ((no params)  ())
    ((rest-param-p params)  params)
    (t  (cons (car params)
              (optional-params-without-defaults (cddr params))))))

(defun partition-optional-params(oparams)
  (cond
    ((not (consp oparams))  ())
    ((not (consp (cdr oparams)))  (list oparams))
    (t   (cons (cons (car oparams)
                     (cadr oparams))
               (partition-optional-params (cddr oparams))))))

(defun rest-param-p(params)
  (not (consp params)))

(defun vararg-param-p(params)
  (not (consp params)))

(defun really-strip-defaults(params)
  (cond
    ((no params)   ())
    ((rest-param-p params)  params)
    ((not (consp (car params)))   (cons (car params)
                                        (really-strip-defaults (cdr params))))
    (t  (cons (caar params)
              (really-strip-defaults (cdr params))))))



; strip the colon
(defun keyword->symbol(k)
  (if (is k ':do)
    'body
    (intern (symbol-name k))))

(defun alref(key alist)
  (cdr (assoc key alist)))
