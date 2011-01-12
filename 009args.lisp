;; Functions support complex arg lists in wart.

(defmacro def(name params &rest body)
  `(defun ,name ,@(compile-params params body)))

(defmacro mac(name params &rest body)
  (wt-transform `(defmacro ,name ,@(compile-params params body))))

(defmacro fn(params &rest body)
  `(lambda ,@(compile-params params body)))



;; Internals
;; Use let* everywhere here because wart will soon override let

; returns arglist and body suitable for insertion into defun or lambda
; new body understands keyword args
; params format (optionals* ? lazy-optionals* . rest)
; optionals can be destructured
; lazy optionals require keywords if rest is present
(defun compile-params(params body)
  (let* ((args  (uniq))
         (positional  (uniq))
         (keywords   (uniq)))
    `((&rest ,args)
      (let* ((,positional  (positional-args ,args ',(rest-param params)))
             (,keywords   (keyword-args ,args ',(rest-param params))))
        (let* ,(append
                 (get-required-arg-exprs params positional keywords)
                 ; args go to rest before optional
                 (get-rest-arg-expr params positional keywords)
                 (get-optional-arg-exprs params positional keywords))
          ,@body)))))

(defun get-required-arg-exprs(params positional keywords)
  (let ((required-params (required-params params)))
    (map 'list
         (lambda(param)
           (list param
                 `(get-arg ',param ',required-params ,positional ,keywords)))
         (flatten required-params))))

(defun get-rest-arg-expr(params positional keywords)
  (let ((rest-param (rest-param params))
        (required-params (required-params params)))
    (if rest-param
      (list (list rest-param
                  ; return remaining positionals when runs out of params
                  `(get-arg ',rest-param ',required-params ,positional ,keywords :no-params (lambda(x) x)))))))

(defun get-optional-arg-exprs(params positional keywords)
  (let ((rest-param (rest-param params))
        (optional-alist (optional-alist params)))
    (map 'list
         (lambda(param)
           (list param
                 `(fa (get-arg ',param
                               (if (or (no ',rest-param) (assoc ',rest-param ,keywords))
                                 ',(strip-defaults params))
                               ,positional ,keywords)
                      ,(alref param optional-alist))))
         (map 'list 'car optional-alist))))

(defun get-arg(var params positional keywords &key (no-params (lambda(x) nil)))
  (cond
    ((assoc var keywords)  (alref var keywords))
    ((no params)  (values (call no-params positional)
                          'no-arg))
    ((is params var)  positional)
    ((not (consp params))   (values nil 'no-arg))
    ((assoc (car params) keywords)  (get-arg var (cdr params) positional keywords :no-params no-params))
    ((no positional)  (values nil 'no-arg))
    ((is (car params) var)  (car positional))
    (t   (fa (get-arg var (car params) (car positional) keywords :no-params no-params)
             (get-arg var (cdr params) (cdr positional) keywords :no-params no-params)))))



;; Slicing and dicing params

(defun required-params(params)
  (if (and (consp params)
           (not (is '? (car params))))
    (cons (car params)
          (required-params (cdr params)))))

(defun rest-param(params)
  (cond
    ((no params)  ())
    ((rest-param-p params)  params)
    (t   (rest-param (cdr params)))))

(defun keyword-args(args rest-param)
  (cond
    ((no args)  ())
    ((not (consp args))  ())
    ((keywordp (car args))  (let* ((param (keyword->symbol (car args))))
                              (cons (cons param
                                          (if (is param rest-param)
                                            (cdr args)
                                            (cadr args)))
                                    (keyword-args (cddr args) rest-param))))
    (t   (keyword-args (cdr args) rest-param))))

(defun positional-args(args rest-param)
  (cond
    ((no args)  ())
    ((keywordp (car args))  (if (is rest-param (keyword->symbol (car args)))
                              ()
                              (positional-args (cddr args) rest-param)))
    (t   (cons (car args)
               (positional-args (cdr args) rest-param)))))

(defun strip-defaults(params &optional past-?)
  (cond
    ((no params)  ())
    ((rest-param-p params)  params)
    ((is (car params) '?)   (strip-defaults (cdr params) t))
    (t   (cons (car params)
               (strip-defaults
                 (if past-? (cddr params) (cdr params))
                 past-?)))))

(defun optional-alist(params)
  (partition-optional-params (strip-required (strip-rest params))))

(defun partition-optional-params(oparams)
  (cond
    ((not (consp oparams))  ())
    ((not (consp (cdr oparams)))  (list oparams))
    (t   (cons (cons (car oparams)
                     (cadr oparams))
               (partition-optional-params (cddr oparams))))))

(defun strip-rest(params)
  (cond
    ((no params) ())
    ((rest-param-p params)  ())
    (t   (cons (car params)
               (strip-rest (cdr params))))))

(defun strip-required(params)
  (if (consp params)
    (let* ((optargs (member '? params))) ; strip-rest has already run
      (if optargs
        (cdr optargs)
        ()))
    params))

(defun rest-param-p(params)
  (not (consp params)))

; strip the colon
(defun keyword->symbol(k)
  (if (is k ':do)
    'body
    (intern (symbol-name k))))
