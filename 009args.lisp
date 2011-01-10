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
  (let* ((ra  (uniq))
         (non-keyword-args  (uniq))
         (keyword-alist   (uniq))
         (rest-param  (rest-param params)))
    `((&rest ,ra)
      (let* ((,non-keyword-args  (strip-keyword-args ,ra ',rest-param))
             (,keyword-alist   (keyword-args ,ra ',rest-param)))
          (let* ,(getargs-exprs params non-keyword-args keyword-alist)
            ,@body)))))

(defun getargs-exprs(params non-keyword-args keyword-alist)
  (let ((required-params  (required-params params))
        (optional-params  (optional-params params))
        (optional-alist   (optional-alist params))
        (rest-param   (rest-param params)))
    (append
      (map 'list
           (lambda(param)
             (list param
                   `(get-arg ',param ',required-params ,non-keyword-args ,keyword-alist)))
           (flatten required-params))
      (if rest-param
        (list (list rest-param `(get-greedy-rest-args ',rest-param ',required-params ,non-keyword-args ,keyword-alist))))
      (map 'list
           (lambda(param)
             (list param
                   `(fa (if (or (no ',rest-param) (assoc ',rest-param ,keyword-alist))
                          (get-arg ',param ',(strip-defaults params) ,non-keyword-args ,keyword-alist)
                          (get-lazy-optional-arg ',param ,keyword-alist))
                        ,(alref param optional-alist))))
           optional-params))))

(defun get-arg(var params arglist keyword-alist)
  (cond
    ((assoc var keyword-alist)  (alref var keyword-alist))
    ((no params)  (values nil 'no-arg))
    ((is params var)  arglist)
    ((not (consp params))   (values nil 'no-arg))
    ((assoc (car params) keyword-alist)  (get-arg var (cdr params) arglist keyword-alist))
    ((no arglist)  (values nil 'no-arg))
    (t   (fa (get-arg var (car params) (car arglist) keyword-alist)
             (get-arg var (cdr params) (cdr arglist) keyword-alist)))))

(defun get-lazy-optional-arg(var keyword-alist)
  (if (assoc var keyword-alist)
    (alref var keyword-alist)
    (values nil 'no-arg)))

(defun get-greedy-rest-args(var params arglist keyword-alist)
  (cond
    ((assoc var keyword-alist)  (alref var keyword-alist))
    ((is params var)  arglist)
    ((no params) arglist)
    ((assoc (car params) keyword-alist)  (get-greedy-rest-args var (cdr params) arglist keyword-alist))
    (t   (get-greedy-rest-args var (cdr params) (cdr arglist) keyword-alist))))



;; Slicing and dicing params

(defun required-params(params)
  (if (and (consp params)
           (not (is '? (car params))))
    (cons (car params)
          (required-params (cdr params)))))

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

(defun strip-keyword-args(args rest-param)
  (cond
    ((no args)  ())
    ((keywordp (car args))  (if (is rest-param (keyword->symbol (car args)))
                              ()
                              (strip-keyword-args (cddr args) rest-param)))
    (t   (cons (car args)
               (strip-keyword-args (cdr args) rest-param)))))

(defun optional-params(params)
  (map 'list 'car
       (tuples 2 (strip-required (strip-rest params)))))

(defun optional-alist(params)
  (partition-optional-params (strip-required (strip-rest params))))

(defun strip-defaults(params)
  (cond
    ((no params)  ())
    ((rest-param-p params)  params)
    ((is (car params) '?)   (really-strip-defaults (cdr params)))
    (t   (cons (car params)
               (strip-defaults (cdr params))))))

(defun really-strip-defaults(params)
  (cond
    ((no params)  ())
    ((rest-param-p params)  params)
    (t  (cons (car params)
              (really-strip-defaults (cddr params))))))

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

(defun flatten(tree)
  (let ((result '()))
    (labels ((scan (item)
               (if (listp item)
                 (map nil #'scan item)
                 (push item result))))
      (scan tree))
    (nreverse result)))

; strip the colon
(defun keyword->symbol(k)
  (if (is k ':do)
    'body
    (intern (symbol-name k))))

(defun alref(key alist)
  (cdr (assoc key alist)))
