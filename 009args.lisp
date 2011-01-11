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
         (keyword-alist   (uniq)))
    `((&rest ,ra)
      (let* ((,non-keyword-args  (non-keyword-args ,ra ',(rest-param params)))
             (,keyword-alist   (keyword-alist ,ra ',(rest-param params))))
          (let* ,(append
                   (get-required-arg-exprs params non-keyword-args keyword-alist)
                   (get-rest-arg-expr params non-keyword-args keyword-alist)
                   (get-optional-arg-exprs params non-keyword-args keyword-alist))
            ,@body)))))

(defun get-required-arg-exprs(params non-keyword-args keyword-alist)
  (let ((required-params (required-params params)))
    (map 'list
         (lambda(param)
           (list param
                 `(get-arg ',param ',required-params ,non-keyword-args ,keyword-alist)))
         (flatten required-params))))

(defun get-rest-arg-expr(params non-keyword-args keyword-alist)
  (let ((rest-param (rest-param params)))
    (if rest-param
      (list (list rest-param `(get-greedy-rest-args ',rest-param ',(required-params params) ,non-keyword-args ,keyword-alist))))))

(defun get-optional-arg-exprs(params non-keyword-args keyword-alist)
  (let ((rest-param (rest-param params))
        (optional-alist (optional-alist params)))
    (map 'list
         (lambda(param)
           (list param
                 `(fa (get-arg ',param
                               (if (or (no ',rest-param) (assoc ',rest-param ,keyword-alist))
                                 ',(strip-defaults params))
                               ,non-keyword-args ,keyword-alist)
                      ,(alref param optional-alist))))
         (map 'list 'car optional-alist))))

(defun get-arg(var params non-keyword-args keyword-alist)
  (cond
    ((assoc var keyword-alist)  (alref var keyword-alist))
    ((no params)  (values nil 'no-arg))
    ((is params var)  non-keyword-args)
    ((not (consp params))   (values nil 'no-arg))
    ((assoc (car params) keyword-alist)  (get-arg var (cdr params) non-keyword-args keyword-alist))
    ((no non-keyword-args)  (values nil 'no-arg))
    ((is (car params) var)  (car non-keyword-args))
    (t   (fa (get-arg var (car params) (car non-keyword-args) keyword-alist)
             (get-arg var (cdr params) (cdr non-keyword-args) keyword-alist)))))

(defun get-greedy-rest-args(var params non-keyword-args keyword-alist)
  (cond
    ((assoc var keyword-alist)  (alref var keyword-alist))
    ((no params) non-keyword-args)
    ((is params var)  non-keyword-args)
    ((not (consp params))   (values nil 'no-arg))
    ((assoc (car params) keyword-alist)  (get-greedy-rest-args var (cdr params) non-keyword-args keyword-alist))
    ((no non-keyword-args)  (values nil 'no-arg))
    ((is (car params) var)  (car non-keyword-args))
    (t   (get-greedy-rest-args var (cdr params) (cdr non-keyword-args) keyword-alist))))



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

(defun keyword-alist(args rest-param)
  (cond
    ((no args)  ())
    ((not (consp args))  ())
    ((keywordp (car args))  (let* ((param (keyword->symbol (car args))))
                              (cons (cons param
                                          (if (is param rest-param)
                                            (cdr args)
                                            (cadr args)))
                                    (keyword-alist (cddr args) rest-param))))
    (t   (keyword-alist (cdr args) rest-param))))

(defun non-keyword-args(args rest-param)
  (cond
    ((no args)  ())
    ((keywordp (car args))  (if (is rest-param (keyword->symbol (car args)))
                              ()
                              (non-keyword-args (cddr args) rest-param)))
    (t   (cons (car args)
               (non-keyword-args (cdr args) rest-param)))))

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
