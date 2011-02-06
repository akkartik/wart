;; Functions with complex arg lists

(defmacro$ def(name params &body body)
  `(defun$ ,name(&rest ,$args)
     ,(compile-params params $args body)))

(defmacro$ fn(params &body body)
  `(lambda(&rest ,$args)
     ,(compile-params params $args body)))

(defmacro proc(name args . body)
  `(def ,name ,args ,@body nil))



;; Internals
;; Use let* everywhere here because wart will soon override let

; convert body to parse keyword args and params format (optionals* ? lazy-optionals . rest)
; optionals can be destructured
; lazy optionals alternate var and default
; lazy optionals require keywords if rest is present
(defun$ compile-params(params args body)
  (cond
    ((not params)  `(progn ,@body))
    ((not (consp params))   `(let* ((,params ,args)) ,@body))
    (t
      `(let* ((,$positionals  (positional-args ,args ',(rest-param params)))
              (,$keywords   (keyword-args ,args ',(rest-param params))))
        (let* ,(append
                 (get-required-arg-exprs params $positionals $keywords)
                 ; args go to rest before optional
                 (get-rest-arg-expr params $positionals $keywords)
                 (get-optional-arg-exprs params $positionals $keywords))
          ,@body)))))

(defun get-required-arg-exprs(params positionals keywords)
  (let ((required-params (required-params params)))
    (map 'list
         (lambda(param)
           (list param
                 `(get-arg ',param ',required-params ,positionals ,keywords)))
         (flat required-params))))

(defun get-rest-arg-expr(params positionals keywords)
  (let ((rest-param (rest-param params))
        (required-params (required-params params)))
    (when rest-param
      (list (list rest-param
                  ; return remaining positionals when runs out of params
                  `(get-arg ',rest-param ',required-params ,positionals ,keywords :no-params (lambda(x) x)))))))

(defun get-optional-arg-exprs(params positionals keywords)
  (let ((rest-param (rest-param params))
        (optional-alist (optional-alist params)))
    (map 'list
         (lambda(param)
           (list param
                 `(fa (get-arg ',param
                               (when (or (no ',rest-param) (assoc ',rest-param ,keywords))
                                 ',(strip-defaults params))
                               ,positionals ,keywords)
                      ,(alref param optional-alist))))
         (map 'list 'car optional-alist))))

(defun get-arg(var params positionals keywords &key (no-params (lambda(x) nil)))
  (cond
    ((assoc var keywords)  (alref var keywords))
    ((no params)  (values (call no-params positionals)
                          'no-arg))
    ((eq params var)  positionals)
    ((not (consp params))   (values nil 'no-arg))
    ((assoc (car params) keywords)  (get-arg var (cdr params) positionals keywords :no-params no-params))
    ((no positionals)  (values nil 'no-arg))
    ((eq (car params) var)  (car positionals))
    (t   (fa (get-arg var (car params) (car positionals) keywords :no-params no-params)
             (get-arg var (cdr params) (cdr positionals) keywords :no-params no-params)))))



(defun required-params(params)
  (when (and (consp params)
             (isnt '? (car params)))
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
                                          (if (eq param rest-param)
                                            (cdr args)
                                            (cadr args)))
                                    (keyword-args (cddr args) rest-param))))
    (t   (keyword-args (cdr args) rest-param))))

(defun positional-args(args rest-param)
  (cond
    ((no args)  ())
    ((keywordp (car args))  (if (eq rest-param (keyword->symbol (car args)))
                              ()
                              (positional-args (cddr args) rest-param)))
    (t   (cons (car args)
               (positional-args (cdr args) rest-param)))))

(defun strip-defaults(params &optional past-?)
  (cond
    ((no params)  ())
    ((rest-param-p params)  params)
    ((eq '? (car params))   (strip-defaults (cdr params) t))
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

(defun keyword->symbol(k)
  (if (eq k ':do)
    'body
    (intern (symbol-name k)))) ; strip the colon
