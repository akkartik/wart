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



;? (def foo(a b c ? d nil e 3 f (+ e 1) . rest) body)
;? =>
;? (defun foo(&rest args)
;?   (destructuring-bind (a b c &optional d (e 3) (f (+ e 1)) &rest rest) (reorder-keyword-args args)
;?     ..))

(defun compile-params2(params body)
  (if (or (not (consp params))
          (singlep params))
    ; only one kind of param; no need for distinguishing keyword args
    `(,params ,@body)
    (let* ((ra (uniq))
           (converted-params (convert-params params))
           (enumerated-params (enumerate-params converted-params)))
    `((&rest ,ra)
      (destructuring-bind ,converted-params (reorg-args ,ra ,enumerated-params)
        ,@body)))))

(defun convert-params(params)
  (cond
    ((no params)  nil)
    ((rest-param-p params)   (list '&rest params))
    ((is '? (car params))   (cons '&optional
                                  (convert-optional-params (cdr params))))
    (t  (cons (car params)
              (convert-params (cdr params))))))

; like convert-params, but pair optionals with defaults
(defun convert-optional-params(params)
  (cond
    ((no params)  nil)
    ((rest-param-p params)   (list '&rest params))
    (t  (cons (list (car params) (cadr params))
              (convert-optional-params (cddr params))))))

(defun enumerate-params(params)
  (cond
    ((no params)  nil)
    ((vararg-param-p params)   (list params))
    ((is '&optional (car params))   (enumerate-optional-params (cdr params)))
    ((is '&rest (car params))   (cdr params)) ; shortcut without error-checking
    (t  (cons (car params)
              (enumerate-params (cdr params))))))

(defun enumerate-optional-params(params)
  (cond
    ((no params)  nil)
    ((is '&rest (car params))   (cdr params)) ; shortcut without error-checking
    ((consp (car params))   (cons (caar params) ; strip defaults
                                  (enumerate-optional-params (cdr params))))
    (t  (cons (car params)
              (enumerate-optional-params (cdr params))))))

(defun rest-param-p(params)
  (not (consp params)))

(defun vararg-param-p(params)
  (not (consp params)))

(defun reorg-args(args params)
  (if (no-keywords args)
    args
    (let* ((rest-param (rest-param params))
           (keyword (keyword-args args rest-param))
           (nonkeyword (strip-keyword-args args rest-param)))
      (generate-args params nonkeyword keyword))))

(defun no-keywords(args)
  (not (some #'keywordp args)))

(defun parse-params(params)
  (let ((optional-index (position '? params))
        (rest-index (position '&rest params))
        (len (length params)))
    (list (subseq params 0 (or optional-index rest-index len))
          (if optional-index
            (pair (subseq params (1+ optional-index) (or rest-index len))))
          (if rest-index
            (elt params (1+ rest-index))))))

(defun generate-args(req nonk key)
  (when req
    (if (alref (car req) key)
      (cons (alref (car req) key)
            (generate-args (cdr req) nonk key))
      (cons (car nonk)
            (generate-args (cdr req) (cdr nonk) key)))))

;? ; this won't work because the arg list is quoted. perhaps we can just eval?
;? (defun foo(&rest args)
;?   (destructuring-bind (a b c d e f &rest rest) (reorg-args args '(a b c ? d nil e 3 f (+ e 1)))
;?     ..))

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
         (z   (getargs-exprs params-without-? non-keyword-args keyword-alist optional-alist)))
    `((&rest ,ra)
      ; let* because wart will soon override let
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

; 'first available' - like or, but a uses multiple values to indicate unavailable
(defmacro fa(a b)
  (let ((val (uniq))
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
  (let ((tmp (assoc key alist)))
    (if (consp tmp)
      (cdr tmp)
      nil)))

; strip the colon
(defun keyword->symbol(k)
  (if (is k ':do)
    'body
    (intern (symbol-name k))))

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
