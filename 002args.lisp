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

(defun firstn (n xs)
  (if (or (= n 0) (no xs))
      nil
      (cons (car xs) (firstn (1- n) (cdr xs)))))



;; Internals

(defmacro wc-complex-bind(vars vals &body body)
  (let* ((restified-vars (wc-restify vars))
        (simplified-vars (strip-default-values restified-vars)))
    `(destructuring-bind (positional-vals keyword-alist)
                         (partition-keywords ,vals (rest-var ',restified-vars))
      (let ((optional-alist (optional-vars ',restified-vars)))
        (wc-destructuring-bind ,simplified-vars
            (merge-keyword-vars positional-vals keyword-alist optional-alist
                                ',simplified-vars)
          ,@body)))))

(defmacro wc-destructuring-bind(vars vals &body body)
  (let ((gval (uniq)))
    `(let ((,gval ,vals))
       (wc-destructuring-bind-1 ,vars ,gval ,@body))))
(defmacro wc-destructuring-bind-1(vars vals &body body)
  (cond ((no vars)  `(progn ,@body))
        ((atom vars)  `(let ((,vars ,vals)) ,@body))
        ((is '&rest (car vars))  `(wc-destructuring-bind-1 ,(cadr vars) ,vals
                                     ,@body))
        (t `(wc-destructuring-bind-1 ,(car vars) (car ,vals)
                  (wc-destructuring-bind-1 ,(cdr vars) (cdr ,vals)
                        ,@body)))))

; handles destructuring, . for rest
; returns a list whose first element is a common lisp lambda-list
; (cltl2, 5.2.2)
(defun compile-params(params body)
  (let ((params   (wc-restify params))
        (gparams  (uniq)))
    `((&rest ,gparams)
       (wc-complex-bind ,params ,gparams
         ,@body))))

(defun partition-keywords(vals &optional rest-var alist)
  (if (consp vals)
    (if (keywordp (car vals))
      (if (is rest-var (keyword->symbol (car vals)))
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
    (if (is '&rest (car vars))
      (cadr vars)
      (rest-var (cdr vars)))))

; strip the colon
(defun keyword->symbol(k)
  (intern (symbol-name k)))

(defun merge-keyword-vars(positional-vals keyword-alist optional-alist params)
  (cond
    ((no params)  nil)
    ; dotted rest
    ((atom params)   (or positional-vals (eval (alref params optional-alist))))
    ((is '&rest (car params))   (or positional-vals
                                  (alref (cadr params) keyword-alist)
                                  (eval (alref (cadr params) optional-alist))))
    ((assoc (car params) keyword-alist)  (cons (alref (car params) keyword-alist)
                                             (merge-keyword-vars positional-vals
                                                                 keyword-alist
                                                                 optional-alist
                                                                 (cdr params))))
    ((no positional-vals)  (cons (eval (alref (car params) optional-alist))
                                 (merge-keyword-vars nil keyword-alist optional-alist (cdr params))))
    (t  (cons (car positional-vals)
              (merge-keyword-vars (cdr positional-vals) keyword-alist optional-alist
                                  (cdr params))))))

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

(defun wc-restify (arglist)
  (if (no arglist)
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
