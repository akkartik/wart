;; Macros with complex arg lists and o$vars params that are guaranteed to be eval'd exactly once.

;? (defmacro$ mac(name params &rest body)
;?   (wt-transform
;?     `(defmacro$ ,name(&rest ,$args)
;?        ,(compile-params params body $args))))
;? 
; =>
;?   (defmacro$ foofoo (&rest args911)
;?              (let* ((positionals912 (positional-args args911 'nil))
;?                     (keywords913 (keyword-args args911 'nil)))
;?                (let* ((o$x (get-arg 'o$x '(o$x) positionals912 keywords913)))
;?                  `(+ ,$x 1))))

(defun odollar-symbol-p(s)
  (and (symbolp s)
       (> (len (symbol-name s)) 2)
       (string= (symbol-name s) "O$"
                :start1 0 :end1 2)))

(defun odollar-symbol-to-dollar-symbol(s)
  (intern (cut (symbol-name s) 1)))

;? (defmacro$ mac(name params &rest body)
;?   (wt-transform
;?     `(defmacro$ ,name(&rest ,$args)
;?        ,(compile-oparams params body $args))))

; like compile-params, but also handles o$vars

;? (defun$ compile-oparams(params body args)
;?   `(let* ((,$positionals  (positional-args ,args ',(rest-param params)))
;?           (,$keywords   (keyword-args ,args ',(rest-param params))))
;?     (let* ,(append
;?              (get-required-arg-exprs params $positionals $keywords)
;?              ; args go to rest before optional
;?              (get-rest-arg-expr params $positionals $keywords)
;?              (get-optional-arg-exprs params $positionals $keywords))
;?       (let* ((,$os  (remove-if-not #'odollar-symbol-p ',params))
;?              (,$s  (mapcar #'odollar-symbol-to-dollar-symbol ,$os)))
;?         `(let* ,(mapcar #'list (list $s) (list $os))
;?           ,@body)))))
;? 
; => 
;? (defmacro$ foofoo (&rest args911)
;?            (let* ((positionals912 (positional-args args911 'nil))
;?                   (keywords913 (keyword-args args911 'nil)))
;?              (let* ((o$x (get-arg 'o$x '(o$x) positionals912 keywords913)))
;?                (let* ((os915 (remove-if-not #'odollar-symbol-p '(o$x)))
;?                       (s914 (mapcar #'odollar-symbol-to-dollar-symbol os915)))
;?                  `(let* (backq-comma (mapcar #'list (list $s) (list $os)))
;?                     ,@body)))))

;? (defun$ compile-oparams(params body args)
;?   `(let* ((,$positionals  (positional-args ,args ',(rest-param params)))
;?           (,$keywords   (keyword-args ,args ',(rest-param params))))
;?     (let* ,(append
;?              (get-required-arg-exprs params $positionals $keywords)
;?              ; args go to rest before optional
;?              (get-rest-arg-expr params $positionals $keywords)
;?              (get-optional-arg-exprs params $positionals $keywords))
;?       (let* ((,$os  (remove-if-not #'odollar-symbol-p ',params))
;?              (,$s  (mapcar #'odollar-symbol-to-dollar-symbol ,$os)))
;?         `(let* ,,(mapcar #'list (list $s) (list $os))
;?           ,@body)))))
; => 
;? (defmacro$ foofoo (&rest args911)
;?            (let* ((positionals912 (positional-args args911 'nil))
;?                   (keywords913 (keyword-args args911 'nil)))
;?              (let* ((o$x (get-arg 'o$x '(o$x) positionals912 keywords913)))
;?                (let* ((os915 (remove-if-not #'odollar-symbol-p '(o$x)))
;?                       (s914 (mapcar #'odollar-symbol-to-dollar-symbol os915)))
;?                  `(let* (backq-comma ((s914 os915)))
;?                     ,@body)))))

;? (defun$ compile-oparams(params body args)
;?   `(let* ((,$positionals  (positional-args ,args ',(rest-param params)))
;?           (,$keywords   (keyword-args ,args ',(rest-param params))))
;?     (let* ,(append
;?              (get-required-arg-exprs params $positionals $keywords)
;?              ; args go to rest before optional
;?              (get-rest-arg-expr params $positionals $keywords)
;?              (get-optional-arg-exprs params $positionals $keywords))
;?       (let* ((,$os  (remove-if-not #'odollar-symbol-p ',params))
;?              (,$s  (mapcar #'odollar-symbol-to-dollar-symbol ,$os)))
;?         (cons 'let* (cons ',(mapcar #'list (list $s) (list $os))
;?           ',body))))))
; =>
;? (defmacro$ foofoo (&rest args911)
;?            (let* ((positionals912 (positional-args args911 'nil))
;?                   (keywords913 (keyword-args args911 'nil)))
;?              (let* ((o$x (get-arg 'o$x '(o$x) positionals912 keywords913)))
;?                (let* ((os915 (remove-if-not #'odollar-symbol-p '(o$x)))
;?                       (s914 (mapcar #'odollar-symbol-to-dollar-symbol os915)))
;?                  (cons 'let* (cons '((s914 os915)) '(`(+ ,$x 1))))))))
;? (let* ((s919 os920))
;?   `(+ ,$x 1))

(defmacro compile-oparams-1(let-exprs body)
  (prn let-exprs)
  `(let* ,let-exprs ,@body))

(defmacro$ mac(name params &rest body)
  (wt-transform
    (let* (($os  (remove-if-not #'odollar-symbol-p params))
           ($s   (mapcar #'odollar-symbol-to-dollar-symbol $os)))
    `(defmacro$ ,name(&rest ,$args)
      (let* ((,$positionals  (positional-args ,$args ',(rest-param params)))
             (,$keywords   (keyword-args ,$args ',(rest-param params))))
        (let* ,(append
                 (get-required-arg-exprs params $positionals $keywords)
                 ; args go to rest before optional
                 (get-rest-arg-expr params $positionals $keywords)
                 (get-optional-arg-exprs params $positionals $keywords))
          (compile-oparams-1 ,(mapcar #'list $s $os)
            ,@body)))))))

;? (defun$ compile-oparams(params body args)
;?   (let* (($os  (remove-if-not #'odollar-symbol-p params))
;?          ($s   (mapcar #'odollar-symbol-to-dollar-symbol $os)))
;?   `(let* ((,$positionals  (positional-args ,args ',(rest-param params)))
;?           (,$keywords   (keyword-args ,args ',(rest-param params))))
;?     (let* ,(append
;?              (get-required-arg-exprs params $positionals $keywords)
;?              ; args go to rest before optional
;?              (get-rest-arg-expr params $positionals $keywords)
;?              (get-optional-arg-exprs params $positionals $keywords))
;?         (cons 'let* (cons ',(mapcar #'list $s $os)
;?           ',body))))))
; =>
;? (defmacro$ foofoo (&rest args911)
;?            (let* ((positionals912 (positional-args args911 'nil))
;?                   (keywords913 (keyword-args args911 'nil)))
;?              (let* ((o$x (get-arg 'o$x '(o$x) positionals912 keywords913)))
;?                (cons 'let* (cons '(($x o$x)) '(`(+ ,$x 1)))))))
;? (let* (($x o$x))
;?   `(+ ,$x 1))

;? (defun$ compile-oparams(params body args)
;?         (prn body)
;?   `(let* ((,$positionals  (positional-args ,args ',(rest-param params)))
;?           (,$keywords   (keyword-args ,args ',(rest-param params))))
;?     (let* ,(append
;?              (get-required-arg-exprs params $positionals $keywords)
;?              ; args go to rest before optional
;?              (get-rest-arg-expr params $positionals $keywords)
;?              (get-optional-arg-exprs params $positionals $keywords))
;?       (let* ((,$os  (remove-if-not #'odollar-symbol-p ',params))
;?              (,$s   (mapcar #'odollar-symbol-to-dollar-symbol ,$os))
;?              (,$z   (mapcar #'list (list $s) (list $os))))
;?         (cons 'let* (cons ,$z ',body))))))
;? 
; =>
;? (defmacro$ foofoo (&rest args911)
;?            (let* ((positionals912 (positional-args args911 'nil))
;?                   (keywords914 (keyword-args args911 'nil)))
;?              (let* ((o$x (get-arg 'o$x '(o$x) positionals912 keywords913)))
;?                (let* ((os915 (remove-if-not #'odollar-symbol-p '(o$x)))
;?                       (s914 (mapcar #'odollar-symbol-to-dollar-symbol os915))
;?                       (z916 (mapcar #'list (list $s) (list $os))))
;?                  (cons 'let* (cons z916 '(`(+ ,$x 1))))))))

;? Desired:
;?   (defmacro$ foofoo (&rest args911)
;?              (let* ((positionals912 (positional-args args911 'nil))
;?                     (keywords913 (keyword-args args911 'nil)))
;?                (let* ((o$x (get-arg 'o$x '(o$x) positionals912 keywords913)))
;?                  `(let* (($x o$x))
;?                    (+ ,$x 1)))))

;? (defun$ compile-oparams(params body args)
;?         (prn body)
;?   `(let* ((,$positionals  (positional-args ,args ',(rest-param params)))
;?           (,$keywords   (keyword-args ,args ',(rest-param params))))
;?     (let* ,(append
;?              (get-required-arg-exprs params $positionals $keywords)
;?              ; args go to rest before optional
;?              (get-rest-arg-expr params $positionals $keywords)
;?              (get-optional-arg-exprs params $positionals $keywords))
;?       (let* ((,$os  (remove-if-not #'odollar-symbol-p ',params))
;?              (,$s   (mapcar #'odollar-symbol-to-dollar-symbol ,$os))
;?              (,$z   (mapcar #'list (list $s) (list $os))))
;?         (cons 'let* (cons ,$z ',@body))))))

;? (defun$ compile-oparams(params body args)
;?         (prn body)
;?   `(let* ((,$positionals  (positional-args ,args ',(rest-param params)))
;?           (,$keywords   (keyword-args ,args ',(rest-param params))))
;?     (let* ,(append
;?              (get-required-arg-exprs params $positionals $keywords)
;?              ; args go to rest before optional
;?              (get-rest-arg-expr params $positionals $keywords)
;?              (get-optional-arg-exprs params $positionals $keywords))
;?       (let* ((,$os  (remove-if-not #'odollar-symbol-p ',params))
;?              (,$s   (mapcar #'odollar-symbol-to-dollar-symbol ,$os))
;?              (,$z   (mapcar #'list (list $s) (list $os))))
;?         `(let* ,,$z
;?           ,,@body)))))

;? (prn (macex1 '(mac foofoo(o$x) `(+ ,$x 1))))
;? (mac foofoo(o$x) `(+ ,$x 1))
;? (prn (macex1 '(foofoo 3)))
;? ;? (prn (compile-oparams '(o$x) '(`(+ ,$x 1)) 'args))
;? ;? (prn (compile-oparams '(n) '(`(+ ,n 1)) 'args))
(prn (macex1 '(mac foofoo(n) `(+ ,n 1))))
(prn (macex '(mac foofoo(n) `(+ ,n 1))))
