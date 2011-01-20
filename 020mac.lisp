;; Macros with complex arg lists and o$vars params that are guaranteed to be eval'd exactly once.

(defmacro$ mac(name params &body body)
  (wt-transform
    (let* (($os   (remove-if-not #'odollar-symbol-p params))
           ($s  (mapcar #'odollar-symbol-to-dollar-symbol $os)))
      `(defmacro$ ,name(&rest ,$args)
         ,(compile-params params $args
            (list
              `(let* ,(mapcar #'list $s $os)
                 ,@body)))))))

(defun odollar-symbol-p(s)
  (and (symbolp s)
       (> (len (symbol-name s)) 2)
       (string= (symbol-name s) "O$"
                :start1 0 :end1 2)))

(defun odollar-symbol-to-dollar-symbol(s)
  (intern (cut (symbol-name s) 1)))

(prn (macex1 '(mac foofoo(n) `(+ ,n 1))))
(prn (macex1 '(mac foofoo(o$n) `(+ ,$n 1))))
(mac foofoo(n) `(+ ,n 1))
(prn (macex1 '(foofoo 3)))
