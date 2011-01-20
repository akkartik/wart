;; Macros with complex arg lists and o$vars params that are guaranteed to be eval'd exactly once.

(defmacro$ mac0(name params &body body)
  `(defmacro$ ,name(&rest ,$args)
     ,(compile-params params $args body)))

(defmacro$ mac(name params &body body)
  (wt-transform
    (let* (($os   (remove-if-not #'odollar-symbol-p (strip-rest params)))
           ($s  (mapcar #'odollar-symbol-to-dollar-symbol $os)))
      `(mac0 ,name ,params
         `(let ,(mapcar #'list (list ,@$s) (list ,@$os))
            ,(progn ,@body))))))

(defun odollar-symbol-p(s)
  (and (symbolp s)
       (> (len (symbol-name s)) 2)
       (string= (symbol-name s) "O$"
                :start1 0 :end1 2)))

(defun odollar-symbol-to-dollar-symbol(s)
  (intern (cut (symbol-name s) 1)))
