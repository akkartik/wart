;; Implicit gensyms: http://letoverlambda.com/index.cl/guest/chap3.html#sec_5
;; Wart uses ! for ssyntax like arc, so instead of g! and o! we use $ and o$
;; prefixes to denote gensyms and once-only syms.

(defun dollar-symbol-p(s)
  (and (symbolp s)
       (> (len (symbol-name s)) 1)
       (string= (symbol-name s) "$"
                :start1 0 :end1 1)))

(defun odollar-symbol-p(s)
  (and (symbolp s)
       (> (len (symbol-name s)) 2)
       (string= (symbol-name s) "O$"
                :start1 0 :end1 2)))

(defun odollar-symbol-to-dollar-symbol(s)
  (intern (cut (symbol-name s) 1)))

(defmacro defmacro/$(name args &rest body)
  (let ((syms (remove-duplicates
                (remove-if-not #'dollar-symbol-p
                               (flatten body)))))
    `(defmacro ,name ,args
       (let ,(mapcar (lambda(_)
                       `(,_   (uniq ,(cut (symbol-name _) 1))))
                     syms)
         ,@body))))

(defmacro defmacro$(name args &rest body)
  (let* ((os (remove-if-not #'odollar-symbol-p args))
         ($s (mapcar #'odollar-symbol-to-dollar-symbol os)))
    `(defmacro/$ ,name ,args
       `(let ,(mapcar #'list (list ,@$s) (list ,@os))
          ,(progn ,@body)))))

; Use defun$ to generate expressions for defmacro$ to return
(defmacro defun$(name args &rest body)
  (let ((syms (remove-duplicates
                (remove-if-not #'dollar-symbol-p
                               (flatten body)))))
    `(defun ,name ,args
       (let ,(mapcar (lambda(_)
                       `(,_   (uniq ,(cut (symbol-name _) 1))))
                     syms)
         ,@body))))
