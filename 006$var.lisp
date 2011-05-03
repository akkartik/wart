;; Implicit gensyms: http://letoverlambda.com/index.cl/guest/chap3.html#sec_5
;; Wart uses ! for ssyntax like arc, so instead of g! we use $ to denote gensyms.

(defmacro defmacro$(name args &rest body)
  `(defmacro ,name ,args
     (handling-$vars ,@body)))

; Use defun$ to generate expressions for defmacro$ to return
(defmacro defun$(name args &rest body)
  `(defun ,name ,args
     (handling-$vars ,@body)))



;; Internals

(defmacro handling-$vars(&body body)
  (let ((syms (remove-duplicates
                (remove-if-not #'dollar-symbol-p
                               (flat body)))))
    `(let ,(mapcar (lambda(_)
                     `(,_   (uniq ,(cut (symbol-name _) 1))))
                   syms)
       ,@body)))

(defun dollar-symbol-p(s)
  (and (symbolp s)
       (> (len (symbol-name s)) 1)
       (string= (symbol-name s) "$"
                :start1 0 :end1 1)))
