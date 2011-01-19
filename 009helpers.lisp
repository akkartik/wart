(defun macp(f)
  (and (symbolp f)
       (macro-function f)))

(defun idfn(x) x)

(defun isnt(x y) (no (is x y)))

(defmacro while(test &body body) ; define before we defover do
  `(loop while ,test do ,@body))

(defmacro thunk(&body body)
  `(lambda() ,@body))

(defmacro aand(&rest args)
  (cond
    ((no args)   t)
    ((no (cdr args))   (car args))
    (`(let ((it ,(car args)))   (and it (aand ,@(cdr args)))))))
