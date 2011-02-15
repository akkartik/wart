;; Debug utils

(synonym err error)
(macro-alias errsafe ignore-errors)

(synonym macex macroexpand
         macex1 macroexpand-1)

(defun pr1(arg)
  (format t "~a" arg))

(defun pr(&rest args)
  (map 'list 'pr1 args)
  (car args))

(defun prn(&rest args)
  (apply 'pr args)
  (format t "~%")
  (car args))

(defun prrn(&rest args)
  (map 'list 'pr args)
  (prn #\return)
  (car args))

(defun writeln(&rest args)
  (map 'list 'write args)
  (format t "~%")
  (car args))

(require :sb-grovel)
