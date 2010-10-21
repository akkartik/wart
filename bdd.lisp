(defmacro test(msg Valueof expr Should &rest predicate)
  `(let ((got ,expr))
     (if (,(car predicate) got ,@(cdr predicate))
       (format t ". ~a~%" ,msg)
       (format t "F ~a~%  got ~a~%" ,msg got))))

(defmacro test-wc(msg Valueof expr Should &rest predicate)
  `(let ((got (eval (wc ',expr))))
     (if (,(car predicate) got ,@(cdr predicate))
       (format t ". ~a~%" ,msg)
       (format t "F ~a~%  got ~a~%" ,msg got))))

(defmacro pending-test(msg Valueof expr Should &rest predicate)
  `(format t "X ~a~%" ,msg))

(defmacro pending-test-wc(msg Valueof expr Should &rest predicate)
  `(format t "X ~a~%" ,msg))

(defmacro be(&rest args)
  `(equal ,@args))

(defun satisfy(x f)
  (funcall f x))
