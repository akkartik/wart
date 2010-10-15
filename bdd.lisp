(defmacro test(msg Valueof expr Should &rest predicate)
  `(let ((got (eval (wc ',expr))))
     (if (,(car predicate) got ,@(cdr predicate))
       (format t ". ~a~%" ,msg)
       (format t "F ~a~%  got ~a~%" ,msg got))))

(defmacro be(&rest args)
  `(equal ,@args))

(defun bdd1() 3)
(test "test framework works"
  :valueof (bdd1)
  :should be 3)

(defun bdd1() 4)
(test "test framework reloads functions"
  :valueof (bdd1)
  :should > 3)
