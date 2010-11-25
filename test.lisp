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
  `(iso ,@args))

(defun satisfy(x f)
  (call f x))



(loop for path in (directory "./*.*") do
  (let ((file (file-namestring path)))
    (when (and (string< "" file)
               (char<= #\0 (char file 0))
               (char>= #\9 (char file 0)))
      (let* ((len (length file))
             (ext (subseq file (- len 4))))
        (cond
          ((iso ext "test") (load file)))))))
