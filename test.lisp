(setf *test-failures* 0)

(defmacro test(msg Valueof expr Should &rest predicate)
  `(let ((got ,expr))
     (unless (,(car predicate) got ,@(cdr predicate))
       (incf *test-failures*)
       (format t "F ~a~%  got ~a~%" ,msg got))))

(defmacro test-wc(msg Valueof expr Should &rest predicate)
  `(let ((got (eval (wc ',expr))))
     (unless (,(car predicate) got ,@(cdr predicate))
       (incf *test-failures*)
       (format t "F ~a~%  got ~a~%" ,msg got))))

(defmacro pending-test(msg Valueof expr Should &rest predicate)
  `(format t "X ~a~%" ,msg))

(defmacro pending-test-wc(msg Valueof expr Should &rest predicate)
  `(format t "X ~a~%" ,msg))

(defmacro be(&rest args)
  `(iso ,@args))

(defun satisfy(x f)
  (call f x))



(format t "~%")

(loop for path in (directory "./*.*") do
  (let ((file (file-namestring path)))
    (when (and (string< "" file)
               (char<= #\0 (char file 0))
               (char>= #\9 (char file 0)))
      (let* ((len (length file))
             (ext (subseq file (- len 4))))
        (cond
          ((iso ext "test") (load file)))))))

(cond
  ((> *test-failures* 1)
    (format t "~%~a failures~%" *test-failures*))
  ((> *test-failures* 0)
    (format t "~%~a failure~%" *test-failures*))
  (t
    (format t "~%")))
