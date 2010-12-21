(setf *test-failures* 0)

(defmacro test(msg Valueof expr Should &rest predicate)
  `(let ((got ,expr))
     (unless (,(car predicate) got ,@(cdr predicate))
       (incf *test-failures*)
       (prn "F " ,msg #\newline "  got " got))))

(defmacro test-wart(msg Valueof expr Should &rest predicate)
  `(let ((got (wt-eval ',expr)))
     (unless (,(car predicate) got ,@(cdr predicate))
       (incf *test-failures*)
       (prn "F " ,msg #\newline "  got " got))))

(defmacro pending-test(msg Valueof expr Should &rest predicate)
  `(prn "X " ,msg))

(defmacro pending-test-wart(msg Valueof expr Should &rest predicate)
  `(pending-test ,msg ,Valueof ,expr ,Should ,@predicate))

(defmacro be(&rest args)
  `(iso ,@args))

(defmacro satisfy(x f)
  `(call (function ,f) ,x))

(defun true_value(x)
  x)

; Use this in tests rather than let or let*, they will be overridden
(defmacro _let(var val &body body)
  `(call (lambda(,var) ,@body) ,val))



(loop for path in (directory "./*.*") do
  (let ((file (file-namestring path)))
    (when (and (string< "" file)
               (char<= #\0 (char file 0))
               (char>= #\9 (char file 0)))
      (let* ((len (length file))
             (ext (subseq file (- len 4))))
        (cond
          ((equalp ext "lisp") (load file))
          ((equalp ext "wart") (wt-load file))
          ((equalp ext "test") (load file))
          ((equalp ext "wtst") (load file)))))))

(cond
  ((> *test-failures* 1)
    (format t "~%~a failures~%" *test-failures*))
  ((> *test-failures* 0)
    (format t "~%~a failure~%" *test-failures*)))
