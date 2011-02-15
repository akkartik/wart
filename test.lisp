(setf *test-failures* 0)

(defmacro test(msg Valueof expr Should &rest predicate)
  `(let ((got ,expr))
     (unless (,(car predicate) got ,@(cdr predicate))
       (incf *test-failures*)
       (pr #\newline "F " ,msg #\newline "  got ")(writeln got))))

(defmacro test-wart(msg Valueof expr Should &rest predicate)
  `(let ((got (wt-eval ',expr)))
     (unless (,(car predicate) got ,@(cdr predicate))
       (incf *test-failures*)
       (pr #\newline "F " ,msg #\newline "  got ")(writeln got))))

(defmacro pending-test(msg &rest args)
  (prn #\newline "X " msg))
(defmacro pending-test-wart(msg &rest args)
  (prn #\newline "X " msg))

(defmacro be(&rest args)
  `(iso ,@args))

(defmacro satisfy(x f)
  `(funcall (function ,f) ,x))

(defun true_value(x)
  x)

; Use this in tests rather than let or let*, they will be overridden
(defmacro _let(var val &body body)
  `(funcall (lambda(,var) ,@body) ,val))



(loop for path in (directory "./*.*") do
  (let ((file (file-namestring path)))
    (when (and (string< "" file)
               (char<= #\0 (char file 0))
               (char>= #\9 (char file 0)))
      (format t "~a" (char file 1))(finish-output)
      (let* ((len (length file))
             (ext (subseq file (- len 4))))
        (cond
          ((equalp ext "lisp") (load file))
          ((equalp ext "wart") (wt-load file))
          ((equalp ext "test") (load file))
          ((equalp ext "wtst") (load file))))))) ; only so it can load after .wart

(cond
  ((> *test-failures* 1)
    (format t "~%~a failures~%" *test-failures*))
  ((> *test-failures* 0)
    (format t "~%~a failure~%" *test-failures*)))
