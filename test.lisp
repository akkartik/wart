(defmacro test(msg Valueof expr Should &rest predicate)
  `(let ((got ,expr))
     (unless (,(car predicate) got ,@(cdr predicate))
       (fail "F " ,msg #\newline "  got ")(writeln got))))

(defmacro test-wart(msg Valueof expr Should &rest predicate)
  `(let ((got (wt-eval ',expr)))
     (unless (,(car predicate) got ,@(cdr predicate))
       (fail "F " ,msg #\newline "  got ")(writeln got))))

(defmacro pending-test(msg &rest args)
  (prn #\newline "X " msg))
(defmacro pending-test-wart(msg &rest args)
  (prn #\newline "X " msg))



;; Useful inside tests

(defmacro be(&rest args)
  `(iso ,@args))

(defmacro satisfy(x f)
  `(funcall (function ,f) ,x))

(defun true_value(x)
  x)

; Use this in tests rather than let or let*, they will be overridden
(defmacro _let(var val &body body)
  `(funcall (lambda(,var) ,@body) ,val))



;; harness

(let ((test-failures 0))
  (defun print-failures()
    (cond
      ((> test-failures 1)
        (format t "~%~a failures~%" test-failures))
      ((> test-failures 0)
        (format t "~%~a failure~%" test-failures))))
  (defun fail(&rest args)
    (incf test-failures)
    (apply 'pr #\newline args)))

(let ((zxwf #\0))
  (defun test-heartbeat(x)
    (if (and (eq #\0 x)
             (not (eq #\0 zxwf)))
      (format t "~%"))
    (setf zxwf x)
    (format t "~a" x)
    (finish-output)))



(loop for path in (directory "./*.*") do
  (let ((file (file-namestring path)))
    (when (and (string< "" file)
               (char<= #\0 (char file 0))
               (char>= #\9 (char file 0)))
      (test-heartbeat (char file 1))
      (let* ((len (length file))
             (ext (subseq file (- len 4))))
        (cond
          ((equalp ext "lisp") (load file))
          ((equalp ext "wart") (wt-load file))
          ((equalp ext "test") (load file))
          ((equalp ext "wtst") (load file))))))) ; only so it can load after .wart

(print-failures)
