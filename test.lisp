(defmacro test(msg Valueof expr Should &rest predicate)
  `(let ((got ,(if (fboundp 'wt-eval)
                 `(wt-eval ',expr)
                 expr)))
     (or (,(car predicate) got ,@(cdr predicate))
         (fail ,msg got))))

(defmacro test-lisp(msg Valueof expr Should &rest predicate)
  `(let ((got ,expr))
     (or (,(car predicate) got ,@(cdr predicate))
         (fail ,msg got))))

(defmacro pending-test(msg &rest args)
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
  (defun print-test-failures()
    (cond
      ((> test-failures 1)
        (format t "~%~a failures~%" test-failures))
      ((> test-failures 0)
        (format t "~%~a failure~%" test-failures))
      (:else
        (format t "[1K"))))

  (defun fail(msg got)
    (incf test-failures)
    (prn #\newline "F " msg)
    (pr "  got ") (writeln got)))

(loop for path in (directory "./*.*") do
  (let ((file (file-namestring path)))
    (when (and (string< "" file)
               (char<= #\0 (char file 0))
               (char>= #\9 (char file 0)))
      (format t ".") (finish-output)
      (let* ((len (length file))
             (ext (subseq file (- len 4))))
        (cond
          ((equalp ext "lisp") (load file))
          ((equalp ext "wart") (wt-load file))
          ((equalp ext "test") (load file))
          ((equalp ext "wtst") (load file))))))) ; only so it can load after .wart
(print-test-failures)
