; like funcall, but can handle macros
; knows about compose so that we can use ssyntax with macros
;   ++^h.1 => (call incf (call h 1))
; ssyntax doesn't seem a good fit for a lisp-2.
(defmacro call(f &rest args)
  `(call-fn (fslot ,f) ,@args))

(extend-macro call(f &rest args) :if (macp f)
  `(,f ,@args))

(extend-macro call(f &rest args) :if (match f '(compose _ _))
  (apply-nested-calls (compositions f) args))



;; Internals

(defun compositions(f)
  (if (match f '(compose _ _))
    (append (compositions (cadr f))
            (compositions (caddr f)))
    (list f))) ; Assumption: compose operates only on syms

(defun apply-nested-calls(fs args)
  (if (singlep fs)
    `(call ,(car fs) ,@args)
    `(call ,(car fs) ,(apply-nested-calls (cdr fs) args))))
