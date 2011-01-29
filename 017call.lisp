; like funcall, but can handle macros
; knows about compose so that we can use ssyntax with macros
;   ++^h.1 => (call incf (call h 1))
; ssyntax doesn't seem a good fit for a lisp-2.
(defmacro call(f &rest args)
  (cond
    ((macp f)  `(,f ,@args))
    ((compose-form-p f)   (expand-composition f args))
    (t  `(call-fn (fslot ,f) ,@args))))



;; Internals

(defun compose-form-p(expr)
  (match expr '(compose _ _)))

(defun expand-composition(f args)
  (apply-nested-calls (compositions f) args))

(defun compositions(f)
  (if (compose-form-p f)
    (append (compositions (cadr f))
            (compositions (caddr f)))
    (list f))) ; Assumption: compose operates only on syms

(defun apply-nested-calls(fs args)
  (if (singlep fs)
    `(call ,(car fs) ,@args)
    `(call ,(car fs) ,(apply-nested-calls (cdr fs) args))))
