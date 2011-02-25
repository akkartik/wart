(defun compose-fn(f g)
  (lambda(&rest args)
    (call-fn f (wart-apply g args))))

(defmacro compose(f g)
  `(compose-fn (fslot ,f) (fslot ,g)))

; tell call about compose so that we can use ssyntax with macros
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
