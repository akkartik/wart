;; Standard ssyntax
(def-ssyntax #\^ 'compose*)
(def-ssyntax #\. 'call)
(def-ssyntax #\! 'call-quoted)
(def-ssyntax #\~ 'complement* -1) ; precedence like arc, but perhaps not best

(defmacro fslot(f)
  (if (and (atom f) (fboundp f))
    `(function ,f)
    f))

(defmacro compose*(f g)
  `(compose (fslot ,f) (fslot ,g)))

(defun compose(f g)
  (lambda(&rest args)
    (call-fn f (wart-apply g args))))

(defmacro complement*(f)
  `(complement (fslot ,f)))

; knows about compose* so that we can use ssyntax with macros
;   ++^h.1 => (call incf (call h 1))
; ssyntax doesn't seem a good fit for a lisp-2.
(defmacro call(f &rest args)
  (cond
    ((macp f)  `(,f ,@args))
    ((and (atom f) (fboundp f))  `(call-fn (function ,f) ,@args))
    ((macro-composition-p f)   (expand-composition f args))
    (t  `(call-fn ,f ,@args))))

(defmacro call-quoted(a b)
  `(call ,a ',b))



;; Internals

(defun macro-composition-p(expr)
  (and (consp expr)
       (is 'compose* (car expr))))

(defun expand-composition(f args)
  (apply-nested-calls (compositions f) args))

(defun compositions(f)
  (if (macro-composition-p f)
    (append (compositions (cadr f))
            (compositions (caddr f)))
    (list f))) ; Assumption: compose* operates only on syms

(defun apply-nested-calls(fs args)
  (if (singlep fs)
    `(call ,(car fs) ,@args)
    `(call ,(car fs) ,(apply-nested-calls (cdr fs) args))))
