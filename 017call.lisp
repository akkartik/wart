; like funcall, but can handle macros
; knows about compose so that we can use ssyntax with macros
;   ++^h.1 => (call incf (call h 1))
; ssyntax doesn't seem a good fit for a lisp-2.

(defmacro macp*(x)
  `(and (not (wart-boundp ,`(eval ,x)))
        (macp ,x)))
(defmacro call(f &rest args)
  (prn f)
  (prn "a")
  (prn (symbolp 'macfoo))
  (prn "b")
  (prn (errsafe macfoo))
  (prn "c")
  (prn (wart-boundp macfoo))
  (prn "m")
  (prn (macp* f))
  (prn (wart-boundp f))
  (prn "zz")
  (cond
    ((macp* f)   `(,f ,@args))
;?     ((and (not (boundp f)) (macp f))  `(,f ,@args))
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
