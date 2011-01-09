;; Standard ssyntax
(def-ssyntax #\^ 'compose*)
(def-ssyntax #\~ 'complement*)
(def-ssyntax #\. 'call*)
(def-ssyntax #\! 'call*-quoted)

(defmacro fslot(f)
  (if (and (atom f) (fboundp f))
    `(function ,f)
    f))

(defmacro compose*(f g)
  `(compose (fslot ,f) (fslot ,g)))

(defmacro complement*(f)
  `(complement (fslot ,f)))

(defun macro-or-compose(f)
  (or (macp f)
      (and (consp f)
           (is 'compose* (car f)))))

(defmacro macro-composition-p(expr)
  `(and (consp ,expr)
        (is 'compose* (car ,expr))))

; flatten -> process from right to left
; g is never nested compose
(defun expand-composition(f g args)
;?   (if (and (not (macp g))
;?            (not (macro-composition-p g)))
;?     (if (macp f)
;?       `(,f (call (fslot ,g) ,@args))
  (if (macp f)
    (if (macp g)
      `(,f (,g ,@args))
      `(,f (call (fslot ,g) ,@args)))
    (if (macp g)
      `(call (fslot ,f) (,g ,@args))
      `(call (compose* ,f ,g) ,@args))))

; needs to know about compose*
;   1+^h.1 => (call (compose* '1+ 'h) 1)
; but
;   ++^h.1 => (incf (call h 1))
; ssyntax doesn't seem a good fit for a lisp-2.
(defmacro call*(f &rest args)
  (cond
    ((macp f)  `(,f ,@args))
    ((and (atom f) (fboundp f))  `(call (function ,f) ,@args))
    ((macro-composition-p f)   (expand-composition (cadr f) (caddr f) args))
    (t  `(call ,f ,@args))))

(defmacro call*-quoted(a b)
  `(call* ,a ',b))

(defun compose(f g)
  (lambda(&rest args)
    (call f (wart-apply g args))))
