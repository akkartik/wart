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

(defmacro macro-composition-p(expr)
  `(and (consp ,expr)
        (is 'compose* (car ,expr))
        (or (macp (cadr ,expr))
            (macp (caddr ,expr)))))

(defun expand-composition(f g args)
  (if (macp f)
    (if (macp g)
      `(,f (,g ,@args))
      `(,f (call (fslot ,g) ,@args)))
    (if (macp g)
      `(call (fslot ,f) (,g ,@args))
      '*error*)))

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
