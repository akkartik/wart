;; Standard ssyntax
(def-ssyntax #\^ 'compose*)
(def-ssyntax #\~ 'complement*)
(def-ssyntax #\. 'call*)
(def-ssyntax #\! 'call*-quoted)

(defmacro compose*(f g)
  `(compose (function ,f) (function ,g)))

(defmacro complement*(f)
  `(complement (function ,f)))

(defmacro call*(a b)
  (cond
    ((macp a)   `(,a ,b))
    ((function-name-p a)  `(call (function ,a) ,b))
    (t  `(call ,a ,b))))

(defmacro call*-quoted(a b)
  `(call* ,a ',b))



;; Internals

(defun compose(f g)
  (lambda(&rest args)
    (call f (apply g args))))

(defun function-name-p(f)
  (and (atom f)
       (eval `(fboundp ',f))))
