;; Standard ssyntax
(def-ssyntax #\^ 'compose*)
(def-ssyntax #\~ 'complement*)
(def-ssyntax #\. 'call*)
(def-ssyntax #\! 'call*-quoted)

(defmacro compose*(f g)
  `(compose (fslot ,f) (fslot ,g)))

(defmacro complement*(f)
  `(complement (fslot ,f)))

(defmacro call*-quoted(a b)
  `(call* ,a ',b))

(defun compose(f g)
  (lambda(&rest args)
    (call f (wart-apply g args))))
