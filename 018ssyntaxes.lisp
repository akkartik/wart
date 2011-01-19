;; Standard ssyntax
(def-ssyntax #\^ 'compose)
(def-ssyntax #\. 'call)
(def-ssyntax #\! 'call-quoted)
(def-ssyntax #\~ 'complement* -1) ; precedence like arc, but perhaps not best

(defmacro fslot(f)
  (if (and (atom f) (fboundp f))
    `(function ,f)
    f))

(defmacro complement*(f)
  `(complement (fslot ,f)))

(defmacro call-quoted(a b)
  `(call ,a ',b))
