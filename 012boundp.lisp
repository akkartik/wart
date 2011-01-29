; boundp now recognizes local vars
; assumes wiped vars are unbound
(defmacro wart-boundp(x)
  `(and (symbolp ',x) (errsafe ,x)))
(defover boundp wart-boundp)
