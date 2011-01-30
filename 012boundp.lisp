; boundp now recognizes local vars
; assumes wiped vars are unbound
; macros aren't bindings
(defmacro wart-boundp(x)
  (if (symbolp x)
    `(errsafe ,x)
    ; ensure (boundp 'x) returns the same as (boundp x)
    (if (and (match x '(quote _))
             (symbolp (cadr x)))
      `(errsafe ,(cadr x)))))
(defover boundp wart-boundp)
