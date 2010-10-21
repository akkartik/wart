(load "bdd.lisp")

(defun foo() 34)
(test-wc "simple defun"
  :valueof (foo)
  :should be 34)

(progn
  (defun foo0() 3)
  (defun foo1() (+ 1 (foo0))))
(test-wc "nested defuns"
  :valueof (foo1)
  :should be 4)

(progn
  (defmacro foo2(n) `(+ 1 ,n))
  (defmacro foo3(n) `(+ 1 (foo2 ,n))))
(test-wc "nested macros"
  :valueof (foo3 2)
  :should be 4)

(eval (wc '(def foo4() 34)))
(test-wc "simple def"
  :valueof (foo4)
  :should be 34)

(eval (wc '(mac foo5(n) `(+ ,n 1))))
(test-wc "simple mac"
  :valueof (foo5 32)
  :should be 33)

(eval (wc '(def foo6() (cons 3 4))))
(test-wc "def 2"
  :valueof (foo6)
  :should be (cons 3 4))
