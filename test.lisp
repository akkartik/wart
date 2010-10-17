(load "bdd.lisp")

(defun foo() 34)
(test "simple defun"
  :valueof (foo)
  :should be 34)

(progn
  (defun foo0() 3)
  (defun foo1() (+ 1 (foo0))))
(test "nested defuns"
  :valueof (foo1)
  :should be 4)

(progn
  (defmacro foo2(n) `(+ 1 ,n))
  (defmacro foo3(n) `(+ 1 (foo2 ,n))))
(test "nested macros"
  :valueof (foo3 2)
  :should be 4)

(eval (wc '(def foo4() 34)))
(test "simple def"
  :valueof (foo4)
  :should be 34)

(eval (wc '(mac foo5(n) `(+ ,n 1))))
(test "simple mac"
  :valueof (foo5 32)
  :should be 33)

(eval (wc '(def foo6() (cons 3 4))))
(test "def 2"
  :valueof (foo6)
  :should be (cons 3 4))

(eval (wc '(def foo7(a . b) b)))
(test "dotted rest"
  :valueof (foo7 3 4)
  :should be '(4))

(test "wc-destructuring-bind handles plain vars"
  :valueof (wc-destructuring-bind a 1 a)
  :should be 1)

(test "wc-destructuring-bind handles nested lists"
  :valueof (wc-destructuring-bind (a) '(1) a)
  :should be 1)

(test "wc-destructuring-bind handles empty lists"
  :valueof (wc-destructuring-bind (a) '() a)
  :should be nil)

(test "wc-destructuring-bind handles lists"
  :valueof (wc-destructuring-bind ((a b) (c d)) '((1 2) (3 4)) c)
  :should be 3)

(test "wc-destructuring-bind handles &rest"
  :valueof (wc-destructuring-bind (&rest b) '(1) b)
  :should be '(1))

(test "wc-destructuring-bind handles destructured args + rest"
  :valueof (wc-destructuring-bind ((a b) (c d) &rest e) '((1 2) (3 4) 5 6 7) (cons c e))
  :should be '(3 5 6 7))

(eval (wc '(def foo8((a b)) b)))
(test "destructured args"
  :valueof (foo8 '(3 4))
  :should be 4)

(eval (wc '(def foo9((a b) . c) b)))
(test "destructured args + dotted rest"
  :valueof (foo9 '(3 4) 5)
  :should be 4)

(test "destructured args + dotted rest for fn"
  :valueof ((fn ((a b) . c) b) '(3 4) 5)
  :should be 4)

(test "alternative syntax for fn"
  :valueof ([+ _ 1] 3)
  :should be 4)

(setq foo10 (lambda() 3))
(test "no need for funcall on function atoms"
  :valueof (foo10)
  :should be 3)

(test "no need for funcall on function forms"
  :valueof ((lambda() 3))
  :should be 3)

(test "no need for funcall on function forms with args"
  :valueof ((lambda(a) (+ a 3)) 2)
  :should be 5)

(test "non-top-level calls require funcall"
  :valueof (let ((a 1)) (funcall (fn() a)))
  :should be 1)

(pending-test "no need for funcall with non-top-level function forms"
  :valueof (let ((a 1)) ((fn() a)))
  :should be 1)

(test "remove-if-cons works"
  :valueof (remove-if-cons #'oddp '(1 2 3 4))
  :should be '(2 4))

(test "remove-if-cons works on dotted lists"
  :valueof (remove-if-cons #'oddp '(1 2 3 . 4))
  :should be '(2 . 4))

(test "remove-if-cons works on dotted lists with passing rest"
  :valueof (remove-if-cons #'evenp '(1 2 3 . 4))
  :should be '(1 3))

(test "remove-if-cons works on dotted lists with nil rest"
  :valueof (remove-if-cons #'evenp '(1 2 3 . nil))
  :should be '(1 3))

(test "wc-complex-bind handles simple args"
  :valueof (wc-complex-bind (a b) '(1 2) b)
  :should be 2)

(test "wc-complex-bind handles dotted rest args"
  :valueof (wc-complex-bind (a . b) '(1 2 3) b)
  :should be '(2 3))

(test "wc-complex-bind handles destructured args"
  :valueof (wc-complex-bind ((a b)) '((1 2)) b)
  :should be 2)

(test "wc-complex-bind handles destructured args + dotted rest"
  :valueof (wc-complex-bind ((a b) . c) '((1 2) 3) (cons b c))
  :should be '(2 3))

(test "strip-lambda-keywords works"
  :valueof (strip-lambda-keywords '(a &rest b))
  :should be '(a b))

(test "partition-keywords siphons keyword-arg val pairs into a hash table"
  :valueof (partition-keywords '(1 2 :c 3))
  :should be (list '(1 2) '((c . 3))))

(test "merge-keyword-vars takes args first from list giving priority to hash"
  :valueof (merge-keyword-vars '(1 3) '((b . 2)) '(a b c))
  :should be '(1 2 3))

(test "merge-keyword-vars is idempotent with non-keyword args"
  :valueof (merge-keyword-vars '(1 2 3) () '(a b c))
  :should be '(1 2 3))

(test "merge-keyword-vars is idempotent with non-keyword dotted args"
  :valueof (merge-keyword-vars '(1 2) () '(a . b))
  :should be '(1 2))

(test "merge-keyword-vars is idempotent with non-keyword rest args"
  :valueof (merge-keyword-vars '(1 2) () '(a &rest b))
  :should be '(1 2))

(test "wc-complex-bind handles an optional keyword param"
  :valueof (wc-complex-bind (a) '(:a 1) a)
  :should be 1)

(eval (wc '(def foo11(a b) (- a b))))
(test "allow param names"
  :valueof (foo11 :a 3 :b 4)
  :should be -1)

(test "allow just some param names"
  :valueof (foo11 :a 3 4)
  :should be -1)

(test "allow args in any order when giving param names"
  :valueof (foo11 :b 3 :a 4)
  :should be 1)

(test "take positional args in order after keyword args have been matched"
  :valueof (foo11 3 :a 4)
  :should be 1)

(test "strip-default-values works"
  :valueof (strip-default-values '(a (b 2)))
  :should be '(a b))

(test "strip-default-values works when the default val is nil"
  :valueof (strip-default-values '(a (b nil)))
  :should be '(a b))

(test "strip-default-values passes through expressions needing destructuring"
  :valueof (strip-default-values '(a (b c)))
  :should be '(a (b c)))

(test "simplify-arg-list passes lists through by default"
  :valueof (simplify-arg-list '(a b))
  :should be '(a b))

(test "simplify-arg-list strips default values"
  :valueof (simplify-arg-list '(a (b 2)))
  :should be '(a b))

(test "simplify-arg-list works on dotted lists"
  :valueof (simplify-arg-list '(a . b))
  :should be '(a &rest b))

(test "add-optional-vars works"
  :valueof (add-optional-vars '(a (b 2)) ())
  :should be '((b . 2)))

(eval (wc '(def foo12(a (b 2)) (cons a b))))
(test "optional param"
  :valueof (foo12 3)
  :should be '(3 . 2))

(test "allow optional params to refer to variables"
  :valueof (let ((a 2)) (funcall (fn((x 3)) x)))
  :should be 3)

(test "args should override optional params"
  :valueof (foo12 3 4)
  :should be '(3 . 4))

(test "distinguish destructured from optional params"
  :valueof ((fn((a b)) (list a b)) '(1 (2)))
  :should be '(1 (2)))

(test "distinguish destructured from optional params - 2"
  :valueof ((fn((a (b))) (list a b)) '(1 (2)))
  :should be '(1 2))

(test "optional param must come at the end"
  :given (def foo(a :o b c) (cons a b))
  :valueof (foo 3)
  :should die)

(test "optional param with a default"
  :given (def foo(a (o b 4)) (cons a b))
  :valueof (foo 3)
  :should be '(3 . 4))

(test "optional arg without naming"
  :given (def foo(a (o b 4)) (cons a b))
  :valueof (foo 3 nil)
  :should be '(3))

(test "optional named arg"
  :given (def foo(a (o b 4)) (cons a b))
  :valueof (foo :a 3)
  :should be '(3 . 4))

(test "require non-optional param"
  :given (def foo(a (o b 4)) (cons a b))
  :valueof (foo :b 3)
  :should die)

(test "multiple optional args"
  :given (def foo(a (o b 4) (o c)) (cons b c))
  :valueof (foo 3)
  :should be '(4))

(test "allow optional named args out of order"
  :given (def foo(a (o b 4) (o c)) (cons b c))
  :valueof (foo 3 :c 2 :b nil)
  :should be '(nil . 2))

(test "allow optional args in order without naming"
  :given (def foo(a (o b 4) (o c)) (cons b c))
  :valueof (foo 3 nil 2)
  :should be '(nil . 2))

(test "rest/body args"
  :given (def foo(a . b) b)
  :valueof (foo 3 4)
  :should be 4)

(test "rest/body args are optional"
  :given (def foo(a . b) b)
  :valueof (foo 3)
  :should be nil)

(test "rest/body args can be named"
  :given (def foo(a . b) b)
  :valueof (foo 3 :b 4 5)
  :should be '(4 5))

(test "body args can come after :do"
  :given (def foo(a . b) b)
  :valueof (foo 3 :do 4 5)
  :should be '(4 5))

(test "rest args can come after ::"
  :given (def foo(a . b) b)
  :valueof (foo 3 :: 4 5)
  :should be '(4 5))

(test "optional and rest params together"
  :given (def foo(a (o b) . body) (cons b body))
  :valueof (foo 3 :do 4 5)
  :should be '(nil 4 5))

(test "call with both optional and rest args without naming"
  :given (def foo(a (o b) . body) (cons b body))
  :valueof (foo 3 4 :do 4 5)
  :should be '(4 4 5))

(test "call with some optional and rest args without naming"
  :given (def foo(a (o b) (o c) . body) (cons b body))
  :valueof (foo 3 4 :do 4 5)
  :should be '(4 4 5))

(test "call with some named optional and rest args"
  :given (def foo(a (o b) (o c) . body) (cons c body))
  :valueof (foo 3 :c 4 :do 4 5)
  :should be '(4 4 5))

; destructured keyword args?

; Plan: with, let, if, map
