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

(eval (wc '(def foo7 args args)))
(test-wc "just a rest arg without parens"
    :valueof (foo7 3 4 5)
    :should be '(3 4 5))

(eval (wc '(def foo8(a . b) b)))
(test-wc "dotted rest"
  :valueof (foo8 3 4)
  :should be '(4))

(test-wc "rest args are optional"
  :valueof (foo8 3)
  :should be nil)

(test-wc "wc-destructuring-bind handles plain vars"
  :valueof (wc-destructuring-bind a 1 a)
  :should be 1)

(test-wc "wc-destructuring-bind handles nested lists"
  :valueof (wc-destructuring-bind (a) '(1) a)
  :should be 1)

(test-wc "wc-destructuring-bind handles empty lists"
  :valueof (wc-destructuring-bind (a) '() a)
  :should be nil)

(test-wc "wc-destructuring-bind handles lists"
  :valueof (wc-destructuring-bind ((a b) (c d)) '((1 2) (3 4)) c)
  :should be 3)

(test-wc "wc-destructuring-bind handles &rest"
  :valueof (wc-destructuring-bind (&rest b) '(1) b)
  :should be '(1))

(test-wc "wc-destructuring-bind handles destructured args + rest"
  :valueof (wc-destructuring-bind ((a b) (c d) &rest e) '((1 2) (3 4) 5 6 7) (cons c e))
  :should be '(3 5 6 7))

(eval (wc '(def foo9((a b)) b)))
(test-wc "destructured args"
  :valueof (foo9 '(3 4))
  :should be 4)

(eval (wc '(def foo10((a b) . c) b)))
(test-wc "destructured args + dotted rest"
  :valueof (foo10 '(3 4) 5)
  :should be 4)

(test-wc "destructured args + dotted rest for fn"
  :valueof ((fn ((a b) . c) b) '(3 4) 5)
  :should be 4)

(test-wc "alternative syntax for fn"
  :valueof ([+ _ 1] 3)
  :should be 4)

(setq foo11 (lambda() 3))
(test-wc "no need for funcall on function atoms"
  :valueof (foo11)
  :should be 3)

(test-wc "no need for funcall on function forms"
  :valueof ((lambda() 3))
  :should be 3)

(test-wc "no need for funcall on function forms with args"
  :valueof ((lambda(a) (+ a 3)) 2)
  :should be 5)

(test-wc "non-top-level calls require funcall"
  :valueof (wc-let a 1 (funcall (fn() a)))
  :should be 1)

(test-wc "no need for funcall with non-top-level function forms"
  :valueof (wc-let a 1 ((fn() a)))
  :should be 1)

(test-wc "wc-complex-bind handles simple args"
  :valueof (wc-complex-bind (a b) '(1 2) b)
  :should be 2)

(test-wc "wc-complex-bind handles dotted rest args"
  :valueof (wc-complex-bind (a . b) '(1 2 3) b)
  :should be '(2 3))

(test-wc "wc-complex-bind handles destructured args"
  :valueof (wc-complex-bind ((a b)) '((1 2)) b)
  :should be 2)

(test-wc "wc-complex-bind handles destructured args + dotted rest"
  :valueof (wc-complex-bind ((a b) . c) '((1 2) 3) (cons b c))
  :should be '(2 3))

(test-wc "partition-keywords siphons keyword-arg val pairs into a hash table"
  :valueof (partition-keywords '(1 2 :c 3))
  :should be (list '(1 2) '((c . 3))))

(test-wc "merge-keyword-vars takes args from list giving priority to keyword hash"
  :valueof (merge-keyword-vars '(1 3) '((b . 2)) () '(a b c))
  :should be '(1 2 3))

(test-wc "merge-keyword-vars takes args from optional hash"
  :valueof (merge-keyword-vars '(1 3) '((b . 2)) '((d . 1)) '(a b c d))
  :should be '(1 2 3 1))

(test-wc "merge-keyword-vars lets args from keyword hash override optional args"
  :valueof (merge-keyword-vars '(1 3) '((b . 2) (d . 3)) '((d . 1)) '(a b c d))
  :should be '(1 2 3 3))

(test-wc "merge-keyword-vars lets args from list override optional args"
  :valueof (merge-keyword-vars '(1 3 4) '((b . 2)) '((d . 1)) '(a b c d))
  :should be '(1 2 3 4))

(test-wc "merge-keyword-vars works in the presence of optional and rest args"
  :valueof (merge-keyword-vars '(2) '((c 4)) '((b . 3)) '(a b &rest c))
  :should be '(2 3 4))

(test-wc "merge-keyword-vars is idempotent with non-keyword args"
  :valueof (merge-keyword-vars '(1 2 3) () () '(a b c))
  :should be '(1 2 3))

(test-wc "merge-keyword-vars is idempotent with non-keyword dotted args"
  :valueof (merge-keyword-vars '(1 2) () () '(a . b))
  :should be '(1 2))

(test-wc "merge-keyword-vars is idempotent with non-keyword rest args"
  :valueof (merge-keyword-vars '(1 2) () () '(a &rest b))
  :should be '(1 2))

(test-wc "wc-complex-bind handles an optional keyword param"
  :valueof (wc-complex-bind (a) '(:a 1) a)
  :should be 1)

(eval (wc '(def foo12(a b) (- a b))))
(test-wc "allow param names"
  :valueof (foo12 :a 3 :b 4)
  :should be -1)

(test-wc "allow just some param names"
  :valueof (foo12 :a 3 4)
  :should be -1)

(test-wc "allow args in any order when giving param names"
  :valueof (foo12 :b 3 :a 4)
  :should be 1)

(test-wc "take positional args in order after keyword args have been matched"
  :valueof (foo12 3 :a 4)
  :should be 1)

(test-wc "strip-default-values works"
  :valueof (strip-default-values '(a (b 2)))
  :should be '(a b))

(test-wc "strip-default-values works when the default val is nil"
  :valueof (strip-default-values '(a (b nil)))
  :should be '(a b))

(test-wc "strip-default-values passes through expressions needing destructuring"
  :valueof (strip-default-values '(a (b c)))
  :should be '(a (b c)))

(test-wc "simplify-arg-list passes lists through by default"
  :valueof (simplify-arg-list '(a b))
  :should be '(a b))

(test-wc "simplify-arg-list strips default values"
  :valueof (simplify-arg-list '(a (b 2)))
  :should be '(a b))

(test-wc "simplify-arg-list works on dotted lists"
  :valueof (simplify-arg-list '(a . b))
  :should be '(a &rest b))

(eval (wc '(def foo13(a (b nil)) (cons a b))))
(test-wc "optional param"
  :valueof (foo13 3)
  :should be '(3))

(eval (wc '(def foo14(a (b 4)) (cons a b))))
(test-wc "optional param with a default"
  :valueof (foo14 3)
  :should be '(3 . 4))

(test-wc "optional named arg"
  :valueof (foo14 :a 3)
  :should be '(3 . 4))

(test-wc "distinguish destructured from optional params"
  :valueof ((fn((a b)) (list a b)) '(1 (2)))
  :should be '(1 (2)))

(test-wc "distinguish destructured from optional params - 2"
  :valueof ((fn((a (b))) (list a b)) '(1 (2)))
  :should be '(1 2))

(test-wc "args should override optional params"
  :valueof (foo14 3 :b 2)
  :should be '(3 . 2))

(test-wc "optional arg without naming"
  :valueof (foo14 3 2)
  :should be '(3 . 2))

(test-wc "nil overrides default for optional arg"
  :valueof (foo14 3 :b nil)
  :should be '(3))

(test-wc "nil overrides default for optional arg without naming"
  :valueof (foo14 3 nil)
  :should be '(3))

(pending-test-wc "allow optional params to refer to variables"
  :valueof (wc-let a 2 (funcall (fn((x a)) x)))
  :should be 3)

(eval (wc '(def foo15(a (b 4) (c nil)) (cons b c))))
(test-wc "multiple optional args"
  :valueof (foo15 3)
  :should be '(4))

(test-wc "allow optional named args out of order"
  :valueof (foo15 3 :c 2 :b nil)
  :should be '(nil . 2))

(test-wc "allow optional args in order without naming"
  :valueof (foo15 3 nil 2)
  :should be '(nil . 2))

(eval (wc '(def foo16(a . b) b)))
(test-wc "rest args can be named"
  :valueof (foo16 3 :b 4 5)
  :should be '(4 5))

(eval (wc '(def foo17(a (b 3) . c) (cons b c))))
(test-wc "optional + named rest args"
  :valueof (foo17 2 :c 3)
  :should be '(3 3))

(test-wc "optional + named rest args - 2"
  :valueof (foo17 2 4 :c 3)
  :should be '(4 3))

(test-wc "optional + named rest args - 3"
  :valueof (foo17 2 4 3 1)
  :should be '(4 3 1))

(test-wc "optional + named rest args - 3"
  :valueof (foo17 2 :b 4 3)
  :should be '(4 3))

(eval (wc '(def foo18(a (b nil) (c 3) . body) (list b c body))))
(test-wc "call with some optional and rest args without naming"
  :valueof (foo18 3 4 :body 4 5)
  :should be '(4 3 (4 5)))

(test-wc "call with some named optional and rest args"
  :valueof (foo18 3 :c 4 :body 4 5)
  :should be '(nil 4 (4 5)))

(format t "~%")
(test-wc "new redefined let"
  :valueof (let a 3 a)
  :should be 3)

(test-wc "tuples works"
  :valueof (tuples '(1 2 3 4 5 6) 3)
  :should be '((1 2 3) (4 5 6)))

(test-wc "tuples works with unbalanced lists"
  :valueof (tuples '(1 2 3 4 5) 3)
  :should be '((1 2 3) (4 5)))

(test-wc "tuples works with n 1"
  :valueof (tuples '(1 2 3 4 5) 1)
  :should be (map 'list #'list '(1 2 3 4 5)))

(test-wc "new redefined if with one branch"
  :valueof (if nil 3)
  :should be nil)

(test-wc "new redefined if with one branch - 2"
  :valueof (if t 3)
  :should be 3)

(test-wc "new redefined if with two branches"
  :valueof (if t 3 4)
  :should be 3)

(test-wc "new redefined if with two branches - 2"
  :valueof (if nil 3 4)
  :should be 4)

(test-wc "new redefined if with three branches"
  :valueof (if (> 3 2) 3 (> 3 4) 5 6)
  :should be 3)

(test-wc "new redefined if with three branches - 2"
  :valueof (if (< 3 2) 3 (< 3 4) 5 6)
  :should be 5)

(test-wc "new redefined if with three branches - 3"
  :valueof (if (< 3 2) 3 (< 3 3) 5 6)
  :should be 6)

(test-wc "combine all these keywords"
  :valueof (let a 3 (if (> a 5) 5
                        (> a 4) 4
                        (> a 3) 3
                        (> a 2) "correct"
                                "Default"))
  :should be "correct")

(test-wc "new keyword - with"
  :valueof (with (a 3) a)
  :should be 3)

(format t "~%")
(test-wc "wc works through defuns"
  :valueof (wc '(defun foo(n) (let it n (+ it 1))))
  :should be '(defun foo(n) (wc-let it n (+ it 1))))

(test-wc "lookup-quoted-handler handles function-names"
  :valueof (lookup-quoted-handler 'let)
  :should be 'wc-let)

(test "wc-1 handles quoted special names"
  :valueof (wc-1 '(quote let))
  :should be '(quote wc-let))

(test "wc handles quoted special names"
  :valueof (wc '(backq-list* 'let))
  :should be '(backq-list* 'wc-let))

(test-wc "wc works through backquote"
  :valueof (wc-let n 3 `(let it ,n (+ it 1)))
  :should be '(wc-let it 3 (+ it 1)))

(test "wc works through defmacro"
  :valueof (wc '(defmacro foo(n) `(let it ,n (+ it 1))))
  :should be '(defmacro foo(n) `(wc-let it ,n (+ it 1))))

(eval (wc '(mac aand(a b) `(let it ,a (and it ,b)))))
(test-wc "macro bodies should pass through the compiler"
  :valueof (let x 3 (aand (- x 1) (> it 1)))
  :should be t)
