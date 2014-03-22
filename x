- 0 test_eval_evals_arg
bind/1/1: (a) <-> (x)
bind/1/2: (a)(a) <-> (x)(x)
bind/1/2: skip keyword? (x)
bind/1/2: skipping ahead to (x)
bind/1/2: regular a
bind/1/2: after eval 3
bind/1/2: a: 3
bind/1/3: (a)nil <-> (x)nil
bind/1/1: => {a:3, }
- 1 test_eval_evals_arg2
bind/1/1: (f) <-> ((fn nil 34))
bind/1/2: (f)(f) <-> ((fn nil 34))((fn nil 34))
bind/1/2: skip keyword? ((fn nil 34))
bind/1/2: skipping ahead to ((fn nil 34))
bind/1/2: regular f
bind/1/2: after eval (object function {body:(34), })
bind/1/2: f: (object function {body:(34), })
bind/1/3: (f)nil <-> ((fn nil 34))nil
bind/1/1: => {f:(object function {body:(34), }), }
- 2 test_eval_handles_multiple_args
bind/1/1: (a b) <-> (1 2)
bind/1/2: (a b)(a b) <-> (1 2)(1 2)
bind/1/2: skip keyword? (1 2)
bind/1/2: skipping ahead to (1 2)
bind/1/2: regular a
bind/1/2: after eval 1
bind/1/2: a: 1
bind/1/3: (a b)(b) <-> (1 2)(2)
bind/1/3: skip keyword? (2)
bind/1/3: skipping ahead to (2)
bind/1/3: regular b
bind/1/3: after eval 2
bind/1/3: b: 2
bind/1/4: (a b)nil <-> (1 2)nil
bind/1/1: => {b:2, a:1, }
- 3 test_eval_binds_missing_params
bind/1/1: (a b) <-> (x)
bind/1/2: (a b)(a b) <-> (x)(x)
bind/1/2: skip keyword? (x)
bind/1/2: skipping ahead to (x)
bind/1/2: regular a
bind/1/2: after eval 3
bind/1/2: a: 3
bind/1/3: (a b)(b) <-> (x)nil
bind/1/3: skip keyword? nil
bind/1/3: skipping ahead to nil
bind/1/3: regular b
bind/1/3: after eval nil
bind/1/3: b: nil
bind/1/4: (a b)nil <-> (x)nil
bind/1/1: => {b:nil, a:3, }
- 4 test_eval_binds_quoted_param
bind/1/1: ('a) <-> (x)
bind/1/2: ('a)('a) <-> (x)(x)
bind/1/2: skip keyword? (x)
bind/1/2: skipping ahead to (x)
bind/1/2: quoted a
bind/1/2: positional arg
bind/1/2: a: x
bind/1/2: regular arg
bind/1/3: ('a)nil <-> (x)nil
bind/1/1: => {a:x, }
- 5 test_eval_handles_quoted_param_list
bind/1/1: (arg1) <-> (a)
bind/1/2: (arg1)(arg1) <-> (a)(a)
bind/1/2: skip keyword? (a)
bind/1/2: skipping ahead to (a)
bind/1/2: quoted arg1
bind/1/2: positional arg
bind/1/2: arg1: a
bind/1/2: regular arg
bind/1/3: (arg1)nil <-> (a)nil
bind/1/1: => {arg1:a, }
- 6 test_eval_handles_vararg_param
bind/1/1: args <-> (1)
bind/1/2: argsargs <-> (1)(1)
bind/1/2: skip keyword? (1)
bind/1/2: skipping ahead to (1)
bind/1/2: rest args
bind/1/2: args: (1)
bind/1/1: => {args:(1), }
- 7 test_eval_evals_vararg_args
bind/1/1: args <-> (x y)
bind/1/2: argsargs <-> (x y)(x y)
bind/1/2: skip keyword? (x y)
bind/1/2: skipping ahead to (x y)
bind/1/2: rest args
bind/1/2: args: (3 4)
bind/1/1: => {args:(3 4), }
- 8 test_eval_binds_quoted_varargs_param
bind/1/1: args <-> (x y)
bind/1/2: argsargs <-> (x y)(x y)
bind/1/2: skip keyword? (x y)
bind/1/2: skipping ahead to (x y)
bind/1/2: quoted rest args
bind/1/2: args: (x y)
bind/1/1: => {args:(x y), }
- 9 test_eval_handles_rest_params
bind/1/1: (a b ... c) <-> (1 2 3 4 5)
bind/1/2: (a b ... c)(a b ... c) <-> (1 2 3 4 5)(1 2 3 4 5)
bind/1/2: skip keyword? (1 2 3 4 5)
bind/1/2: skipping ahead to (1 2 3 4 5)
bind/1/2: regular a
bind/1/2: after eval 1
bind/1/2: a: 1
bind/1/3: (a b ... c)(b ... c) <-> (1 2 3 4 5)(2 3 4 5)
bind/1/3: skip keyword? (2 3 4 5)
bind/1/3: skipping ahead to (2 3 4 5)
bind/1/3: regular b
bind/1/3: after eval 2
bind/1/3: b: 2
bind/1/4: (a b ... c)c <-> (1 2 3 4 5)(3 4 5)
bind/1/4: skip keyword? (3 4 5)
bind/1/4: skipping ahead to (3 4 5)
bind/1/4: rest c
bind/1/4: c: (3 4 5)
bind/1/1: => {b:2, c:(3 4 5), a:1, }
- 10 test_eval_evals_rest_args
bind/1/1: (a ... b) <-> (x y)
bind/1/2: (a ... b)(a ... b) <-> (x y)(x y)
bind/1/2: skip keyword? (x y)
bind/1/2: skipping ahead to (x y)
bind/1/2: regular a
bind/1/2: after eval 3
bind/1/2: a: 3
bind/1/3: (a ... b)b <-> (x y)(y)
bind/1/3: skip keyword? (y)
bind/1/3: skipping ahead to (y)
bind/1/3: rest b
bind/1/3: b: (4)
bind/1/1: => {b:(4), a:3, }
- 11 test_eval_binds_quoted_rest_param
bind/1/1: (a ' ... b) <-> (x y)
bind/1/2: (a ' ... b)(a ' ... b) <-> (x y)(x y)
bind/1/2: skip keyword? (x y)
bind/1/2: skipping ahead to (x y)
bind/1/2: regular a
bind/1/2: after eval 3
bind/1/2: a: 3
bind/1/3: (a ' ... b)'b <-> (x y)(y)
bind/1/3: skip keyword? (y)
bind/1/3: skipping ahead to (y)
bind/1/3: quoted rest b
bind/1/3: b: (y)
bind/1/1: => {a:3, b:(y), }
- 12 test_eval_handles_destructured_params
bind/1/1: ((a b)) <-> ('(1 2))
bind/1/2: ((a b))((a b)) <-> ('(1 2))('(1 2))
bind/1/2: skip keyword? ('(1 2))
bind/1/2: skipping ahead to ('(1 2))
bind/1/2: destructured (a b)
bind/1/2: regular arg
bind/1/3: (a b) <-> (1 2)
bind/1/4: (a b)(a b) <-> (1 2)(1 2)
bind/1/4: skip keyword? (1 2)
bind/1/4: skipping ahead to (1 2)
bind/1/4: quoted a
bind/1/4: positional arg
bind/1/4: a: 1
bind/1/4: regular arg
bind/1/5: (a b)(b) <-> (1 2)(2)
bind/1/5: skip keyword? (2)
bind/1/5: skipping ahead to (2)
bind/1/5: quoted b
bind/1/5: positional arg
bind/1/5: b: 2
bind/1/5: regular arg
bind/1/6: (a b)nil <-> (1 2)nil
bind/1/3: => {b:2, a:1, }
bind/1/3: ((a b))nil <-> ('(1 2))nil
bind/1/1: => {b:2, a:1, }
- 13 test_eval_evals_destructured_args
bind/1/1: ((a b)) <-> (`(,x ,y))
bind/1/2: ((a b))((a b)) <-> (`(,x ,y))(`(,x ,y))
bind/1/2: skip keyword? (`(,x ,y))
bind/1/2: skipping ahead to (`(,x ,y))
bind/1/2: destructured (a b)
bind/1/2: regular arg
bind/1/3: (a b) <-> (3 4)
bind/1/4: (a b)(a b) <-> (3 4)(3 4)
bind/1/4: skip keyword? (3 4)
bind/1/4: skipping ahead to (3 4)
bind/1/4: quoted a
bind/1/4: positional arg
bind/1/4: a: 3
bind/1/4: regular arg
bind/1/5: (a b)(b) <-> (3 4)(4)
bind/1/5: skip keyword? (4)
bind/1/5: skipping ahead to (4)
bind/1/5: quoted b
bind/1/5: positional arg
bind/1/5: b: 4
bind/1/5: regular arg
bind/1/6: (a b)nil <-> (3 4)nil
bind/1/3: => {b:4, a:3, }
bind/1/3: ((a b))nil <-> (`(,x ,y))nil
bind/1/1: => {b:4, a:3, }
- 14 test_eval_handles_quoted_destructured_params
bind/1/1: ('(a b)) <-> ((1 2))
bind/1/2: ('(a b))('(a b)) <-> ((1 2))((1 2))
bind/1/2: skip keyword? ((1 2))
bind/1/2: skipping ahead to ((1 2))
bind/1/2: quoted destructured (a b)
bind/1/3: (a b) <-> (1 2)
bind/1/4: (a b)(a b) <-> (1 2)(1 2)
bind/1/4: skip keyword? (1 2)
bind/1/4: skipping ahead to (1 2)
bind/1/4: quoted a
bind/1/4: positional arg
bind/1/4: a: 1
bind/1/4: regular arg
bind/1/5: (a b)(b) <-> (1 2)(2)
bind/1/5: skip keyword? (2)
bind/1/5: skipping ahead to (2)
bind/1/5: quoted b
bind/1/5: positional arg
bind/1/5: b: 2
bind/1/5: regular arg
bind/1/6: (a b)nil <-> (1 2)nil
bind/1/3: => {a:1, b:2, }
bind/1/3: ('(a b))nil <-> ((1 2))nil
bind/1/1: => {a:1, b:2, }
- 15 test_eval_handles_improper_list_in_destructured_arg
bind/1/1: ((a ... b)) <-> ((x ... y))
bind/1/2: ((a ... b))((a ... b)) <-> ((x ... y))((x ... y))
bind/1/2: skip keyword? ((x ... y))
bind/1/2: skipping ahead to ((x ... y))
bind/1/2: quoted destructured (a ... b)
bind/1/3: (a ... b) <-> (x ... y)
bind/1/4: (a ... b)(a ... b) <-> (x ... y)(x ... y)
bind/1/4: skip keyword? (x ... y)
bind/1/4: skipping ahead to (x ... y)
bind/1/4: quoted a
bind/1/4: positional arg
bind/1/4: a: x
bind/1/4: non-cons arg
bind/1/4: b: y
bind/1/3: => {a:x, b:y, }
bind/1/3: ((a ... b))nil <-> ((x ... y))nil
bind/1/1: => {a:x, b:y, }
- 16 test_eval_splices_args
bind/1/1: ($x $y) <-> (''3 ''4)
bind/1/2: ($x $y)($x $y) <-> (''3 ''4)(''3 ''4)
bind/1/2: skip keyword? (''3 ''4)
bind/1/2: skipping ahead to (''3 ''4)
bind/1/2: regular $x
bind/1/2: after eval 3
bind/1/2: $x: 3
bind/1/3: ($x $y)($y) <-> (''3 ''4)(''4)
bind/1/3: skip keyword? (''4)
bind/1/3: skipping ahead to (''4)
bind/1/3: regular $y
bind/1/3: after eval 4
bind/1/3: $y: 4
bind/1/4: ($x $y)nil <-> (''3 ''4)nil
bind/1/1: => {$x:3, $y:4, }
- 17 test_eval_splices_args2
bind/1/1: nil <-> (a ''4 ''5 a)
bind/1/2: nilnil <-> (a ''4 ''5 a)(a ''4 ''5 a)
bind/1/1: => {}
- 18 test_eval_splices_args3
bind/1/0: args: (a b)
bind/1/1: (x y) <-> (''a ''b)
bind/1/2: (x y)(x y) <-> (''a ''b)(''a ''b)
bind/1/2: skip keyword? (''a ''b)
bind/1/2: skipping ahead to (''a ''b)
bind/1/2: regular x
bind/1/2: after eval a
bind/1/2: x: a
bind/1/3: (x y)(y) <-> (''a ''b)(''b)
bind/1/3: skip keyword? (''b)
bind/1/3: skipping ahead to (''b)
bind/1/3: regular y
bind/1/3: after eval b
bind/1/3: y: b
bind/1/4: (x y)nil <-> (''a ''b)nil
bind/1/1: => {x:a, y:b, }
- 19 test_eval_splices_nil_args
bind/1/1: nil <-> (a a)
bind/1/2: nilnil <-> (a a)(a a)
bind/1/1: => {}
- 20 test_eval_splices_keyword_syms_into_args
bind/1/1: nil <-> (a ''4 :x a)
bind/1/2: nilnil <-> (a ''4 :x a)(a ''4 :x a)
bind/1/1: => {}
- 21 test_eval_handles_splice_inside_fn_body
bind/1/1: x <-> (1 2)
bind/1/2: xx <-> (1 2)(1 2)
bind/1/2: skip keyword? (1 2)
bind/1/2: skipping ahead to (1 2)
bind/1/2: rest x
bind/1/2: x: (1 2)
bind/1/1: => {x:(1 2), }
- 22 test_eval_handles_splice_and_selective_quoting
bind/1/0: args: (b)
bind/1/1: ('x y) <-> (a ''b)
bind/1/2: ('x y)('x y) <-> (a ''b)(a ''b)
bind/1/2: skip keyword? (a ''b)
bind/1/2: skipping ahead to (a ''b)
bind/1/2: quoted x
bind/1/2: positional arg
bind/1/2: x: a
bind/1/2: regular arg
bind/1/3: ('x y)(y) <-> (a ''b)(''b)
bind/1/3: skip keyword? (''b)
bind/1/3: skipping ahead to (''b)
bind/1/3: regular y
bind/1/3: after eval b
bind/1/3: y: b
bind/1/4: ('x y)nil <-> (a ''b)nil
bind/1/1: => {x:a, y:b, }
- 23 test_eval_overrides_quoted_params_with_spliced_args
bind/1/0: args: (a b)
bind/1/1: (x 'y) <-> (''a ''b)
bind/1/2: (x 'y)(x 'y) <-> (''a ''b)(''a ''b)
bind/1/2: skip keyword? (''a ''b)
bind/1/2: skipping ahead to (''a ''b)
bind/1/2: regular x
bind/1/2: after eval a
bind/1/2: x: a
bind/1/3: (x 'y)('y) <-> (''a ''b)(''b)
bind/1/3: skip keyword? (''b)
bind/1/3: skipping ahead to (''b)
bind/1/3: quoted y
bind/1/3: positional arg
bind/1/3: y: ''b
bind/1/3: regular arg
bind/1/4: (x 'y)nil <-> (''a ''b)nil
bind/1/1: => {x:a, y:''b, }
- 24 test_eval_handles_already_evald_arg
bind/1/1: (x) <-> (''a)
bind/1/2: (x)(x) <-> (''a)(''a)
bind/1/2: skip keyword? (''a)
bind/1/2: skipping ahead to (''a)
bind/1/2: regular x
bind/1/2: after eval a
bind/1/2: x: a
bind/1/3: (x)nil <-> (''a)nil
bind/1/1: => {x:a, }
- 25 test_eval_handles_already_evald_arg_quoted_param
bind/1/1: (x) <-> (''a)
bind/1/2: (x)(x) <-> (''a)(''a)
bind/1/2: skip keyword? (''a)
bind/1/2: skipping ahead to (''a)
bind/1/2: quoted x
bind/1/2: positional arg
bind/1/2: x: ''a
bind/1/2: regular arg
bind/1/3: (x)nil <-> (''a)nil
bind/1/1: => {x:''a, }
- 26 test_eval_handles_multiply_already_evald_arg
bind/1/1: (x) <-> (''''a)
bind/1/2: (x)(x) <-> (''''a)(''''a)
bind/1/2: skip keyword? (''''a)
bind/1/2: skipping ahead to (''''a)
bind/1/2: regular x
bind/1/2: after eval a
bind/1/2: x: a
bind/1/3: (x)nil <-> (''''a)nil
bind/1/1: => {x:a, }
- 27 test_eval_handles_already_evald_rest_arg
bind/1/1: x <-> (''a)
bind/1/2: xx <-> (''a)(''a)
bind/1/2: skip keyword? (''a)
bind/1/2: skipping ahead to (''a)
bind/1/2: rest x
bind/1/2: x: (a)
bind/1/1: => {x:(a), }
- 28 test_eval_splice_on_macros_with_backquote
bind/1/0: args: (a b)
bind/1/1: (x y) <-> (''a ''b)
bind/1/2: (x y)(x y) <-> (''a ''b)(''a ''b)
bind/1/2: skip keyword? (''a ''b)
bind/1/2: skipping ahead to (''a ''b)
bind/1/2: quoted x
bind/1/2: positional arg
bind/1/2: x: ''a
bind/1/2: regular arg
bind/1/3: (x y)(y) <-> (''a ''b)(''b)
bind/1/3: skip keyword? (''b)
bind/1/3: skipping ahead to (''b)
bind/1/3: quoted y
bind/1/3: positional arg
bind/1/3: y: ''b
bind/1/3: regular arg
bind/1/4: (x y)nil <-> (''a ''b)nil
bind/1/1: => {y:''b, x:''a, }
- 29 test_eval_splice_on_backquoteless_macros_warns
bind/1/0: args: (a b)
bind/1/1: (x y) <-> (''a ''b)
bind/1/2: (x y)(x y) <-> (''a ''b)(''a ''b)
bind/1/2: skip keyword? (''a ''b)
bind/1/2: skipping ahead to (''a ''b)
bind/1/2: quoted x
bind/1/2: positional arg
bind/1/2: x: ''a
bind/1/2: regular arg
bind/1/3: (x y)(y) <-> (''a ''b)(''b)
bind/1/3: skip keyword? (''b)
bind/1/3: skipping ahead to (''b)
bind/1/3: quoted y
bind/1/3: positional arg
bind/1/3: y: ''b
bind/1/3: regular arg
bind/1/4: (x y)nil <-> (''a ''b)nil
bind/1/1: => {x:''a, y:''b, }
- 30 test_eval_handles_keyword_args
bind/1/1: (a b) <-> (2 :a 1)
bind/1/2: (a b)(a b) <-> (2 :a 1)(2 :a 1)
bind/1/2: skip keyword? (2 :a 1)
bind/1/2: skipping ahead to (2 :a 1)
bind/1/2: regular a
bind/1/2: regular keyword arg 1
bind/1/2: after eval 1
bind/1/2: a: 1
bind/1/3: (a b)(b) <-> (2 :a 1)(2 :a 1)
bind/1/3: skip keyword? (2 :a 1)
bind/1/3: skipping ahead to (2 :a 1)
bind/1/3: regular b
bind/1/3: after eval 2
bind/1/3: b: 2
bind/1/4: (a b)nil <-> (2 :a 1)(:a 1)
bind/1/1: => {a:1, b:2, }
- 31 test_eval_handles_keyword_args2
bind/1/1: (a b c) <-> (:c 1 2)
bind/1/2: (a b c)(a b c) <-> (:c 1 2)(:c 1 2)
bind/1/2: skip keyword? (:c 1 2)
bind/1/2: match? c in (a b c)
bind/1/2: match? c in (b c)
bind/1/2: match? c in (c)
bind/1/2: skipping keyword arg:c 1
bind/1/2: skip keyword? (2)
bind/1/2: skipping ahead to (2)
bind/1/2: regular a
bind/1/2: after eval 2
bind/1/2: a: 2
bind/1/3: (a b c)(b c) <-> (:c 1 2)nil
bind/1/3: skip keyword? nil
bind/1/3: skipping ahead to nil
bind/1/3: regular b
bind/1/3: after eval nil
bind/1/3: b: nil
bind/1/4: (a b c)(c) <-> (:c 1 2)nil
bind/1/4: skip keyword? nil
bind/1/4: skipping ahead to nil
bind/1/4: regular c
bind/1/4: regular keyword arg 1
bind/1/4: after eval 1
bind/1/4: c: 1
bind/1/5: (a b c)nil <-> (:c 1 2)nil
bind/1/1: => {b:nil, c:1, a:2, }
- 32 test_eval_handles_nil_keyword_arg
bind/1/1: (a) <-> (2 :a nil)
bind/1/2: (a)(a) <-> (2 :a nil)(2 :a nil)
bind/1/2: skip keyword? (2 :a nil)
bind/1/2: skipping ahead to (2 :a nil)
bind/1/2: regular a
bind/1/2: regular keyword arg nil
bind/1/2: after eval nil
bind/1/2: a: nil
bind/1/3: (a)nil <-> (2 :a nil)(2 :a nil)
bind/1/1: => {a:nil, }
- 33 test_eval_handles_quoted_keyword_args
bind/1/1: (a b 'c) <-> (:c 1 2)
bind/1/2: (a b 'c)(a b 'c) <-> (:c 1 2)(:c 1 2)
bind/1/2: skip keyword? (:c 1 2)
bind/1/2: match? c in (a b 'c)
bind/1/2: match? c in (b 'c)
bind/1/2: match? c in ('c)
bind/1/2: skipping keyword arg:c 1
bind/1/2: skip keyword? (2)
bind/1/2: skipping ahead to (2)
bind/1/2: regular a
bind/1/2: after eval 2
bind/1/2: a: 2
bind/1/3: (a b 'c)(b 'c) <-> (:c 1 2)nil
bind/1/3: skip keyword? nil
bind/1/3: skipping ahead to nil
bind/1/3: regular b
bind/1/3: after eval nil
bind/1/3: b: nil
bind/1/4: (a b 'c)('c) <-> (:c 1 2)nil
bind/1/4: skip keyword? nil
bind/1/4: skipping ahead to nil
bind/1/4: quoted c
bind/1/4: keyword arg
bind/1/4: c: 1
bind/1/5: (a b 'c)nil <-> (:c 1 2)nil
bind/1/1: => {b:nil, c:1, a:2, }
- 34 test_eval_handles_quoted_keyword_args2
bind/1/0: x: 1
bind/1/1: (a b 'c) <-> (:c x 2)
bind/1/2: (a b 'c)(a b 'c) <-> (:c x 2)(:c x 2)
bind/1/2: skip keyword? (:c x 2)
bind/1/2: match? c in (a b 'c)
bind/1/2: match? c in (b 'c)
bind/1/2: match? c in ('c)
bind/1/2: skipping keyword arg:c x
bind/1/2: skip keyword? (2)
bind/1/2: skipping ahead to (2)
bind/1/2: regular a
bind/1/2: after eval 2
bind/1/2: a: 2
bind/1/3: (a b 'c)(b 'c) <-> (:c x 2)nil
bind/1/3: skip keyword? nil
bind/1/3: skipping ahead to nil
bind/1/3: regular b
bind/1/3: after eval nil
bind/1/3: b: nil
bind/1/4: (a b 'c)('c) <-> (:c x 2)nil
bind/1/4: skip keyword? nil
bind/1/4: skipping ahead to nil
bind/1/4: quoted c
bind/1/4: keyword arg
bind/1/4: c: x
bind/1/5: (a b 'c)nil <-> (:c x 2)nil
bind/1/1: => {b:nil, a:2, c:x, }
- 35 test_eval_handles_rest_keyword_arg
bind/1/1: (a ... b) <-> (2 :b 1 3)
bind/1/2: (a ... b)(a ... b) <-> (2 :b 1 3)(2 :b 1 3)
bind/1/2: skip keyword? (2 :b 1 3)
bind/1/2: skipping ahead to (2 :b 1 3)
bind/1/2: regular a
bind/1/2: after eval 2
bind/1/2: a: 2
bind/1/3: (a ... b)b <-> (2 :b 1 3)(:b 1 3)
bind/1/3: skip keyword? (:b 1 3)
bind/1/3: match? b in (a ... b)
bind/1/3: match? b in b
bind/1/3: skipping rest keyword args
bind/1/3: skip keyword? nil
bind/1/3: skipping ahead to nil
bind/1/3: rest b
bind/1/3: until next keyword after (:b 1 3)
bind/1/3: rest keyword (1 3)
bind/1/3: b: (1 3)
bind/1/1: => {a:2, b:(1 3), }
- 36 test_eval_handles_rest_keyword_arg2
bind/1/1: (a ... b) <-> (:b 1 2 3)
bind/1/2: (a ... b)(a ... b) <-> (:b 1 2 3)(:b 1 2 3)
bind/1/2: skip keyword? (:b 1 2 3)
bind/1/2: match? b in (a ... b)
bind/1/2: match? b in b
bind/1/2: skipping rest keyword args
bind/1/2: skip keyword? nil
bind/1/2: skipping ahead to nil
bind/1/2: regular a
bind/1/2: after eval nil
bind/1/2: a: nil
bind/1/3: (a ... b)b <-> (:b 1 2 3)nil
bind/1/3: skip keyword? nil
bind/1/3: skipping ahead to nil
bind/1/3: rest b
bind/1/3: until next keyword after (:b 1 2 3)
bind/1/3: rest keyword (1 2 3)
bind/1/3: b: (1 2 3)
bind/1/1: => {a:nil, b:(1 2 3), }
- 37 test_eval_handles_args_after_rest_keyword
bind/1/1: (a ... b) <-> (:b 1 2 :a 3)
bind/1/2: (a ... b)(a ... b) <-> (:b 1 2 :a 3)(:b 1 2 :a 3)
bind/1/2: skip keyword? (:b 1 2 :a 3)
bind/1/2: match? b in (a ... b)
bind/1/2: match? b in b
bind/1/2: skipping rest keyword args
bind/1/2: at keyword sym (:a 3)
bind/1/2: scanning for a
bind/1/2: match? a in (a ... b)
bind/1/2: skip keyword? (:a 3)
bind/1/2: match? a in (a ... b)
bind/1/2: skipping keyword arg:a 3
bind/1/2: skip keyword? nil
bind/1/2: skipping ahead to nil
bind/1/2: regular a
bind/1/2: regular keyword arg 3
bind/1/2: after eval 3
bind/1/2: a: 3
bind/1/3: (a ... b)b <-> (:b 1 2 :a 3)nil
bind/1/3: skip keyword? nil
bind/1/3: skipping ahead to nil
bind/1/3: rest b
bind/1/3: until next keyword after (:b 1 2 :a 3)
bind/1/3: at keyword sym (:a 3)
bind/1/3: scanning for a
bind/1/3: match? a in (a ... b)
bind/1/3: rest keyword (1 2)
bind/1/3: b: (1 2)
bind/1/1: => {a:3, b:(1 2), }
- 38 test_eval_handles_args_after_rest_keyword2
bind/1/1: (a b ... c) <-> (:c 1 2 :b 3)
bind/1/2: (a b ... c)(a b ... c) <-> (:c 1 2 :b 3)(:c 1 2 :b 3)
bind/1/2: skip keyword? (:c 1 2 :b 3)
bind/1/2: match? c in (a b ... c)
bind/1/2: match? c in (b ... c)
bind/1/2: match? c in c
bind/1/2: skipping rest keyword args
bind/1/2: at keyword sym (:b 3)
bind/1/2: scanning for b
bind/1/2: match? b in (a b ... c)
bind/1/2: match? b in (b ... c)
bind/1/2: skip keyword? (:b 3)
bind/1/2: match? b in (a b ... c)
bind/1/2: match? b in (b ... c)
bind/1/2: skipping keyword arg:b 3
bind/1/2: skip keyword? nil
bind/1/2: skipping ahead to nil
bind/1/2: regular a
bind/1/2: after eval nil
bind/1/2: a: nil
bind/1/3: (a b ... c)(b ... c) <-> (:c 1 2 :b 3)nil
bind/1/3: skip keyword? nil
bind/1/3: skipping ahead to nil
bind/1/3: regular b
bind/1/3: regular keyword arg 3
bind/1/3: after eval 3
bind/1/3: b: 3
bind/1/4: (a b ... c)c <-> (:c 1 2 :b 3)nil
bind/1/4: skip keyword? nil
bind/1/4: skipping ahead to nil
bind/1/4: rest c
bind/1/4: until next keyword after (:c 1 2 :b 3)
bind/1/4: at keyword sym (:b 3)
bind/1/4: scanning for b
bind/1/4: match? b in (a b ... c)
bind/1/4: match? b in (b ... c)
bind/1/4: rest keyword (1 2)
bind/1/4: c: (1 2)
bind/1/1: => {a:nil, b:3, c:(1 2), }
- 39 test_eval_handles_quoted_rest_keyword_arg
bind/1/0: x: 2
bind/1/1: (a ' ... b) <-> (:b 1 x 3)
bind/1/2: (a ' ... b)(a ' ... b) <-> (:b 1 x 3)(:b 1 x 3)
bind/1/2: skip keyword? (:b 1 x 3)
bind/1/2: match? b in (a ' ... b)
bind/1/2: match? b in 'b
bind/1/2: skipping rest keyword args
bind/1/2: skip keyword? nil
bind/1/2: skipping ahead to nil
bind/1/2: regular a
bind/1/2: after eval nil
bind/1/2: a: nil
bind/1/3: (a ' ... b)'b <-> (:b 1 x 3)nil
bind/1/3: skip keyword? nil
bind/1/3: skipping ahead to nil
bind/1/3: quoted rest b
bind/1/3: quoted rest keyword (1 x 3)
bind/1/3: b: (1 x 3)
bind/1/1: => {b:(1 x 3), a:nil, }
- 40 test_eval_handles_non_keyword_arg_colon_syms
bind/1/1: (a b) <-> (:x 1)
bind/1/2: (a b)(a b) <-> (:x 1)(:x 1)
bind/1/2: skip keyword? (:x 1)
bind/1/2: match? x in (a b)
bind/1/2: match? x in (b)
bind/1/2: match? x in nil
bind/1/2: done skipping (:x 1)
bind/1/2: skipping ahead to (:x 1)
bind/1/2: regular a
bind/1/2: after eval :x
bind/1/2: a: :x
bind/1/3: (a b)(b) <-> (:x 1)(1)
bind/1/3: skip keyword? (1)
bind/1/3: skipping ahead to (1)
bind/1/3: regular b
bind/1/3: after eval 1
bind/1/3: b: 1
bind/1/4: (a b)nil <-> (:x 1)nil
bind/1/1: => {b:1, a::x, }
- 41 test_eval_handles_keyword_args_inside_splice
bind/1/1: (a b) <-> (''3 :a ''4)
bind/1/2: (a b)(a b) <-> (''3 :a ''4)(''3 :a ''4)
bind/1/2: skip keyword? (''3 :a ''4)
bind/1/2: skipping ahead to (''3 :a ''4)
bind/1/2: regular a
bind/1/2: regular keyword arg ''4
bind/1/2: after eval 4
bind/1/2: a: 4
bind/1/3: (a b)(b) <-> (''3 :a ''4)(''3 :a ''4)
bind/1/3: skip keyword? (''3 :a ''4)
bind/1/3: skipping ahead to (''3 :a ''4)
bind/1/3: regular b
bind/1/3: after eval 3
bind/1/3: b: 3
bind/1/4: (a b)nil <-> (''3 :a ''4)(:a ''4)
bind/1/1: => {a:4, b:3, }
- 42 test_eval_handles_keyword_args_inside_destructured_params
bind/1/1: ((a b)) <-> ('(3 :a 4))
bind/1/2: ((a b))((a b)) <-> ('(3 :a 4))('(3 :a 4))
bind/1/2: skip keyword? ('(3 :a 4))
bind/1/2: skipping ahead to ('(3 :a 4))
bind/1/2: destructured (a b)
bind/1/2: regular arg
bind/1/3: (a b) <-> (3 :a 4)
bind/1/4: (a b)(a b) <-> (3 :a 4)(3 :a 4)
bind/1/4: skip keyword? (3 :a 4)
bind/1/4: skipping ahead to (3 :a 4)
bind/1/4: quoted a
bind/1/4: keyword arg
bind/1/4: a: 4
bind/1/5: (a b)(b) <-> (3 :a 4)(3 :a 4)
bind/1/5: skip keyword? (3 :a 4)
bind/1/5: skipping ahead to (3 :a 4)
bind/1/5: quoted b
bind/1/5: positional arg
bind/1/5: b: 3
bind/1/5: regular arg
bind/1/6: (a b)nil <-> (3 :a 4)(:a 4)
bind/1/3: => {a:4, b:3, }
bind/1/3: ((a b))nil <-> ('(3 :a 4))nil
bind/1/1: => {a:4, b:3, }
- 43 test_eval_handles_param_aliases
bind/1/1: ((| x y)) <-> (4)
bind/1/2: ((| x y))((| x y)) <-> (4)(4)
bind/1/2: skip keyword? (4)
bind/1/2: skipping ahead to (4)
bind/1/2: alias (| x y)
bind/1/2: positional arg
bind/1/3: alias sym x
bind/1/3: eval arg
bind/1/3: x: 4
bind/1/3: alias sym y
bind/1/3: y: 4
bind/1/3: ((| x y))nil <-> (4)nil
bind/1/1: => {y:4, x:4, }
- 44 test_eval_handles_aliased_keyword_args
bind/1/1: (a b (| c x)) <-> (:x 1 2)
bind/1/2: (a b (| c x))(a b (| c x)) <-> (:x 1 2)(:x 1 2)
bind/1/2: skip keyword? (:x 1 2)
bind/1/2: match? x in (a b (| c x))
bind/1/2: match? x in (b (| c x))
bind/1/2: match? x in ((| c x))
bind/1/2: match alias
bind/1/2: skipping keyword arg:x 1
bind/1/2: skip keyword? (2)
bind/1/2: skipping ahead to (2)
bind/1/2: regular a
bind/1/2: after eval 2
bind/1/2: a: 2
bind/1/3: (a b (| c x))(b (| c x)) <-> (:x 1 2)nil
bind/1/3: skip keyword? nil
bind/1/3: skipping ahead to nil
bind/1/3: regular b
bind/1/3: after eval nil
bind/1/3: b: nil
bind/1/4: (a b (| c x))((| c x)) <-> (:x 1 2)nil
bind/1/4: skip keyword? nil
bind/1/4: skipping ahead to nil
bind/1/4: alias (| c x)
bind/1/4: keyword arg
bind/1/5: alias sym c
bind/1/5: eval arg
bind/1/5: c: 1
bind/1/5: alias sym x
bind/1/5: x: 1
bind/1/5: (a b (| c x))nil <-> (:x 1 2)nil
bind/1/1: => {b:nil, x:1, a:2, c:1, }
- 45 test_eval_handles_aliased_keyword_args2
bind/1/1: ((| a x) b) <-> (:x 1 2)
bind/1/2: ((| a x) b)((| a x) b) <-> (:x 1 2)(:x 1 2)
bind/1/2: skip keyword? (:x 1 2)
bind/1/2: match? x in ((| a x) b)
bind/1/2: match alias
bind/1/2: skipping keyword arg:x 1
bind/1/2: skip keyword? (2)
bind/1/2: skipping ahead to (2)
bind/1/2: alias (| a x)
bind/1/2: keyword arg
bind/1/3: alias sym a
bind/1/3: eval arg
bind/1/3: a: 1
bind/1/3: alias sym x
bind/1/3: x: 1
bind/1/3: ((| a x) b)(b) <-> (:x 1 2)(2)
bind/1/3: skip keyword? (2)
bind/1/3: skipping ahead to (2)
bind/1/3: regular b
bind/1/3: after eval 2
bind/1/3: b: 2
bind/1/4: ((| a x) b)nil <-> (:x 1 2)nil
bind/1/1: => {x:1, b:2, a:1, }
- 46 test_eval_handles_quoted_param_aliases
bind/1/1: ((| a 'b)) <-> (x)
bind/1/2: ((| a 'b))((| a 'b)) <-> (x)(x)
bind/1/2: skip keyword? (x)
bind/1/2: skipping ahead to (x)
bind/1/2: alias (| a 'b)
bind/1/2: positional arg
bind/1/3: alias sym a
bind/1/3: eval arg
bind/1/3: a: 3
bind/1/3: quoted alias 'b
bind/1/3: b: x
bind/1/3: ((| a 'b))nil <-> (x)nil
bind/1/1: => {a:3, b:x, }
- 47 test_eval_handles_quoted_param_aliases2
bind/1/1: ((| x y)) <-> (a)
bind/1/2: ((| x y))((| x y)) <-> (a)(a)
bind/1/2: skip keyword? (a)
bind/1/2: skipping ahead to (a)
bind/1/2: alias (| x y)
bind/1/2: positional arg
bind/1/3: quoted alias x
bind/1/3: x: a
bind/1/3: quoted alias y
bind/1/3: y: a
bind/1/3: ((| x y))nil <-> (a)nil
bind/1/1: => {x:a, y:a, }
- 48 test_eval_handles_aliased_rest_keyword_args
bind/1/1: (a | body do) <-> (2 :do 1 3)
bind/1/2: (a | body do)(a | body do) <-> (2 :do 1 3)(2 :do 1 3)
bind/1/2: skip keyword? (2 :do 1 3)
bind/1/2: skipping ahead to (2 :do 1 3)
bind/1/2: regular a
bind/1/2: after eval 2
bind/1/2: a: 2
bind/1/3: (a | body do)(| body do) <-> (2 :do 1 3)(:do 1 3)
bind/1/3: skip keyword? (:do 1 3)
bind/1/3: match? do in (a | body do)
bind/1/3: match? do in (| body do)
bind/1/3: match rest alias
bind/1/3: skipping rest keyword args
bind/1/3: skip keyword? nil
bind/1/3: skipping ahead to nil
bind/1/3: rest alias (| body do)
bind/1/3: until next keyword after (:do 1 3)
bind/1/3: rest alias keyword (1 3)
bind/1/3: rest alias sym body
bind/1/3: body: (1 3)
bind/1/3: do: (1 3)
bind/1/1: => {body:(1 3), do:(1 3), a:2, }
- 49 test_eval_handles_aliased_rest_keyword_args2
bind/1/1: (a b | body do) <-> (2 :do 1 3)
bind/1/2: (a b | body do)(a b | body do) <-> (2 :do 1 3)(2 :do 1 3)
bind/1/2: skip keyword? (2 :do 1 3)
bind/1/2: skipping ahead to (2 :do 1 3)
bind/1/2: regular a
bind/1/2: after eval 2
bind/1/2: a: 2
bind/1/3: (a b | body do)(b | body do) <-> (2 :do 1 3)(:do 1 3)
bind/1/3: skip keyword? (:do 1 3)
bind/1/3: match? do in (a b | body do)
bind/1/3: match? do in (b | body do)
bind/1/3: match? do in (| body do)
bind/1/3: match rest alias
bind/1/3: skipping rest keyword args
bind/1/3: skip keyword? nil
bind/1/3: skipping ahead to nil
bind/1/3: regular b
bind/1/3: after eval nil
bind/1/3: b: nil
bind/1/4: (a b | body do)(| body do) <-> (2 :do 1 3)nil
bind/1/4: skip keyword? nil
bind/1/4: skipping ahead to nil
bind/1/4: rest alias (| body do)
bind/1/4: until next keyword after (:do 1 3)
bind/1/4: rest alias keyword (1 3)
bind/1/4: rest alias sym body
bind/1/4: body: (1 3)
bind/1/4: do: (1 3)
bind/1/1: => {body:(1 3), a:2, do:(1 3), b:nil, }
- 50 test_eval_handles_quoted_rest_param_aliases
bind/1/1: (| a 'b) <-> (x)
bind/1/2: (| a 'b)(| a 'b) <-> (x)(x)
bind/1/2: skip keyword? (x)
bind/1/2: skipping ahead to (x)
bind/1/2: rest alias (| a 'b)
bind/1/2: rest alias sym a
bind/1/2: a: (3)
bind/1/2: quoted rest alias 'b
bind/1/2: b: (x)
bind/1/1: => {a:(3), b:(x), }
- 51 test_eval_handles_quoted_rest_param_aliases2
bind/1/1: (| a b) <-> (x)
bind/1/2: (| a b)(| a b) <-> (x)(x)
bind/1/2: skip keyword? (x)
bind/1/2: skipping ahead to (x)
bind/1/2: rest alias (| a b)
bind/1/2: quoted rest alias sym a
bind/1/2: a: (x)
bind/1/2: b: (x)
bind/1/1: => {b:(x), a:(x), }
- 52 test_eval_binds_as_params
bind/1/1: (| a (b c)) <-> (1 2)
bind/1/2: (| a (b c))(| a (b c)) <-> (1 2)(1 2)
bind/1/2: skip keyword? (1 2)
bind/1/2: skipping ahead to (1 2)
bind/1/2: rest alias (| a (b c))
bind/1/2: rest alias sym a
bind/1/2: a: (1 2)
bind/1/2: multiple rest aliases (b c)
bind/1/2: rest alias cons (as-param) (b c)
bind/1/3: (b c) <-> (1 2)
bind/1/4: (b c)(b c) <-> (1 2)(1 2)
bind/1/4: skip keyword? (1 2)
bind/1/4: skipping ahead to (1 2)
bind/1/4: regular b
bind/1/4: after eval 1
bind/1/4: b: 1
bind/1/5: (b c)(c) <-> (1 2)(2)
bind/1/5: skip keyword? (2)
bind/1/5: skipping ahead to (2)
bind/1/5: regular c
bind/1/5: after eval 2
bind/1/5: c: 2
bind/1/6: (b c)nil <-> (1 2)nil
bind/1/3: => {b:1, c:2, a:(1 2), }
bind/1/1: => {b:1, c:2, a:(1 2), }
- 53 test_eval_binds_as_params2
bind/1/1: ((| a (b c))) <-> (x)
bind/1/2: ((| a (b c)))((| a (b c))) <-> (x)(x)
bind/1/2: skip keyword? (x)
bind/1/2: skipping ahead to (x)
bind/1/2: alias (| a (b c))
bind/1/2: positional arg
bind/1/3: alias sym a
bind/1/3: eval arg
bind/1/3: a: (1 2)
bind/1/3: destructured alias (as-param) (b c)
bind/1/4: (b c) <-> (1 2)
bind/1/5: (b c)(b c) <-> (1 2)(1 2)
bind/1/5: skip keyword? (1 2)
bind/1/5: skipping ahead to (1 2)
bind/1/5: quoted b
bind/1/5: positional arg
bind/1/5: b: 1
bind/1/5: regular arg
bind/1/6: (b c)(c) <-> (1 2)(2)
bind/1/6: skip keyword? (2)
bind/1/6: skipping ahead to (2)
bind/1/6: quoted c
bind/1/6: positional arg
bind/1/6: c: 2
bind/1/6: regular arg
bind/1/7: (b c)nil <-> (1 2)nil
bind/1/4: => {b:1, c:2, a:(1 2), }
bind/1/3: ((| a (b c)))nil <-> (x)nil
bind/1/1: => {b:1, c:2, a:(1 2), }
- 54 test_eval_binds_as_params_with_keyword_args
bind/1/1: (| a (b c)) <-> (1 :b 2)
bind/1/2: (| a (b c))(| a (b c)) <-> (1 :b 2)(1 :b 2)
bind/1/2: skip keyword? (1 :b 2)
bind/1/2: skipping ahead to (1 :b 2)
bind/1/2: rest alias (| a (b c))
bind/1/2: rest alias sym a
bind/1/2: a: (1 :b 2)
bind/1/2: multiple rest aliases (b c)
bind/1/2: rest alias cons (as-param) (b c)
bind/1/3: (b c) <-> (1 :b 2)
bind/1/4: (b c)(b c) <-> (1 :b 2)(1 :b 2)
bind/1/4: skip keyword? (1 :b 2)
bind/1/4: skipping ahead to (1 :b 2)
bind/1/4: regular b
bind/1/4: regular keyword arg 2
bind/1/4: after eval 2
bind/1/4: b: 2
bind/1/5: (b c)(c) <-> (1 :b 2)(1 :b 2)
bind/1/5: skip keyword? (1 :b 2)
bind/1/5: skipping ahead to (1 :b 2)
bind/1/5: regular c
bind/1/5: after eval 1
bind/1/5: c: 1
bind/1/6: (b c)nil <-> (1 :b 2)(:b 2)
bind/1/3: => {b:2, c:1, a:(1 :b 2), }
bind/1/1: => {b:2, c:1, a:(1 :b 2), }
- 55 test_eval_binds_as_params_with_keyword_args2
bind/1/1: (| a (b c d)) <-> (1 :b 2 3)
bind/1/2: (| a (b c d))(| a (b c d)) <-> (1 :b 2 3)(1 :b 2 3)
bind/1/2: skip keyword? (1 :b 2 3)
bind/1/2: skipping ahead to (1 :b 2 3)
bind/1/2: rest alias (| a (b c d))
bind/1/2: rest alias sym a
bind/1/2: a: (1 :b 2 3)
bind/1/2: multiple rest aliases (b c d)
bind/1/2: rest alias cons (as-param) (b c d)
bind/1/3: (b c d) <-> (1 :b 2 3)
bind/1/4: (b c d)(b c d) <-> (1 :b 2 3)(1 :b 2 3)
bind/1/4: skip keyword? (1 :b 2 3)
bind/1/4: skipping ahead to (1 :b 2 3)
bind/1/4: regular b
bind/1/4: regular keyword arg 2
bind/1/4: after eval 2
bind/1/4: b: 2
bind/1/5: (b c d)(c d) <-> (1 :b 2 3)(1 :b 2 3)
bind/1/5: skip keyword? (1 :b 2 3)
bind/1/5: skipping ahead to (1 :b 2 3)
bind/1/5: regular c
bind/1/5: after eval 1
bind/1/5: c: 1
bind/1/6: (b c d)(d) <-> (1 :b 2 3)(:b 2 3)
bind/1/6: skip keyword? (:b 2 3)
bind/1/6: match? b in (b c d)
bind/1/6: skipping keyword arg:b 2
bind/1/6: skip keyword? (3)
bind/1/6: skipping ahead to (3)
bind/1/6: regular d
bind/1/6: after eval 3
bind/1/6: d: 3
bind/1/7: (b c d)nil <-> (1 :b 2 3)nil
bind/1/3: => {d:3, b:2, c:1, a:(1 :b 2 3), }
bind/1/1: => {d:3, b:2, c:1, a:(1 :b 2 3), }
- 56 test_eval_warns_on_keyword_args_for_conflicting_aliases
bind/1/1: ((| a b) (| c d)) <-> (:c 1 :d 2)
bind/1/2: ((| a b) (| c d))((| a b) (| c d)) <-> (:c 1 :d 2)(:c 1 :d 2)
bind/1/2: skip keyword? (:c 1 :d 2)
bind/1/2: match? c in ((| a b) (| c d))
bind/1/2: match alias
bind/1/2: match? c in ((| c d))
bind/1/2: match alias
bind/1/2: skipping keyword arg:c 1
bind/1/2: skip keyword? (:d 2)
bind/1/2: match? d in ((| a b) (| c d))
bind/1/2: match alias
bind/1/2: match? d in ((| c d))
bind/1/2: match alias
bind/1/2: skipping keyword arg:d 2
bind/1/2: skip keyword? nil
bind/1/2: skipping ahead to nil
bind/1/2: alias (| a b)
bind/1/2: positional arg
bind/1/3: alias sym a
bind/1/3: eval arg
bind/1/3: a: nil
bind/1/3: alias sym b
bind/1/3: b: nil
bind/1/3: ((| a b) (| c d))((| c d)) <-> (:c 1 :d 2)nil
bind/1/3: skip keyword? nil
bind/1/3: skipping ahead to nil
bind/1/3: alias (| c d)
bind/1/3: keyword arg
bind/1/4: alias sym c
bind/1/4: eval arg
bind/1/4: c: 2
bind/1/4: alias sym d
bind/1/4: d: 2
bind/1/4: ((| a b) (| c d))nil <-> (:c 1 :d 2)nil
bind/1/1: => {b:nil, d:2, a:nil, c:2, }
- 57 test_eval_binds_as_params_recursively
bind/1/1: (| a (b | c (d e))) <-> (1 2 3)
bind/1/2: (| a (b | c (d e)))(| a (b | c (d e))) <-> (1 2 3)(1 2 3)
bind/1/2: skip keyword? (1 2 3)
bind/1/2: skipping ahead to (1 2 3)
bind/1/2: rest alias (| a (b | c (d e)))
bind/1/2: rest alias sym a
bind/1/2: a: (1 2 3)
bind/1/2: multiple rest aliases (b | c (d e))
bind/1/2: rest alias cons (as-param) (b | c (d e))
bind/1/3: (b | c (d e)) <-> (1 2 3)
bind/1/4: (b | c (d e))(b | c (d e)) <-> (1 2 3)(1 2 3)
bind/1/4: skip keyword? (1 2 3)
bind/1/4: skipping ahead to (1 2 3)
bind/1/4: regular b
bind/1/4: after eval 1
bind/1/4: b: 1
bind/1/5: (b | c (d e))(| c (d e)) <-> (1 2 3)(2 3)
bind/1/5: skip keyword? (2 3)
bind/1/5: skipping ahead to (2 3)
bind/1/5: rest alias (| c (d e))
bind/1/5: rest alias sym c
bind/1/5: c: (2 3)
bind/1/5: multiple rest aliases (d e)
bind/1/5: rest alias cons (as-param) (d e)
bind/1/6: (d e) <-> (2 3)
bind/1/7: (d e)(d e) <-> (2 3)(2 3)
bind/1/7: skip keyword? (2 3)
bind/1/7: skipping ahead to (2 3)
bind/1/7: regular d
bind/1/7: after eval 2
bind/1/7: d: 2
bind/1/8: (d e)(e) <-> (2 3)(3)
bind/1/8: skip keyword? (3)
bind/1/8: skipping ahead to (3)
bind/1/8: regular e
bind/1/8: after eval 3
bind/1/8: e: 3
bind/1/9: (d e)nil <-> (2 3)nil
bind/1/6: => {d:2, e:3, a:(1 2 3), c:(2 3), b:1, }
bind/1/3: => {d:2, e:3, a:(1 2 3), c:(2 3), b:1, }
bind/1/1: => {d:2, e:3, a:(1 2 3), c:(2 3), b:1, }
- 58 test_eval_binds_as_params_recursively_using_keyword_args
bind/1/1: (| a (b | c (d e))) <-> (1 :e 2 3)
bind/1/2: (| a (b | c (d e)))(| a (b | c (d e))) <-> (1 :e 2 3)(1 :e 2 3)
bind/1/2: skip keyword? (1 :e 2 3)
bind/1/2: skipping ahead to (1 :e 2 3)
bind/1/2: rest alias (| a (b | c (d e)))
bind/1/2: rest alias sym a
bind/1/2: a: (1 :e 2 3)
bind/1/2: multiple rest aliases (b | c (d e))
bind/1/2: rest alias cons (as-param) (b | c (d e))
bind/1/3: (b | c (d e)) <-> (1 :e 2 3)
bind/1/4: (b | c (d e))(b | c (d e)) <-> (1 :e 2 3)(1 :e 2 3)
bind/1/4: skip keyword? (1 :e 2 3)
bind/1/4: skipping ahead to (1 :e 2 3)
bind/1/4: regular b
bind/1/4: after eval 1
bind/1/4: b: 1
bind/1/5: (b | c (d e))(| c (d e)) <-> (1 :e 2 3)(:e 2 3)
bind/1/5: skip keyword? (:e 2 3)
bind/1/5: match? e in (b | c (d e))
bind/1/5: match? e in (| c (d e))
bind/1/5: match rest alias
bind/1/5: match? e in (c (d e))
bind/1/5: match? e in ((d e))
bind/1/5: match? e in nil
bind/1/5: done skipping (:e 2 3)
bind/1/5: skipping ahead to (:e 2 3)
bind/1/5: rest alias (| c (d e))
bind/1/5: rest alias sym c
bind/1/5: c: (:e 2 3)
bind/1/5: multiple rest aliases (d e)
bind/1/5: rest alias cons (as-param) (d e)
bind/1/6: (d e) <-> (:e 2 3)
bind/1/7: (d e)(d e) <-> (:e 2 3)(:e 2 3)
bind/1/7: skip keyword? (:e 2 3)
bind/1/7: match? e in (d e)
bind/1/7: match? e in (e)
bind/1/7: skipping keyword arg:e 2
bind/1/7: skip keyword? (3)
bind/1/7: skipping ahead to (3)
bind/1/7: regular d
bind/1/7: after eval 3
bind/1/7: d: 3
bind/1/8: (d e)(e) <-> (:e 2 3)nil
bind/1/8: skip keyword? nil
bind/1/8: skipping ahead to nil
bind/1/8: regular e
bind/1/8: regular keyword arg 2
bind/1/8: after eval 2
bind/1/8: e: 2
bind/1/9: (d e)nil <-> (:e 2 3)nil
bind/1/6: => {e:2, a:(1 :e 2 3), c:(:e 2 3), b:1, d:3, }
bind/1/3: => {e:2, a:(1 :e 2 3), c:(:e 2 3), b:1, d:3, }
bind/1/1: => {e:2, a:(1 :e 2 3), c:(:e 2 3), b:1, d:3, }
- 59 test_eval_binds_quoted_as_params_recursively_using_keyword_args
bind/1/1: (| 'a ((| b c))) <-> (:b 1)
bind/1/2: (| 'a ((| b c)))(| 'a ((| b c))) <-> (:b 1)(:b 1)
bind/1/2: skip keyword? (:b 1)
bind/1/2: match? b in (| 'a ((| b c)))
bind/1/2: match rest alias
bind/1/2: match? b in ('a ((| b c)))
bind/1/2: match? b in (((| b c)))
bind/1/2: match? b in nil
bind/1/2: done skipping (:b 1)
bind/1/2: skipping ahead to (:b 1)
bind/1/2: rest alias (| 'a ((| b c)))
bind/1/2: quoted rest alias 'a
bind/1/2: a: (:b 1)
bind/1/2: multiple rest aliases ((| b c))
bind/1/2: rest alias cons (as-param) ((| b c))
bind/1/3: ((| b c)) <-> (:b 1)
bind/1/4: ((| b c))((| b c)) <-> (:b 1)(:b 1)
bind/1/4: skip keyword? (:b 1)
bind/1/4: match? b in ((| b c))
bind/1/4: match alias
bind/1/4: skipping keyword arg:b 1
bind/1/4: skip keyword? nil
bind/1/4: skipping ahead to nil
bind/1/4: alias (| b c)
bind/1/4: keyword arg
bind/1/5: alias sym b
bind/1/5: eval arg
bind/1/5: b: 1
bind/1/5: alias sym c
bind/1/5: c: 1
bind/1/5: ((| b c))nil <-> (:b 1)nil
bind/1/3: => {b:1, a:(:b 1), c:1, }
bind/1/1: => {b:1, a:(:b 1), c:1, }
- 60 test_eval_binds_quoted_rest_as_params_recursively_using_keyword_args
bind/1/1: (| 'a (b | c (d e))) <-> (1 :e 2 3)
bind/1/2: (| 'a (b | c (d e)))(| 'a (b | c (d e))) <-> (1 :e 2 3)(1 :e 2 3)
bind/1/2: skip keyword? (1 :e 2 3)
bind/1/2: skipping ahead to (1 :e 2 3)
bind/1/2: rest alias (| 'a (b | c (d e)))
bind/1/2: quoted rest alias 'a
bind/1/2: a: (1 :e 2 3)
bind/1/2: multiple rest aliases (b | c (d e))
bind/1/2: rest alias cons (as-param) (b | c (d e))
bind/1/3: (b | c (d e)) <-> (1 :e 2 3)
bind/1/4: (b | c (d e))(b | c (d e)) <-> (1 :e 2 3)(1 :e 2 3)
bind/1/4: skip keyword? (1 :e 2 3)
bind/1/4: skipping ahead to (1 :e 2 3)
bind/1/4: regular b
bind/1/4: after eval 1
bind/1/4: b: 1
bind/1/5: (b | c (d e))(| c (d e)) <-> (1 :e 2 3)(:e 2 3)
bind/1/5: skip keyword? (:e 2 3)
bind/1/5: match? e in (b | c (d e))
bind/1/5: match? e in (| c (d e))
bind/1/5: match rest alias
bind/1/5: match? e in (c (d e))
bind/1/5: match? e in ((d e))
bind/1/5: match? e in nil
bind/1/5: done skipping (:e 2 3)
bind/1/5: skipping ahead to (:e 2 3)
bind/1/5: rest alias (| c (d e))
bind/1/5: rest alias sym c
bind/1/5: c: (:e 2 3)
bind/1/5: multiple rest aliases (d e)
bind/1/5: rest alias cons (as-param) (d e)
bind/1/6: (d e) <-> (:e 2 3)
bind/1/7: (d e)(d e) <-> (:e 2 3)(:e 2 3)
bind/1/7: skip keyword? (:e 2 3)
bind/1/7: match? e in (d e)
bind/1/7: match? e in (e)
bind/1/7: skipping keyword arg:e 2
bind/1/7: skip keyword? (3)
bind/1/7: skipping ahead to (3)
bind/1/7: regular d
bind/1/7: after eval 3
bind/1/7: d: 3
bind/1/8: (d e)(e) <-> (:e 2 3)nil
bind/1/8: skip keyword? nil
bind/1/8: skipping ahead to nil
bind/1/8: regular e
bind/1/8: regular keyword arg 2
bind/1/8: after eval 2
bind/1/8: e: 2
bind/1/9: (d e)nil <-> (:e 2 3)nil
bind/1/6: => {e:2, a:(1 :e 2 3), c:(:e 2 3), b:1, d:3, }
bind/1/3: => {e:2, a:(1 :e 2 3), c:(:e 2 3), b:1, d:3, }
bind/1/1: => {e:2, a:(1 :e 2 3), c:(:e 2 3), b:1, d:3, }
- 61 test_eval_binds_quoted_as_params
bind/1/1: (| 'a ('b)) <-> (x)
bind/1/2: (| 'a ('b))(| 'a ('b)) <-> (x)(x)
bind/1/2: skip keyword? (x)
bind/1/2: skipping ahead to (x)
bind/1/2: rest alias (| 'a ('b))
bind/1/2: quoted rest alias 'a
bind/1/2: a: (x)
bind/1/2: multiple rest aliases ('b)
bind/1/2: rest alias cons (as-param) ('b)
bind/1/3: ('b) <-> (x)
bind/1/4: ('b)('b) <-> (x)(x)
bind/1/4: skip keyword? (x)
bind/1/4: skipping ahead to (x)
bind/1/4: quoted b
bind/1/4: positional arg
bind/1/4: b: x
bind/1/4: regular arg
bind/1/5: ('b)nil <-> (x)nil
bind/1/3: => {b:x, a:(x), }
bind/1/1: => {b:x, a:(x), }
- 62 test_eval_binds_quoted_as_params2
bind/1/1: (| a ('b)) <-> (x)
bind/1/2: (| a ('b))(| a ('b)) <-> (x)(x)
bind/1/2: skip keyword? (x)
bind/1/2: skipping ahead to (x)
bind/1/2: rest alias (| a ('b))
bind/1/2: rest alias sym a
bind/1/2: a: (3)
bind/1/2: multiple rest aliases ('b)
bind/1/2: rest alias cons (as-param) ('b)
bind/1/3: ('b) <-> (x)
bind/1/4: ('b)('b) <-> (x)(x)
bind/1/4: skip keyword? (x)
bind/1/4: skipping ahead to (x)
bind/1/4: quoted b
bind/1/4: positional arg
bind/1/4: b: x
bind/1/4: regular arg
bind/1/5: ('b)nil <-> (x)nil
bind/1/3: => {a:(3), b:x, }
bind/1/1: => {a:(3), b:x, }
- 63 test_eval_binds_destructured_as_params
bind/1/1: ((| a (b c))) <-> ('(1 2))
bind/1/2: ((| a (b c)))((| a (b c))) <-> ('(1 2))('(1 2))
bind/1/2: skip keyword? ('(1 2))
bind/1/2: skipping ahead to ('(1 2))
bind/1/2: alias (| a (b c))
bind/1/2: positional arg
bind/1/3: alias sym a
bind/1/3: eval arg
bind/1/3: a: (1 2)
bind/1/3: destructured alias (as-param) (b c)
bind/1/4: (b c) <-> (1 2)
bind/1/5: (b c)(b c) <-> (1 2)(1 2)
bind/1/5: skip keyword? (1 2)
bind/1/5: skipping ahead to (1 2)
bind/1/5: quoted b
bind/1/5: positional arg
bind/1/5: b: 1
bind/1/5: regular arg
bind/1/6: (b c)(c) <-> (1 2)(2)
bind/1/6: skip keyword? (2)
bind/1/6: skipping ahead to (2)
bind/1/6: quoted c
bind/1/6: positional arg
bind/1/6: c: 2
bind/1/6: regular arg
bind/1/7: (b c)nil <-> (1 2)nil
bind/1/4: => {b:1, c:2, a:(1 2), }
bind/1/3: ((| a (b c)))nil <-> ('(1 2))nil
bind/1/1: => {b:1, c:2, a:(1 2), }
- 64 test_eval_warns_on_unary_as
bind/1/1: (| a) <-> (1 2)
bind/1/2: (| a)(| a) <-> (1 2)(1 2)
bind/1/2: skip keyword? (1 2)
bind/1/2: skipping ahead to (1 2)
bind/1/2: rest alias (| a)
bind/1/2: rest alias sym a
bind/1/2: a: (1 2)
bind/1/1: => {a:(1 2), }
- 65 test_eval_warns_on_unary_as2
bind/1/1: ((| a)) <-> (1 2)
bind/1/2: ((| a))((| a)) <-> (1 2)(1 2)
bind/1/2: skip keyword? (1 2)
bind/1/2: skipping ahead to (1 2)
bind/1/2: alias (| a)
bind/1/2: positional arg
bind/1/3: alias sym a
bind/1/3: eval arg
bind/1/3: a: 1
bind/1/3: ((| a))nil <-> (1 2)(2)
bind/1/1: => {a:1, }
- 66 test_eval_warns_on_conflicting_as_params
bind/1/1: (| (a b) c) <-> (1 2)
bind/1/2: (| (a b) c)(| (a b) c) <-> (1 2)(1 2)
bind/1/2: skip keyword? (1 2)
bind/1/2: skipping ahead to (1 2)
bind/1/2: rest alias (| (a b) c)
bind/1/2: rest alias sym c
bind/1/2: c: (1 2)
bind/1/1: => {c:(1 2), }
- 67 test_eval_warns_on_double_quoting
bind/1/1: (''a) <-> (1)
bind/1/2: (''a)(''a) <-> (1)(1)
bind/1/2: skip keyword? (1)
bind/1/2: skipping ahead to (1)
bind/1/2: quoted destructured 'a
bind/1/3: 'a <-> 1
bind/1/3: a: 1
bind/1/3: => {a:1, }
bind/1/3: (''a)nil <-> (1)nil
bind/1/1: => {a:1, }
- 68 test_eval_warns_on_double_quoting2
bind/1/1: 'a <-> (1)
bind/1/2: 'a'a <-> (1)(1)
bind/1/2: skip keyword? (1)
bind/1/2: skipping ahead to (1)
bind/1/1: => {}
- 69 test_eval_warns_on_double_quoting3
bind/1/1: ('a) <-> (1)
bind/1/2: ('a)('a) <-> (1)(1)
bind/1/2: skip keyword? (1)
bind/1/2: skipping ahead to (1)
bind/1/1: => {}
- 70 test_eval_warns_on_double_quoting4
bind/1/1: (a 'b) <-> (1 2)
bind/1/2: (a 'b)(a 'b) <-> (1 2)(1 2)
bind/1/2: skip keyword? (1 2)
bind/1/2: skipping ahead to (1 2)
bind/1/2: quoted a
bind/1/2: positional arg
bind/1/2: a: 1
bind/1/2: regular arg
bind/1/3: (a 'b)('b) <-> (1 2)(2)
bind/1/3: skip keyword? (2)
bind/1/3: skipping ahead to (2)
bind/1/1: => {a:1, }
- 71 test_eval_warns_on_double_quoting5
bind/1/1: ((| a 'b)) <-> (1)
bind/1/2: ((| a 'b))((| a 'b)) <-> (1)(1)
bind/1/2: skip keyword? (1)
bind/1/2: skipping ahead to (1)
bind/1/2: alias (| a 'b)
bind/1/2: positional arg
bind/1/3: quoted alias a
bind/1/3: a: 1
bind/1/3: ((| a 'b))nil <-> (1)nil
bind/1/1: => {a:1, }
- 72 test_eval_binds_missing_as_params_to_nil
bind/1/1: ((| a (b c))) <-> (1)
bind/1/2: ((| a (b c)))((| a (b c))) <-> (1)(1)
bind/1/2: skip keyword? (1)
bind/1/2: skipping ahead to (1)
bind/1/2: alias (| a (b c))
bind/1/2: positional arg
bind/1/3: alias sym a
bind/1/3: eval arg
bind/1/3: a: 1
bind/1/3: destructured alias (as-param) (b c)
bind/1/4: (b c) <-> nil
bind/1/5: (b c)(b c) <-> nilnil
bind/1/5: skip keyword? nil
bind/1/5: skipping ahead to nil
bind/1/5: quoted b
bind/1/5: positional arg
bind/1/5: b: nil
bind/1/5: regular arg
bind/1/6: (b c)(c) <-> nilnil
bind/1/6: skip keyword? nil
bind/1/6: skipping ahead to nil
bind/1/6: quoted c
bind/1/6: positional arg
bind/1/6: c: nil
bind/1/6: regular arg
bind/1/7: (b c)nil <-> nilnil
bind/1/4: => {c:nil, a:1, b:nil, }
bind/1/3: ((| a (b c)))nil <-> (1)nil
bind/1/1: => {c:nil, a:1, b:nil, }
- 73 test_eval_handles_duplicate_destructured_aliases
bind/1/1: ((a (| b x)) (c (| d x))) <-> ('(1 :x 2) '(3 :x 4))
bind/1/2: ((a (| b x)) (c (| d x)))((a (| b x)) (c (| d x))) <-> ('(1 :x 2) '(3 :x 4))('(1 :x 2) '(3 :x 4))
bind/1/2: skip keyword? ('(1 :x 2) '(3 :x 4))
bind/1/2: skipping ahead to ('(1 :x 2) '(3 :x 4))
bind/1/2: destructured (a (| b x))
bind/1/2: regular arg
bind/1/3: (a (| b x)) <-> (1 :x 2)
bind/1/4: (a (| b x))(a (| b x)) <-> (1 :x 2)(1 :x 2)
bind/1/4: skip keyword? (1 :x 2)
bind/1/4: skipping ahead to (1 :x 2)
bind/1/4: quoted a
bind/1/4: positional arg
bind/1/4: a: 1
bind/1/4: regular arg
bind/1/5: (a (| b x))((| b x)) <-> (1 :x 2)(:x 2)
bind/1/5: skip keyword? (:x 2)
bind/1/5: match? x in (a (| b x))
bind/1/5: match? x in ((| b x))
bind/1/5: match alias
bind/1/5: skipping keyword arg:x 2
bind/1/5: skip keyword? nil
bind/1/5: skipping ahead to nil
bind/1/5: alias (| b x)
bind/1/5: keyword arg
bind/1/6: quoted alias b
bind/1/6: b: 2
bind/1/6: quoted alias x
bind/1/6: x: 2
bind/1/6: (a (| b x))nil <-> (1 :x 2)nil
bind/1/3: => {b:2, a:1, x:2, }
bind/1/3: ((a (| b x)) (c (| d x)))((c (| d x))) <-> ('(1 :x 2) '(3 :x 4))('(3 :x 4))
bind/1/3: skip keyword? ('(3 :x 4))
bind/1/3: skipping ahead to ('(3 :x 4))
bind/1/3: destructured (c (| d x))
bind/1/3: regular arg
bind/1/4: (c (| d x)) <-> (3 :x 4)
bind/1/5: (c (| d x))(c (| d x)) <-> (3 :x 4)(3 :x 4)
bind/1/5: skip keyword? (3 :x 4)
bind/1/5: skipping ahead to (3 :x 4)
bind/1/5: quoted c
bind/1/5: positional arg
bind/1/5: c: 3
bind/1/5: regular arg
bind/1/6: (c (| d x))((| d x)) <-> (3 :x 4)(:x 4)
bind/1/6: skip keyword? (:x 4)
bind/1/6: match? x in (c (| d x))
bind/1/6: match? x in ((| d x))
bind/1/6: match alias
bind/1/6: skipping keyword arg:x 4
bind/1/6: skip keyword? nil
bind/1/6: skipping ahead to nil
bind/1/6: alias (| d x)
bind/1/6: keyword arg
bind/1/7: quoted alias d
bind/1/7: d: 4
bind/1/7: quoted alias x
bind/1/7: (c (| d x))nil <-> (3 :x 4)nil
bind/1/4: => {d:4, c:3, b:2, a:1, x:2, }
bind/1/4: ((a (| b x)) (c (| d x)))nil <-> ('(1 :x 2) '(3 :x 4))nil
bind/1/1: => {d:4, c:3, b:2, a:1, x:2, }
- 74 test_eval_handles_duplicate_destructured_aliases2
bind/1/1: ((a (| b x)) (c (| d x))) <-> ('(1 :x 2) '(3 :x 4))
bind/1/2: ((a (| b x)) (c (| d x)))((a (| b x)) (c (| d x))) <-> ('(1 :x 2) '(3 :x 4))('(1 :x 2) '(3 :x 4))
bind/1/2: skip keyword? ('(1 :x 2) '(3 :x 4))
bind/1/2: skipping ahead to ('(1 :x 2) '(3 :x 4))
bind/1/2: destructured (a (| b x))
bind/1/2: regular arg
bind/1/3: (a (| b x)) <-> (1 :x 2)
bind/1/4: (a (| b x))(a (| b x)) <-> (1 :x 2)(1 :x 2)
bind/1/4: skip keyword? (1 :x 2)
bind/1/4: skipping ahead to (1 :x 2)
bind/1/4: quoted a
bind/1/4: positional arg
bind/1/4: a: 1
bind/1/4: regular arg
bind/1/5: (a (| b x))((| b x)) <-> (1 :x 2)(:x 2)
bind/1/5: skip keyword? (:x 2)
bind/1/5: match? x in (a (| b x))
bind/1/5: match? x in ((| b x))
bind/1/5: match alias
bind/1/5: skipping keyword arg:x 2
bind/1/5: skip keyword? nil
bind/1/5: skipping ahead to nil
bind/1/5: alias (| b x)
bind/1/5: keyword arg
bind/1/6: quoted alias b
bind/1/6: b: 2
bind/1/6: quoted alias x
bind/1/6: x: 2
bind/1/6: (a (| b x))nil <-> (1 :x 2)nil
bind/1/3: => {x:2, b:2, a:1, }
bind/1/3: ((a (| b x)) (c (| d x)))((c (| d x))) <-> ('(1 :x 2) '(3 :x 4))('(3 :x 4))
bind/1/3: skip keyword? ('(3 :x 4))
bind/1/3: skipping ahead to ('(3 :x 4))
bind/1/3: destructured (c (| d x))
bind/1/3: regular arg
bind/1/4: (c (| d x)) <-> (3 :x 4)
bind/1/5: (c (| d x))(c (| d x)) <-> (3 :x 4)(3 :x 4)
bind/1/5: skip keyword? (3 :x 4)
bind/1/5: skipping ahead to (3 :x 4)
bind/1/5: quoted c
bind/1/5: positional arg
bind/1/5: c: 3
bind/1/5: regular arg
bind/1/6: (c (| d x))((| d x)) <-> (3 :x 4)(:x 4)
bind/1/6: skip keyword? (:x 4)
bind/1/6: match? x in (c (| d x))
bind/1/6: match? x in ((| d x))
bind/1/6: match alias
bind/1/6: skipping keyword arg:x 4
bind/1/6: skip keyword? nil
bind/1/6: skipping ahead to nil
bind/1/6: alias (| d x)
bind/1/6: keyword arg
bind/1/7: quoted alias d
bind/1/7: d: 4
bind/1/7: quoted alias x
bind/1/7: (c (| d x))nil <-> (3 :x 4)nil
bind/1/4: => {x:2, d:4, c:3, b:2, a:1, }
bind/1/4: ((a (| b x)) (c (| d x)))nil <-> ('(1 :x 2) '(3 :x 4))nil
bind/1/1: => {x:2, d:4, c:3, b:2, a:1, }
- 75 test_eval_handles_already_evald_aliased_arg
bind/1/1: ((| x y)) <-> (''a)
bind/1/2: ((| x y))((| x y)) <-> (''a)(''a)
bind/1/2: skip keyword? (''a)
bind/1/2: skipping ahead to (''a)
bind/1/2: alias (| x y)
bind/1/2: positional arg
bind/1/3: alias sym x
bind/1/3: eval arg
bind/1/3: x: a
bind/1/3: alias sym y
bind/1/3: y: a
bind/1/3: ((| x y))nil <-> (''a)nil
bind/1/1: => {x:a, y:a, }
- 77 test_fn_evals_arg_only_when_necessary
bind/1/1: (| 'a (| 'b 'c)) <-> (x)
bind/1/2: (| 'a (| 'b 'c))(| 'a (| 'b 'c)) <-> (x)(x)
bind/1/2: skip keyword? (x)
bind/1/2: skipping ahead to (x)
bind/1/2: rest alias (| 'a (| 'b 'c))
bind/1/2: quoted rest alias 'a
bind/1/2: a: (x)
bind/1/2: multiple rest aliases (| 'b 'c)
bind/1/2: rest alias cons (as-param) (| 'b 'c)
bind/1/3: (| 'b 'c) <-> (x)
bind/1/4: (| 'b 'c)(| 'b 'c) <-> (x)(x)
bind/1/4: skip keyword? (x)
bind/1/4: skipping ahead to (x)
bind/1/4: rest alias (| 'b 'c)
bind/1/4: quoted rest alias 'b
bind/1/4: b: (x)
bind/1/4: quoted rest alias 'c
bind/1/4: c: (x)
bind/1/3: => {b:(x), a:(x), c:(x), }
bind/1/1: => {b:(x), a:(x), c:(x), }
- 78 test_fn_evals_arg_only_when_necessary2
bind/1/1: (| 'a ('b c)) <-> (x y)
bind/1/2: (| 'a ('b c))(| 'a ('b c)) <-> (x y)(x y)
bind/1/2: skip keyword? (x y)
bind/1/2: skipping ahead to (x y)
bind/1/2: rest alias (| 'a ('b c))
bind/1/2: quoted rest alias 'a
bind/1/2: a: (x y)
bind/1/2: multiple rest aliases ('b c)
bind/1/2: rest alias cons (as-param) ('b c)
bind/1/3: ('b c) <-> (x y)
bind/1/4: ('b c)('b c) <-> (x y)(x y)
bind/1/4: skip keyword? (x y)
bind/1/4: skipping ahead to (x y)
bind/1/4: quoted b
bind/1/4: positional arg
bind/1/4: b: x
bind/1/4: regular arg
bind/1/5: ('b c)(c) <-> (x y)(y)
bind/1/5: skip keyword? (y)
bind/1/5: skipping ahead to (y)
bind/1/5: regular c
bind/1/5: after eval 3
bind/1/5: c: 3
bind/1/6: ('b c)nil <-> (x y)nil
bind/1/3: => {a:(x y), c:3, b:x, }
bind/1/1: => {a:(x y), c:3, b:x, }
- 79 test_fn_evals_destructured_arg_only_when_necessary
bind/1/1: ((| 'a 'b)) <-> (x)
bind/1/2: ((| 'a 'b))((| 'a 'b)) <-> (x)(x)
bind/1/2: skip keyword? (x)
bind/1/2: skipping ahead to (x)
bind/1/2: alias (| 'a 'b)
bind/1/2: positional arg
bind/1/3: quoted alias 'a
bind/1/3: a: x
bind/1/3: quoted alias 'b
bind/1/3: b: x
bind/1/3: ((| 'a 'b))nil <-> (x)nil
bind/1/1: => {a:x, b:x, }
- 80 test_fn_evals_destructured_arg_only_when_necessary2
bind/1/1: ((| 'a (| 'b 'c))) <-> (x)
bind/1/2: ((| 'a (| 'b 'c)))((| 'a (| 'b 'c))) <-> (x)(x)
bind/1/2: skip keyword? (x)
bind/1/2: skipping ahead to (x)
bind/1/2: alias (| 'a (| 'b 'c))
bind/1/2: positional arg
bind/1/3: quoted alias 'a
bind/1/3: a: x
bind/1/3: nested alias (as-param) (| 'b 'c)
bind/1/4: quoted alias 'b
bind/1/4: b: x
bind/1/4: quoted alias 'c
bind/1/4: c: x
bind/1/3: ((| 'a (| 'b 'c)))nil <-> (x)nil
bind/1/1: => {c:x, b:x, a:x, }
- 81 test_fn_evals_destructured_arg_only_when_necessary3
bind/1/1: ((| 'a (| 'b c))) <-> (x)
bind/1/2: ((| 'a (| 'b c)))((| 'a (| 'b c))) <-> (x)(x)
bind/1/2: skip keyword? (x)
bind/1/2: skipping ahead to (x)
bind/1/2: alias (| 'a (| 'b c))
bind/1/2: positional arg
bind/1/3: quoted alias 'a
bind/1/3: a: x
bind/1/3: nested alias (as-param) (| 'b c)
bind/1/4: quoted alias 'b
bind/1/4: b: x
bind/1/4: alias sym c
bind/1/4: eval arg
bind/1/4: c: 3
bind/1/3: ((| 'a (| 'b c)))nil <-> (x)nil
bind/1/1: => {c:3, b:x, a:x, }
