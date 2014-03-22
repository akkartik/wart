- 0 test_eval_evals_arg
bind/0: ($params ... $body) <-> ((a))
bind/0: ($params ... $body)($params ... $body) <-> ((a))((a))
bind/0: skip keyword? ((a))
bind/0: skipping ahead to ((a))
bind/0: quoted $params
bind/0: positional arg
bind/0: $params: (a)
bind/0: regular arg
bind/0: ($params ... $body)$body <-> ((a))nil
bind/0: skip keyword? nil
bind/0: skipping ahead to nil
bind/0: quoted rest $body
bind/0: $body: nil
bind/0: => {$params:(a), $body:nil, }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {$params:(a), $body:nil, }
bind/0: caller_scope: nil
bind/0: (a) <-> (x)
bind/0: (a)(a) <-> (x)(x)
bind/0: skip keyword? (x)
bind/0: skipping ahead to (x)
bind/0: regular a
bind/0: after eval 3
bind/0: a: 3
bind/0: (a)nil <-> (x)nil
bind/0: => {a:3, }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {a:3, }
bind/0: caller_scope: nil
- 1 test_eval_evals_arg2
bind/0: ($params ... $body) <-> ((f) (f))
bind/0: ($params ... $body)($params ... $body) <-> ((f) (f))((f) (f))
bind/0: skip keyword? ((f) (f))
bind/0: skipping ahead to ((f) (f))
bind/0: quoted $params
bind/0: positional arg
bind/0: $params: (f)
bind/0: regular arg
bind/0: ($params ... $body)$body <-> ((f) (f))((f))
bind/0: skip keyword? ((f))
bind/0: skipping ahead to ((f))
bind/0: quoted rest $body
bind/0: $body: ((f))
bind/0: => {$params:(f), $body:((f)), }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {$params:(f), $body:((f)), }
bind/0: caller_scope: nil
bind/0: (f) <-> ((fn nil 34))
bind/0: (f)(f) <-> ((fn nil 34))((fn nil 34))
bind/0: skip keyword? ((fn nil 34))
bind/0: skipping ahead to ((fn nil 34))
bind/0: regular f
bind/0: ($params ... $body) <-> (nil 34)
bind/0: ($params ... $body)($params ... $body) <-> (nil 34)(nil 34)
bind/0: skip keyword? (nil 34)
bind/0: skipping ahead to (nil 34)
bind/0: quoted $params
bind/0: positional arg
bind/0: $params: nil
bind/0: regular arg
bind/0: ($params ... $body)$body <-> (nil 34)(34)
bind/0: skip keyword? (34)
bind/0: skipping ahead to (34)
bind/0: quoted rest $body
bind/0: $body: (34)
bind/0: => {$params:nil, $body:(34), }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {$params:nil, $body:(34), }
bind/0: caller_scope: nil
bind/0: after eval (object function {body:(34), })
bind/0: f: (object function {body:(34), })
bind/0: (f)nil <-> ((fn nil 34))nil
bind/0: => {f:(object function {body:(34), }), }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {f:(object function {body:(34), }), }
bind/0: caller_scope: nil
bind/0: nil <-> nil
bind/0: nilnil <-> nilnil
bind/0: => {}
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {}
bind/0: caller_scope: {caller_scope:nil, f:(object function {body:(34), }), }
- 2 test_eval_handles_multiple_args
bind/0: ($params ... $body) <-> ((a b) b)
bind/0: ($params ... $body)($params ... $body) <-> ((a b) b)((a b) b)
bind/0: skip keyword? ((a b) b)
bind/0: skipping ahead to ((a b) b)
bind/0: quoted $params
bind/0: positional arg
bind/0: $params: (a b)
bind/0: regular arg
bind/0: ($params ... $body)$body <-> ((a b) b)(b)
bind/0: skip keyword? (b)
bind/0: skipping ahead to (b)
bind/0: quoted rest $body
bind/0: $body: (b)
bind/0: => {$params:(a b), $body:(b), }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {$params:(a b), $body:(b), }
bind/0: caller_scope: nil
bind/0: (a b) <-> (1 2)
bind/0: (a b)(a b) <-> (1 2)(1 2)
bind/0: skip keyword? (1 2)
bind/0: skipping ahead to (1 2)
bind/0: regular a
bind/0: after eval 1
bind/0: a: 1
bind/0: (a b)(b) <-> (1 2)(2)
bind/0: skip keyword? (2)
bind/0: skipping ahead to (2)
bind/0: regular b
bind/0: after eval 2
bind/0: b: 2
bind/0: (a b)nil <-> (1 2)nil
bind/0: => {a:1, b:2, }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {a:1, b:2, }
bind/0: caller_scope: nil
- 3 test_eval_binds_missing_params
bind/0: ($params ... $body) <-> ((a b))
bind/0: ($params ... $body)($params ... $body) <-> ((a b))((a b))
bind/0: skip keyword? ((a b))
bind/0: skipping ahead to ((a b))
bind/0: quoted $params
bind/0: positional arg
bind/0: $params: (a b)
bind/0: regular arg
bind/0: ($params ... $body)$body <-> ((a b))nil
bind/0: skip keyword? nil
bind/0: skipping ahead to nil
bind/0: quoted rest $body
bind/0: $body: nil
bind/0: => {$params:(a b), $body:nil, }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {$params:(a b), $body:nil, }
bind/0: caller_scope: nil
bind/0: (a b) <-> (x)
bind/0: (a b)(a b) <-> (x)(x)
bind/0: skip keyword? (x)
bind/0: skipping ahead to (x)
bind/0: regular a
bind/0: after eval 3
bind/0: a: 3
bind/0: (a b)(b) <-> (x)nil
bind/0: skip keyword? nil
bind/0: skipping ahead to nil
bind/0: regular b
bind/0: after eval nil
bind/0: b: nil
bind/0: (a b)nil <-> (x)nil
bind/0: => {a:3, b:nil, }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {a:3, b:nil, }
bind/0: caller_scope: nil
- 4 test_eval_binds_quoted_param
bind/0: ($params ... $body) <-> (('a))
bind/0: ($params ... $body)($params ... $body) <-> (('a))(('a))
bind/0: skip keyword? (('a))
bind/0: skipping ahead to (('a))
bind/0: quoted $params
bind/0: positional arg
bind/0: $params: ('a)
bind/0: regular arg
bind/0: ($params ... $body)$body <-> (('a))nil
bind/0: skip keyword? nil
bind/0: skipping ahead to nil
bind/0: quoted rest $body
bind/0: $body: nil
bind/0: => {$params:('a), $body:nil, }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {$params:('a), $body:nil, }
bind/0: caller_scope: nil
bind/0: ('a) <-> (x)
bind/0: ('a)('a) <-> (x)(x)
bind/0: skip keyword? (x)
bind/0: skipping ahead to (x)
bind/0: quoted a
bind/0: positional arg
bind/0: a: x
bind/0: regular arg
bind/0: ('a)nil <-> (x)nil
bind/0: => {a:x, }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {a:x, }
bind/0: caller_scope: nil
- 5 test_eval_handles_quoted_param_list
bind/0: a: 23
bind/0: ($params ... $body) <-> ('(arg1) arg1)
bind/0: ($params ... $body)($params ... $body) <-> ('(arg1) arg1)('(arg1) arg1)
bind/0: skip keyword? ('(arg1) arg1)
bind/0: skipping ahead to ('(arg1) arg1)
bind/0: quoted $params
bind/0: positional arg
bind/0: $params: '(arg1)
bind/0: regular arg
bind/0: ($params ... $body)$body <-> ('(arg1) arg1)(arg1)
bind/0: skip keyword? (arg1)
bind/0: skipping ahead to (arg1)
bind/0: quoted rest $body
bind/0: $body: (arg1)
bind/0: => {$params:'(arg1), $body:(arg1), }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {$params:'(arg1), $body:(arg1), }
bind/0: caller_scope: nil
bind/0: (arg1) <-> (a)
bind/0: (arg1)(arg1) <-> (a)(a)
bind/0: skip keyword? (a)
bind/0: skipping ahead to (a)
bind/0: quoted arg1
bind/0: positional arg
bind/0: arg1: a
bind/0: regular arg
bind/0: (arg1)nil <-> (a)nil
bind/0: => {arg1:a, }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {arg1:a, }
bind/0: caller_scope: nil
- 6 test_eval_handles_vararg_param
bind/0: ($params ... $body) <-> (args args)
bind/0: ($params ... $body)($params ... $body) <-> (args args)(args args)
bind/0: skip keyword? (args args)
bind/0: skipping ahead to (args args)
bind/0: quoted $params
bind/0: positional arg
bind/0: $params: args
bind/0: regular arg
bind/0: ($params ... $body)$body <-> (args args)(args)
bind/0: skip keyword? (args)
bind/0: skipping ahead to (args)
bind/0: quoted rest $body
bind/0: $body: (args)
bind/0: => {$params:args, $body:(args), }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {$params:args, $body:(args), }
bind/0: caller_scope: nil
bind/0: args <-> (1)
bind/0: argsargs <-> (1)(1)
bind/0: skip keyword? (1)
bind/0: skipping ahead to (1)
bind/0: rest args
bind/0: args: (1)
bind/0: => {args:(1), }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {args:(1), }
bind/0: caller_scope: nil
- 7 test_eval_evals_vararg_args
bind/0: x: 3
bind/0: ($params ... $body) <-> (args)
bind/0: ($params ... $body)($params ... $body) <-> (args)(args)
bind/0: skip keyword? (args)
bind/0: skipping ahead to (args)
bind/0: quoted $params
bind/0: positional arg
bind/0: $params: args
bind/0: regular arg
bind/0: ($params ... $body)$body <-> (args)nil
bind/0: skip keyword? nil
bind/0: skipping ahead to nil
bind/0: quoted rest $body
bind/0: $body: nil
bind/0: => {$params:args, $body:nil, }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {$params:args, $body:nil, }
bind/0: caller_scope: nil
bind/0: args <-> (x y)
bind/0: argsargs <-> (x y)(x y)
bind/0: skip keyword? (x y)
bind/0: skipping ahead to (x y)
bind/0: rest args
bind/0: args: (3 4)
bind/0: => {args:(3 4), }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {args:(3 4), }
bind/0: caller_scope: nil
- 8 test_eval_binds_quoted_varargs_param
bind/0: x: 3
bind/0: ($params ... $body) <-> ('args)
bind/0: ($params ... $body)($params ... $body) <-> ('args)('args)
bind/0: skip keyword? ('args)
bind/0: skipping ahead to ('args)
bind/0: quoted $params
bind/0: positional arg
bind/0: $params: 'args
bind/0: regular arg
bind/0: ($params ... $body)$body <-> ('args)nil
bind/0: skip keyword? nil
bind/0: skipping ahead to nil
bind/0: quoted rest $body
bind/0: $body: nil
bind/0: => {$params:'args, $body:nil, }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {$params:'args, $body:nil, }
bind/0: caller_scope: nil
bind/0: args <-> (x y)
bind/0: argsargs <-> (x y)(x y)
bind/0: skip keyword? (x y)
bind/0: skipping ahead to (x y)
bind/0: quoted rest args
bind/0: args: (x y)
bind/0: => {args:(x y), }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {args:(x y), }
bind/0: caller_scope: nil
- 9 test_eval_handles_rest_params
bind/0: ($params ... $body) <-> ((a b ... c) c)
bind/0: ($params ... $body)($params ... $body) <-> ((a b ... c) c)((a b ... c) c)
bind/0: skip keyword? ((a b ... c) c)
bind/0: skipping ahead to ((a b ... c) c)
bind/0: quoted $params
bind/0: positional arg
bind/0: $params: (a b ... c)
bind/0: regular arg
bind/0: ($params ... $body)$body <-> ((a b ... c) c)(c)
bind/0: skip keyword? (c)
bind/0: skipping ahead to (c)
bind/0: quoted rest $body
bind/0: $body: (c)
bind/0: => {$params:(a b ... c), $body:(c), }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {$params:(a b ... c), $body:(c), }
bind/0: caller_scope: nil
bind/0: (a b ... c) <-> (1 2 3 4 5)
bind/0: (a b ... c)(a b ... c) <-> (1 2 3 4 5)(1 2 3 4 5)
bind/0: skip keyword? (1 2 3 4 5)
bind/0: skipping ahead to (1 2 3 4 5)
bind/0: regular a
bind/0: after eval 1
bind/0: a: 1
bind/0: (a b ... c)(b ... c) <-> (1 2 3 4 5)(2 3 4 5)
bind/0: skip keyword? (2 3 4 5)
bind/0: skipping ahead to (2 3 4 5)
bind/0: regular b
bind/0: after eval 2
bind/0: b: 2
bind/0: (a b ... c)c <-> (1 2 3 4 5)(3 4 5)
bind/0: skip keyword? (3 4 5)
bind/0: skipping ahead to (3 4 5)
bind/0: rest c
bind/0: c: (3 4 5)
bind/0: => {a:1, b:2, c:(3 4 5), }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {a:1, b:2, c:(3 4 5), }
bind/0: caller_scope: nil
- 10 test_eval_evals_rest_args
bind/0: x: 3
bind/0: ($params ... $body) <-> ((a ... b))
bind/0: ($params ... $body)($params ... $body) <-> ((a ... b))((a ... b))
bind/0: skip keyword? ((a ... b))
bind/0: skipping ahead to ((a ... b))
bind/0: quoted $params
bind/0: positional arg
bind/0: $params: (a ... b)
bind/0: regular arg
bind/0: ($params ... $body)$body <-> ((a ... b))nil
bind/0: skip keyword? nil
bind/0: skipping ahead to nil
bind/0: quoted rest $body
bind/0: $body: nil
bind/0: => {$params:(a ... b), $body:nil, }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {$params:(a ... b), $body:nil, }
bind/0: caller_scope: nil
bind/0: (a ... b) <-> (x y)
bind/0: (a ... b)(a ... b) <-> (x y)(x y)
bind/0: skip keyword? (x y)
bind/0: skipping ahead to (x y)
bind/0: regular a
bind/0: after eval 3
bind/0: a: 3
bind/0: (a ... b)b <-> (x y)(y)
bind/0: skip keyword? (y)
bind/0: skipping ahead to (y)
bind/0: rest b
bind/0: b: (4)
bind/0: => {a:3, b:(4), }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {a:3, b:(4), }
bind/0: caller_scope: nil
- 11 test_eval_binds_quoted_rest_param
bind/0: x: 3
bind/0: ($params ... $body) <-> ((a ' ... b))
bind/0: ($params ... $body)($params ... $body) <-> ((a ' ... b))((a ' ... b))
bind/0: skip keyword? ((a ' ... b))
bind/0: skipping ahead to ((a ' ... b))
bind/0: quoted $params
bind/0: positional arg
bind/0: $params: (a ' ... b)
bind/0: regular arg
bind/0: ($params ... $body)$body <-> ((a ' ... b))nil
bind/0: skip keyword? nil
bind/0: skipping ahead to nil
bind/0: quoted rest $body
bind/0: $body: nil
bind/0: => {$params:(a ' ... b), $body:nil, }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {$params:(a ' ... b), $body:nil, }
bind/0: caller_scope: nil
bind/0: (a ' ... b) <-> (x y)
bind/0: (a ' ... b)(a ' ... b) <-> (x y)(x y)
bind/0: skip keyword? (x y)
bind/0: skipping ahead to (x y)
bind/0: regular a
bind/0: after eval 3
bind/0: a: 3
bind/0: (a ' ... b)'b <-> (x y)(y)
bind/0: skip keyword? (y)
bind/0: skipping ahead to (y)
bind/0: quoted rest b
bind/0: b: (y)
bind/0: => {b:(y), a:3, }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {b:(y), a:3, }
bind/0: caller_scope: nil
- 12 test_eval_handles_destructured_params
bind/0: ($params ... $body) <-> (((a b)) b)
bind/0: ($params ... $body)($params ... $body) <-> (((a b)) b)(((a b)) b)
bind/0: skip keyword? (((a b)) b)
bind/0: skipping ahead to (((a b)) b)
bind/0: quoted $params
bind/0: positional arg
bind/0: $params: ((a b))
bind/0: regular arg
bind/0: ($params ... $body)$body <-> (((a b)) b)(b)
bind/0: skip keyword? (b)
bind/0: skipping ahead to (b)
bind/0: quoted rest $body
bind/0: $body: (b)
bind/0: => {$params:((a b)), $body:(b), }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {$params:((a b)), $body:(b), }
bind/0: caller_scope: nil
bind/0: ((a b)) <-> ('(1 2))
bind/0: ((a b))((a b)) <-> ('(1 2))('(1 2))
bind/0: skip keyword? ('(1 2))
bind/0: skipping ahead to ('(1 2))
bind/0: destructured (a b)
bind/0: regular arg
bind/0: (a b) <-> (1 2)
bind/0: (a b)(a b) <-> (1 2)(1 2)
bind/0: skip keyword? (1 2)
bind/0: skipping ahead to (1 2)
bind/0: quoted a
bind/0: positional arg
bind/0: a: 1
bind/0: regular arg
bind/0: (a b)(b) <-> (1 2)(2)
bind/0: skip keyword? (2)
bind/0: skipping ahead to (2)
bind/0: quoted b
bind/0: positional arg
bind/0: b: 2
bind/0: regular arg
bind/0: (a b)nil <-> (1 2)nil
bind/0: => {b:2, a:1, }
bind/0: ((a b))nil <-> ('(1 2))nil
bind/0: => {b:2, a:1, }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {b:2, a:1, }
bind/0: caller_scope: nil
- 13 test_eval_evals_destructured_args
bind/0: x: 3
bind/0: ($params ... $body) <-> (((a b)))
bind/0: ($params ... $body)($params ... $body) <-> (((a b)))(((a b)))
bind/0: skip keyword? (((a b)))
bind/0: skipping ahead to (((a b)))
bind/0: quoted $params
bind/0: positional arg
bind/0: $params: ((a b))
bind/0: regular arg
bind/0: ($params ... $body)$body <-> (((a b)))nil
bind/0: skip keyword? nil
bind/0: skipping ahead to nil
bind/0: quoted rest $body
bind/0: $body: nil
bind/0: => {$params:((a b)), $body:nil, }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {$params:((a b)), $body:nil, }
bind/0: caller_scope: nil
bind/0: ((a b)) <-> (`(,x ,y))
bind/0: ((a b))((a b)) <-> (`(,x ,y))(`(,x ,y))
bind/0: skip keyword? (`(,x ,y))
bind/0: skipping ahead to (`(,x ,y))
bind/0: destructured (a b)
bind/0: regular arg
bind/0: (a b) <-> (3 4)
bind/0: (a b)(a b) <-> (3 4)(3 4)
bind/0: skip keyword? (3 4)
bind/0: skipping ahead to (3 4)
bind/0: quoted a
bind/0: positional arg
bind/0: a: 3
bind/0: regular arg
bind/0: (a b)(b) <-> (3 4)(4)
bind/0: skip keyword? (4)
bind/0: skipping ahead to (4)
bind/0: quoted b
bind/0: positional arg
bind/0: b: 4
bind/0: regular arg
bind/0: (a b)nil <-> (3 4)nil
bind/0: => {a:3, b:4, }
bind/0: ((a b))nil <-> (`(,x ,y))nil
bind/0: => {a:3, b:4, }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {a:3, b:4, }
bind/0: caller_scope: nil
- 14 test_eval_handles_quoted_destructured_params
bind/0: ($params ... $body) <-> (('(a b)) b)
bind/0: ($params ... $body)($params ... $body) <-> (('(a b)) b)(('(a b)) b)
bind/0: skip keyword? (('(a b)) b)
bind/0: skipping ahead to (('(a b)) b)
bind/0: quoted $params
bind/0: positional arg
bind/0: $params: ('(a b))
bind/0: regular arg
bind/0: ($params ... $body)$body <-> (('(a b)) b)(b)
bind/0: skip keyword? (b)
bind/0: skipping ahead to (b)
bind/0: quoted rest $body
bind/0: $body: (b)
bind/0: => {$params:('(a b)), $body:(b), }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {$params:('(a b)), $body:(b), }
bind/0: caller_scope: nil
bind/0: ('(a b)) <-> ((1 2))
bind/0: ('(a b))('(a b)) <-> ((1 2))((1 2))
bind/0: skip keyword? ((1 2))
bind/0: skipping ahead to ((1 2))
bind/0: quoted destructured (a b)
bind/0: (a b) <-> (1 2)
bind/0: (a b)(a b) <-> (1 2)(1 2)
bind/0: skip keyword? (1 2)
bind/0: skipping ahead to (1 2)
bind/0: quoted a
bind/0: positional arg
bind/0: a: 1
bind/0: regular arg
bind/0: (a b)(b) <-> (1 2)(2)
bind/0: skip keyword? (2)
bind/0: skipping ahead to (2)
bind/0: quoted b
bind/0: positional arg
bind/0: b: 2
bind/0: regular arg
bind/0: (a b)nil <-> (1 2)nil
bind/0: => {a:1, b:2, }
bind/0: ('(a b))nil <-> ((1 2))nil
bind/0: => {a:1, b:2, }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {a:1, b:2, }
bind/0: caller_scope: nil
- 15 test_eval_handles_improper_list_in_destructured_arg
bind/0: ($params ... $body) <-> ('((a ... b)))
bind/0: ($params ... $body)($params ... $body) <-> ('((a ... b)))('((a ... b)))
bind/0: skip keyword? ('((a ... b)))
bind/0: skipping ahead to ('((a ... b)))
bind/0: quoted $params
bind/0: positional arg
bind/0: $params: '((a ... b))
bind/0: regular arg
bind/0: ($params ... $body)$body <-> ('((a ... b)))nil
bind/0: skip keyword? nil
bind/0: skipping ahead to nil
bind/0: quoted rest $body
bind/0: $body: nil
bind/0: => {$params:'((a ... b)), $body:nil, }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {$params:'((a ... b)), $body:nil, }
bind/0: caller_scope: nil
bind/0: ((a ... b)) <-> ((x ... y))
bind/0: ((a ... b))((a ... b)) <-> ((x ... y))((x ... y))
bind/0: skip keyword? ((x ... y))
bind/0: skipping ahead to ((x ... y))
bind/0: quoted destructured (a ... b)
bind/0: (a ... b) <-> (x ... y)
bind/0: (a ... b)(a ... b) <-> (x ... y)(x ... y)
bind/0: skip keyword? (x ... y)
bind/0: skipping ahead to (x ... y)
bind/0: quoted a
bind/0: positional arg
bind/0: a: x
bind/0: non-cons arg
bind/0: b: y
bind/0: => {a:x, b:y, }
bind/0: ((a ... b))nil <-> ((x ... y))nil
bind/0: => {a:x, b:y, }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {a:x, b:y, }
bind/0: caller_scope: nil
- 16 test_eval_splices_args
bind/0: b: (3 4)
bind/0: ($x $y) <-> (''3 ''4)
bind/0: ($x $y)($x $y) <-> (''3 ''4)(''3 ''4)
bind/0: skip keyword? (''3 ''4)
bind/0: skipping ahead to (''3 ''4)
bind/0: regular $x
bind/0: after eval 3
bind/0: $x: 3
bind/0: ($x $y)($y) <-> (''3 ''4)(''4)
bind/0: skip keyword? (''4)
bind/0: skipping ahead to (''4)
bind/0: regular $y
bind/0: after eval 4
bind/0: $y: 4
bind/0: ($x $y)nil <-> (''3 ''4)nil
bind/0: => {$x:3, $y:4, }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {$x:3, $y:4, }
bind/0: caller_scope: nil
- 17 test_eval_splices_args2
bind/0: a: 3
bind/0: b: (4 5)
bind/0: ($params ... $body) <-> (nil 3)
bind/0: ($params ... $body)($params ... $body) <-> (nil 3)(nil 3)
bind/0: skip keyword? (nil 3)
bind/0: skipping ahead to (nil 3)
bind/0: quoted $params
bind/0: positional arg
bind/0: $params: nil
bind/0: regular arg
bind/0: ($params ... $body)$body <-> (nil 3)(3)
bind/0: skip keyword? (3)
bind/0: skipping ahead to (3)
bind/0: quoted rest $body
bind/0: $body: (3)
bind/0: => {$params:nil, $body:(3), }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {$params:nil, $body:(3), }
bind/0: caller_scope: nil
bind/0: nil <-> (a ''4 ''5 a)
bind/0: nilnil <-> (a ''4 ''5 a)(a ''4 ''5 a)
bind/0: => {}
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {}
bind/0: caller_scope: nil
- 18 test_eval_splices_args3
bind/0: ('$var $val) <-> (f (fn (x y) (cons x y)))
bind/0: ('$var $val)('$var $val) <-> (f (fn (x y) (cons x y)))(f (fn (x y) (cons x y)))
bind/0: skip keyword? (f (fn (x y) (cons x y)))
bind/0: skipping ahead to (f (fn (x y) (cons x y)))
bind/0: quoted $var
bind/0: positional arg
bind/0: $var: f
bind/0: regular arg
bind/0: ('$var $val)($val) <-> (f (fn (x y) (cons x y)))((fn (x y) (cons x y)))
bind/0: skip keyword? ((fn (x y) (cons x y)))
bind/0: skipping ahead to ((fn (x y) (cons x y)))
bind/0: regular $val
bind/0: ($params ... $body) <-> ((x y) (cons x y))
bind/0: ($params ... $body)($params ... $body) <-> ((x y) (cons x y))((x y) (cons x y))
bind/0: skip keyword? ((x y) (cons x y))
bind/0: skipping ahead to ((x y) (cons x y))
bind/0: quoted $params
bind/0: positional arg
bind/0: $params: (x y)
bind/0: regular arg
bind/0: ($params ... $body)$body <-> ((x y) (cons x y))((cons x y))
bind/0: skip keyword? ((cons x y))
bind/0: skipping ahead to ((cons x y))
bind/0: quoted rest $body
bind/0: $body: ((cons x y))
bind/0: => {$params:(x y), $body:((cons x y)), }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {$params:(x y), $body:((cons x y)), }
bind/0: caller_scope: nil
bind/0: after eval (object function {sig:(x y), body:((cons x y)), })
bind/0: $val: (object function {sig:(x y), body:((cons x y)), })
bind/0: ('$var $val)nil <-> (f (fn (x y) (cons x y)))nil
bind/0: => {$val:(object function {sig:(x y), body:((cons x y)), }), $var:f, }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {$val:(object function {sig:(x y), body:((cons x y)), }), $var:f, }
bind/0: caller_scope: nil
bind/0: f: (object function {sig:(x y), body:((cons x y)), })
bind/0: ('$var $val) <-> (a 3)
bind/0: ('$var $val)('$var $val) <-> (a 3)(a 3)
bind/0: skip keyword? (a 3)
bind/0: skipping ahead to (a 3)
bind/0: quoted $var
bind/0: positional arg
bind/0: $var: a
bind/0: regular arg
bind/0: ('$var $val)($val) <-> (a 3)(3)
bind/0: skip keyword? (3)
bind/0: skipping ahead to (3)
bind/0: regular $val
bind/0: after eval 3
bind/0: $val: 3
bind/0: ('$var $val)nil <-> (a 3)nil
bind/0: => {$val:3, $var:a, }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {$val:3, $var:a, }
bind/0: caller_scope: nil
bind/0: a: 3
bind/0: ('$var $val) <-> (b 4)
bind/0: ('$var $val)('$var $val) <-> (b 4)(b 4)
bind/0: skip keyword? (b 4)
bind/0: skipping ahead to (b 4)
bind/0: quoted $var
bind/0: positional arg
bind/0: $var: b
bind/0: regular arg
bind/0: ('$var $val)($val) <-> (b 4)(4)
bind/0: skip keyword? (4)
bind/0: skipping ahead to (4)
bind/0: regular $val
bind/0: after eval 4
bind/0: $val: 4
bind/0: ('$var $val)nil <-> (b 4)nil
bind/0: => {$val:4, $var:b, }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {$val:4, $var:b, }
bind/0: caller_scope: nil
bind/0: b: 4
bind/0: ('$var $val) <-> (args '(a b))
bind/0: ('$var $val)('$var $val) <-> (args '(a b))(args '(a b))
bind/0: skip keyword? (args '(a b))
bind/0: skipping ahead to (args '(a b))
bind/0: quoted $var
bind/0: positional arg
bind/0: $var: args
bind/0: regular arg
bind/0: ('$var $val)($val) <-> (args '(a b))('(a b))
bind/0: skip keyword? ('(a b))
bind/0: skipping ahead to ('(a b))
bind/0: regular $val
bind/0: after eval (a b)
bind/0: $val: (a b)
bind/0: ('$var $val)nil <-> (args '(a b))nil
bind/0: => {$val:(a b), $var:args, }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {$val:(a b), $var:args, }
bind/0: caller_scope: nil
bind/0: args: (a b)
bind/0: (x y) <-> (''a ''b)
bind/0: (x y)(x y) <-> (''a ''b)(''a ''b)
bind/0: skip keyword? (''a ''b)
bind/0: skipping ahead to (''a ''b)
bind/0: regular x
bind/0: after eval a
bind/0: x: a
bind/0: (x y)(y) <-> (''a ''b)(''b)
bind/0: skip keyword? (''b)
bind/0: skipping ahead to (''b)
bind/0: regular y
bind/0: after eval b
bind/0: y: b
bind/0: (x y)nil <-> (''a ''b)nil
bind/0: => {y:b, x:a, }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {y:b, x:a, }
bind/0: caller_scope: nil
bind/0: ($x $y) <-> (x y)
bind/0: ($x $y)($x $y) <-> (x y)(x y)
bind/0: skip keyword? (x y)
bind/0: skipping ahead to (x y)
bind/0: regular $x
bind/0: after eval a
bind/0: $x: a
bind/0: ($x $y)($y) <-> (x y)(y)
bind/0: skip keyword? (y)
bind/0: skipping ahead to (y)
bind/0: regular $y
bind/0: after eval b
bind/0: $y: b
bind/0: ($x $y)nil <-> (x y)nil
bind/0: => {$y:b, $x:a, }
bind/0: Curr_lexical_scope: {y:b, caller_scope:nil, x:a, }
bind/0: Curr_lexical_scope: {$y:b, $x:a, }->{y:b, caller_scope:nil, x:a, }
bind/0: caller_scope: {y:b, caller_scope:nil, x:a, }
- 19 test_eval_splices_nil_args
bind/0: a: 3
bind/0: b: nil
bind/0: ($params ... $body) <-> (nil 3)
bind/0: ($params ... $body)($params ... $body) <-> (nil 3)(nil 3)
bind/0: skip keyword? (nil 3)
bind/0: skipping ahead to (nil 3)
bind/0: quoted $params
bind/0: positional arg
bind/0: $params: nil
bind/0: regular arg
bind/0: ($params ... $body)$body <-> (nil 3)(3)
bind/0: skip keyword? (3)
bind/0: skipping ahead to (3)
bind/0: quoted rest $body
bind/0: $body: (3)
bind/0: => {$params:nil, $body:(3), }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {$params:nil, $body:(3), }
bind/0: caller_scope: nil
bind/0: nil <-> (a a)
bind/0: nilnil <-> (a a)(a a)
bind/0: => {}
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {}
bind/0: caller_scope: nil
- 20 test_eval_splices_keyword_syms_into_args
bind/0: a: 3
bind/0: b: (4 :x)
bind/0: ($params ... $body) <-> (nil 3)
bind/0: ($params ... $body)($params ... $body) <-> (nil 3)(nil 3)
bind/0: skip keyword? (nil 3)
bind/0: skipping ahead to (nil 3)
bind/0: quoted $params
bind/0: positional arg
bind/0: $params: nil
bind/0: regular arg
bind/0: ($params ... $body)$body <-> (nil 3)(3)
bind/0: skip keyword? (3)
bind/0: skipping ahead to (3)
bind/0: quoted rest $body
bind/0: $body: (3)
bind/0: => {$params:nil, $body:(3), }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {$params:nil, $body:(3), }
bind/0: caller_scope: nil
bind/0: nil <-> (a ''4 :x a)
bind/0: nilnil <-> (a ''4 :x a)(a ''4 :x a)
bind/0: => {}
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {}
bind/0: caller_scope: nil
- 21 test_eval_handles_splice_inside_fn_body
bind/0: ($params ... $body) <-> (x (cons @x))
bind/0: ($params ... $body)($params ... $body) <-> (x (cons @x))(x (cons @x))
bind/0: skip keyword? (x (cons @x))
bind/0: skipping ahead to (x (cons @x))
bind/0: quoted $params
bind/0: positional arg
bind/0: $params: x
bind/0: regular arg
bind/0: ($params ... $body)$body <-> (x (cons @x))((cons @x))
bind/0: skip keyword? ((cons @x))
bind/0: skipping ahead to ((cons @x))
bind/0: quoted rest $body
bind/0: $body: ((cons @x))
bind/0: => {$params:x, $body:((cons @x)), }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {$params:x, $body:((cons @x)), }
bind/0: caller_scope: nil
bind/0: x <-> (1 2)
bind/0: xx <-> (1 2)(1 2)
bind/0: skip keyword? (1 2)
bind/0: skipping ahead to (1 2)
bind/0: rest x
bind/0: x: (1 2)
bind/0: => {x:(1 2), }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {x:(1 2), }
bind/0: caller_scope: nil
bind/0: ($x $y) <-> (''1 ''2)
bind/0: ($x $y)($x $y) <-> (''1 ''2)(''1 ''2)
bind/0: skip keyword? (''1 ''2)
bind/0: skipping ahead to (''1 ''2)
bind/0: regular $x
bind/0: after eval 1
bind/0: $x: 1
bind/0: ($x $y)($y) <-> (''1 ''2)(''2)
bind/0: skip keyword? (''2)
bind/0: skipping ahead to (''2)
bind/0: regular $y
bind/0: after eval 2
bind/0: $y: 2
bind/0: ($x $y)nil <-> (''1 ''2)nil
bind/0: => {$x:1, $y:2, }
bind/0: Curr_lexical_scope: {caller_scope:nil, x:(1 2), }
bind/0: Curr_lexical_scope: {$x:1, $y:2, }->{caller_scope:nil, x:(1 2), }
bind/0: caller_scope: {caller_scope:nil, x:(1 2), }
- 22 test_eval_handles_splice_and_selective_quoting
bind/0: ('$var $val) <-> (f (fn ('x y) (cons x y)))
bind/0: ('$var $val)('$var $val) <-> (f (fn ('x y) (cons x y)))(f (fn ('x y) (cons x y)))
bind/0: skip keyword? (f (fn ('x y) (cons x y)))
bind/0: skipping ahead to (f (fn ('x y) (cons x y)))
bind/0: quoted $var
bind/0: positional arg
bind/0: $var: f
bind/0: regular arg
bind/0: ('$var $val)($val) <-> (f (fn ('x y) (cons x y)))((fn ('x y) (cons x y)))
bind/0: skip keyword? ((fn ('x y) (cons x y)))
bind/0: skipping ahead to ((fn ('x y) (cons x y)))
bind/0: regular $val
bind/0: ($params ... $body) <-> (('x y) (cons x y))
bind/0: ($params ... $body)($params ... $body) <-> (('x y) (cons x y))(('x y) (cons x y))
bind/0: skip keyword? (('x y) (cons x y))
bind/0: skipping ahead to (('x y) (cons x y))
bind/0: quoted $params
bind/0: positional arg
bind/0: $params: ('x y)
bind/0: regular arg
bind/0: ($params ... $body)$body <-> (('x y) (cons x y))((cons x y))
bind/0: skip keyword? ((cons x y))
bind/0: skipping ahead to ((cons x y))
bind/0: quoted rest $body
bind/0: $body: ((cons x y))
bind/0: => {$params:('x y), $body:((cons x y)), }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {$params:('x y), $body:((cons x y)), }
bind/0: caller_scope: nil
bind/0: after eval (object function {sig:('x y), body:((cons x y)), })
bind/0: $val: (object function {sig:('x y), body:((cons x y)), })
bind/0: ('$var $val)nil <-> (f (fn ('x y) (cons x y)))nil
bind/0: => {$val:(object function {sig:('x y), body:((cons x y)), }), $var:f, }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {$val:(object function {sig:('x y), body:((cons x y)), }), $var:f, }
bind/0: caller_scope: nil
bind/0: f: (object function {sig:('x y), body:((cons x y)), })
bind/0: ('$var $val) <-> (a 3)
bind/0: ('$var $val)('$var $val) <-> (a 3)(a 3)
bind/0: skip keyword? (a 3)
bind/0: skipping ahead to (a 3)
bind/0: quoted $var
bind/0: positional arg
bind/0: $var: a
bind/0: regular arg
bind/0: ('$var $val)($val) <-> (a 3)(3)
bind/0: skip keyword? (3)
bind/0: skipping ahead to (3)
bind/0: regular $val
bind/0: after eval 3
bind/0: $val: 3
bind/0: ('$var $val)nil <-> (a 3)nil
bind/0: => {$val:3, $var:a, }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {$val:3, $var:a, }
bind/0: caller_scope: nil
bind/0: a: 3
bind/0: ('$var $val) <-> (b 4)
bind/0: ('$var $val)('$var $val) <-> (b 4)(b 4)
bind/0: skip keyword? (b 4)
bind/0: skipping ahead to (b 4)
bind/0: quoted $var
bind/0: positional arg
bind/0: $var: b
bind/0: regular arg
bind/0: ('$var $val)($val) <-> (b 4)(4)
bind/0: skip keyword? (4)
bind/0: skipping ahead to (4)
bind/0: regular $val
bind/0: after eval 4
bind/0: $val: 4
bind/0: ('$var $val)nil <-> (b 4)nil
bind/0: => {$val:4, $var:b, }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {$val:4, $var:b, }
bind/0: caller_scope: nil
bind/0: b: 4
bind/0: ('$var $val) <-> (args '(b))
bind/0: ('$var $val)('$var $val) <-> (args '(b))(args '(b))
bind/0: skip keyword? (args '(b))
bind/0: skipping ahead to (args '(b))
bind/0: quoted $var
bind/0: positional arg
bind/0: $var: args
bind/0: regular arg
bind/0: ('$var $val)($val) <-> (args '(b))('(b))
bind/0: skip keyword? ('(b))
bind/0: skipping ahead to ('(b))
bind/0: regular $val
bind/0: after eval (b)
bind/0: $val: (b)
bind/0: ('$var $val)nil <-> (args '(b))nil
bind/0: => {$val:(b), $var:args, }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {$val:(b), $var:args, }
bind/0: caller_scope: nil
bind/0: args: (b)
bind/0: ('x y) <-> (a ''b)
bind/0: ('x y)('x y) <-> (a ''b)(a ''b)
bind/0: skip keyword? (a ''b)
bind/0: skipping ahead to (a ''b)
bind/0: quoted x
bind/0: positional arg
bind/0: x: a
bind/0: regular arg
bind/0: ('x y)(y) <-> (a ''b)(''b)
bind/0: skip keyword? (''b)
bind/0: skipping ahead to (''b)
bind/0: regular y
bind/0: after eval b
bind/0: y: b
bind/0: ('x y)nil <-> (a ''b)nil
bind/0: => {x:a, y:b, }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {x:a, y:b, }
bind/0: caller_scope: nil
bind/0: ($x $y) <-> (x y)
bind/0: ($x $y)($x $y) <-> (x y)(x y)
bind/0: skip keyword? (x y)
bind/0: skipping ahead to (x y)
bind/0: regular $x
bind/0: after eval a
bind/0: $x: a
bind/0: ($x $y)($y) <-> (x y)(y)
bind/0: skip keyword? (y)
bind/0: skipping ahead to (y)
bind/0: regular $y
bind/0: after eval b
bind/0: $y: b
bind/0: ($x $y)nil <-> (x y)nil
bind/0: => {$y:b, $x:a, }
bind/0: Curr_lexical_scope: {caller_scope:nil, x:a, y:b, }
bind/0: Curr_lexical_scope: {$y:b, $x:a, }->{caller_scope:nil, x:a, y:b, }
bind/0: caller_scope: {caller_scope:nil, x:a, y:b, }
- 23 test_eval_overrides_quoted_params_with_spliced_args
bind/0: ('$var $val) <-> (f (fn (x 'y) (cons x y)))
bind/0: ('$var $val)('$var $val) <-> (f (fn (x 'y) (cons x y)))(f (fn (x 'y) (cons x y)))
bind/0: skip keyword? (f (fn (x 'y) (cons x y)))
bind/0: skipping ahead to (f (fn (x 'y) (cons x y)))
bind/0: quoted $var
bind/0: positional arg
bind/0: $var: f
bind/0: regular arg
bind/0: ('$var $val)($val) <-> (f (fn (x 'y) (cons x y)))((fn (x 'y) (cons x y)))
bind/0: skip keyword? ((fn (x 'y) (cons x y)))
bind/0: skipping ahead to ((fn (x 'y) (cons x y)))
bind/0: regular $val
bind/0: ($params ... $body) <-> ((x 'y) (cons x y))
bind/0: ($params ... $body)($params ... $body) <-> ((x 'y) (cons x y))((x 'y) (cons x y))
bind/0: skip keyword? ((x 'y) (cons x y))
bind/0: skipping ahead to ((x 'y) (cons x y))
bind/0: quoted $params
bind/0: positional arg
bind/0: $params: (x 'y)
bind/0: regular arg
bind/0: ($params ... $body)$body <-> ((x 'y) (cons x y))((cons x y))
bind/0: skip keyword? ((cons x y))
bind/0: skipping ahead to ((cons x y))
bind/0: quoted rest $body
bind/0: $body: ((cons x y))
bind/0: => {$params:(x 'y), $body:((cons x y)), }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {$params:(x 'y), $body:((cons x y)), }
bind/0: caller_scope: nil
bind/0: after eval (object function {sig:(x 'y), body:((cons x y)), })
bind/0: $val: (object function {sig:(x 'y), body:((cons x y)), })
bind/0: ('$var $val)nil <-> (f (fn (x 'y) (cons x y)))nil
bind/0: => {$var:f, $val:(object function {sig:(x 'y), body:((cons x y)), }), }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {$var:f, $val:(object function {sig:(x 'y), body:((cons x y)), }), }
bind/0: caller_scope: nil
bind/0: f: (object function {sig:(x 'y), body:((cons x y)), })
bind/0: ('$var $val) <-> (a 3)
bind/0: ('$var $val)('$var $val) <-> (a 3)(a 3)
bind/0: skip keyword? (a 3)
bind/0: skipping ahead to (a 3)
bind/0: quoted $var
bind/0: positional arg
bind/0: $var: a
bind/0: regular arg
bind/0: ('$var $val)($val) <-> (a 3)(3)
bind/0: skip keyword? (3)
bind/0: skipping ahead to (3)
bind/0: regular $val
bind/0: after eval 3
bind/0: $val: 3
bind/0: ('$var $val)nil <-> (a 3)nil
bind/0: => {$var:a, $val:3, }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {$var:a, $val:3, }
bind/0: caller_scope: nil
bind/0: a: 3
bind/0: ('$var $val) <-> (b 4)
bind/0: ('$var $val)('$var $val) <-> (b 4)(b 4)
bind/0: skip keyword? (b 4)
bind/0: skipping ahead to (b 4)
bind/0: quoted $var
bind/0: positional arg
bind/0: $var: b
bind/0: regular arg
bind/0: ('$var $val)($val) <-> (b 4)(4)
bind/0: skip keyword? (4)
bind/0: skipping ahead to (4)
bind/0: regular $val
bind/0: after eval 4
bind/0: $val: 4
bind/0: ('$var $val)nil <-> (b 4)nil
bind/0: => {$var:b, $val:4, }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {$var:b, $val:4, }
bind/0: caller_scope: nil
bind/0: b: 4
bind/0: ('$var $val) <-> (args '(a b))
bind/0: ('$var $val)('$var $val) <-> (args '(a b))(args '(a b))
bind/0: skip keyword? (args '(a b))
bind/0: skipping ahead to (args '(a b))
bind/0: quoted $var
bind/0: positional arg
bind/0: $var: args
bind/0: regular arg
bind/0: ('$var $val)($val) <-> (args '(a b))('(a b))
bind/0: skip keyword? ('(a b))
bind/0: skipping ahead to ('(a b))
bind/0: regular $val
bind/0: after eval (a b)
bind/0: $val: (a b)
bind/0: ('$var $val)nil <-> (args '(a b))nil
bind/0: => {$var:args, $val:(a b), }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {$var:args, $val:(a b), }
bind/0: caller_scope: nil
bind/0: args: (a b)
bind/0: (x 'y) <-> (''a ''b)
bind/0: (x 'y)(x 'y) <-> (''a ''b)(''a ''b)
bind/0: skip keyword? (''a ''b)
bind/0: skipping ahead to (''a ''b)
bind/0: regular x
bind/0: after eval a
bind/0: x: a
bind/0: (x 'y)('y) <-> (''a ''b)(''b)
bind/0: skip keyword? (''b)
bind/0: skipping ahead to (''b)
bind/0: quoted y
bind/0: positional arg
bind/0: y: ''b
bind/0: regular arg
bind/0: (x 'y)nil <-> (''a ''b)nil
bind/0: => {y:''b, x:a, }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {y:''b, x:a, }
bind/0: caller_scope: nil
bind/0: ($x $y) <-> (x y)
bind/0: ($x $y)($x $y) <-> (x y)(x y)
bind/0: skip keyword? (x y)
bind/0: skipping ahead to (x y)
bind/0: regular $x
bind/0: after eval a
bind/0: $x: a
bind/0: ($x $y)($y) <-> (x y)(y)
bind/0: skip keyword? (y)
bind/0: skipping ahead to (y)
bind/0: regular $y
bind/0: after eval b
bind/0: $y: b
bind/0: ($x $y)nil <-> (x y)nil
bind/0: => {$x:a, $y:b, }
bind/0: Curr_lexical_scope: {y:''b, x:a, caller_scope:nil, }
bind/0: Curr_lexical_scope: {$x:a, $y:b, }->{y:''b, x:a, caller_scope:nil, }
bind/0: caller_scope: {y:''b, x:a, caller_scope:nil, }
- 24 test_eval_handles_already_evald_arg
bind/0: a: 3
bind/0: ($params ... $body) <-> ((x) 3)
bind/0: ($params ... $body)($params ... $body) <-> ((x) 3)((x) 3)
bind/0: skip keyword? ((x) 3)
bind/0: skipping ahead to ((x) 3)
bind/0: quoted $params
bind/0: positional arg
bind/0: $params: (x)
bind/0: regular arg
bind/0: ($params ... $body)$body <-> ((x) 3)(3)
bind/0: skip keyword? (3)
bind/0: skipping ahead to (3)
bind/0: quoted rest $body
bind/0: $body: (3)
bind/0: => {$params:(x), $body:(3), }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {$params:(x), $body:(3), }
bind/0: caller_scope: nil
bind/0: (x) <-> (''a)
bind/0: (x)(x) <-> (''a)(''a)
bind/0: skip keyword? (''a)
bind/0: skipping ahead to (''a)
bind/0: regular x
bind/0: after eval a
bind/0: x: a
bind/0: (x)nil <-> (''a)nil
bind/0: => {x:a, }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {x:a, }
bind/0: caller_scope: nil
- 25 test_eval_handles_already_evald_arg_quoted_param
bind/0: a: 3
bind/0: ($params ... $body) <-> ('(x) 3)
bind/0: ($params ... $body)($params ... $body) <-> ('(x) 3)('(x) 3)
bind/0: skip keyword? ('(x) 3)
bind/0: skipping ahead to ('(x) 3)
bind/0: quoted $params
bind/0: positional arg
bind/0: $params: '(x)
bind/0: regular arg
bind/0: ($params ... $body)$body <-> ('(x) 3)(3)
bind/0: skip keyword? (3)
bind/0: skipping ahead to (3)
bind/0: quoted rest $body
bind/0: $body: (3)
bind/0: => {$params:'(x), $body:(3), }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {$params:'(x), $body:(3), }
bind/0: caller_scope: nil
bind/0: (x) <-> (''a)
bind/0: (x)(x) <-> (''a)(''a)
bind/0: skip keyword? (''a)
bind/0: skipping ahead to (''a)
bind/0: quoted x
bind/0: positional arg
bind/0: x: ''a
bind/0: regular arg
bind/0: (x)nil <-> (''a)nil
bind/0: => {x:''a, }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {x:''a, }
bind/0: caller_scope: nil
- 26 test_eval_handles_multiply_already_evald_arg
bind/0: ($params ... $body) <-> ((x) 3)
bind/0: ($params ... $body)($params ... $body) <-> ((x) 3)((x) 3)
bind/0: skip keyword? ((x) 3)
bind/0: skipping ahead to ((x) 3)
bind/0: quoted $params
bind/0: positional arg
bind/0: $params: (x)
bind/0: regular arg
bind/0: ($params ... $body)$body <-> ((x) 3)(3)
bind/0: skip keyword? (3)
bind/0: skipping ahead to (3)
bind/0: quoted rest $body
bind/0: $body: (3)
bind/0: => {$params:(x), $body:(3), }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {$params:(x), $body:(3), }
bind/0: caller_scope: nil
bind/0: (x) <-> (''''a)
bind/0: (x)(x) <-> (''''a)(''''a)
bind/0: skip keyword? (''''a)
bind/0: skipping ahead to (''''a)
bind/0: regular x
bind/0: after eval a
bind/0: x: a
bind/0: (x)nil <-> (''''a)nil
bind/0: => {x:a, }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {x:a, }
bind/0: caller_scope: nil
- 27 test_eval_handles_already_evald_rest_arg
bind/0: a: 3
bind/0: ($params ... $body) <-> (x 3)
bind/0: ($params ... $body)($params ... $body) <-> (x 3)(x 3)
bind/0: skip keyword? (x 3)
bind/0: skipping ahead to (x 3)
bind/0: quoted $params
bind/0: positional arg
bind/0: $params: x
bind/0: regular arg
bind/0: ($params ... $body)$body <-> (x 3)(3)
bind/0: skip keyword? (3)
bind/0: skipping ahead to (3)
bind/0: quoted rest $body
bind/0: $body: (3)
bind/0: => {$params:x, $body:(3), }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {$params:x, $body:(3), }
bind/0: caller_scope: nil
bind/0: x <-> (''a)
bind/0: xx <-> (''a)(''a)
bind/0: skip keyword? (''a)
bind/0: skipping ahead to (''a)
bind/0: rest x
bind/0: x: (a)
bind/0: => {x:(a), }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {x:(a), }
bind/0: caller_scope: nil
- 28 test_eval_splice_on_macros_with_backquote
bind/0: ('$var $val) <-> (m (fn '(x y) (eval `(cons ,x ,y) caller_scope)))
bind/0: ('$var $val)('$var $val) <-> (m (fn '(x y) (eval `(cons ,x ,y) caller_scope)))(m (fn '(x y) (eval `(cons ,x ,y) caller_scope)))
bind/0: skip keyword? (m (fn '(x y) (eval `(cons ,x ,y) caller_scope)))
bind/0: skipping ahead to (m (fn '(x y) (eval `(cons ,x ,y) caller_scope)))
bind/0: quoted $var
bind/0: positional arg
bind/0: $var: m
bind/0: regular arg
bind/0: ('$var $val)($val) <-> (m (fn '(x y) (eval `(cons ,x ,y) caller_scope)))((fn '(x y) (eval `(cons ,x ,y) caller_scope)))
bind/0: skip keyword? ((fn '(x y) (eval `(cons ,x ,y) caller_scope)))
bind/0: skipping ahead to ((fn '(x y) (eval `(cons ,x ,y) caller_scope)))
bind/0: regular $val
bind/0: ($params ... $body) <-> ('(x y) (eval `(cons ,x ,y) caller_scope))
bind/0: ($params ... $body)($params ... $body) <-> ('(x y) (eval `(cons ,x ,y) caller_scope))('(x y) (eval `(cons ,x ,y) caller_scope))
bind/0: skip keyword? ('(x y) (eval `(cons ,x ,y) caller_scope))
bind/0: skipping ahead to ('(x y) (eval `(cons ,x ,y) caller_scope))
bind/0: quoted $params
bind/0: positional arg
bind/0: $params: '(x y)
bind/0: regular arg
bind/0: ($params ... $body)$body <-> ('(x y) (eval `(cons ,x ,y) caller_scope))((eval `(cons ,x ,y) caller_scope))
bind/0: skip keyword? ((eval `(cons ,x ,y) caller_scope))
bind/0: skipping ahead to ((eval `(cons ,x ,y) caller_scope))
bind/0: quoted rest $body
bind/0: $body: ((eval `(cons ,x ,y) caller_scope))
bind/0: => {$params:'(x y), $body:((eval `(cons ,x ,y) caller_scope)), }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {$params:'(x y), $body:((eval `(cons ,x ,y) caller_scope)), }
bind/0: caller_scope: nil
bind/0: after eval (object function {sig:'(x y), body:((eval `(cons ,x ,y) caller_scope)), })
bind/0: $val: (object function {sig:'(x y), body:((eval `(cons ,x ,y) caller_scope)), })
bind/0: ('$var $val)nil <-> (m (fn '(x y) (eval `(cons ,x ,y) caller_scope)))nil
bind/0: => {$val:(object function {sig:'(x y), body:((eval `(cons ,x ,y) caller_scope)), }), $var:m, }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {$val:(object function {sig:'(x y), body:((eval `(cons ,x ,y) caller_scope)), }), $var:m, }
bind/0: caller_scope: nil
bind/0: m: (object function {sig:'(x y), body:((eval `(cons ,x ,y) caller_scope)), })
bind/0: ('$var $val) <-> (a 3)
bind/0: ('$var $val)('$var $val) <-> (a 3)(a 3)
bind/0: skip keyword? (a 3)
bind/0: skipping ahead to (a 3)
bind/0: quoted $var
bind/0: positional arg
bind/0: $var: a
bind/0: regular arg
bind/0: ('$var $val)($val) <-> (a 3)(3)
bind/0: skip keyword? (3)
bind/0: skipping ahead to (3)
bind/0: regular $val
bind/0: after eval 3
bind/0: $val: 3
bind/0: ('$var $val)nil <-> (a 3)nil
bind/0: => {$val:3, $var:a, }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {$val:3, $var:a, }
bind/0: caller_scope: nil
bind/0: a: 3
bind/0: ('$var $val) <-> (b 4)
bind/0: ('$var $val)('$var $val) <-> (b 4)(b 4)
bind/0: skip keyword? (b 4)
bind/0: skipping ahead to (b 4)
bind/0: quoted $var
bind/0: positional arg
bind/0: $var: b
bind/0: regular arg
bind/0: ('$var $val)($val) <-> (b 4)(4)
bind/0: skip keyword? (4)
bind/0: skipping ahead to (4)
bind/0: regular $val
bind/0: after eval 4
bind/0: $val: 4
bind/0: ('$var $val)nil <-> (b 4)nil
bind/0: => {$val:4, $var:b, }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {$val:4, $var:b, }
bind/0: caller_scope: nil
bind/0: b: 4
bind/0: ('$var $val) <-> (args '(a b))
bind/0: ('$var $val)('$var $val) <-> (args '(a b))(args '(a b))
bind/0: skip keyword? (args '(a b))
bind/0: skipping ahead to (args '(a b))
bind/0: quoted $var
bind/0: positional arg
bind/0: $var: args
bind/0: regular arg
bind/0: ('$var $val)($val) <-> (args '(a b))('(a b))
bind/0: skip keyword? ('(a b))
bind/0: skipping ahead to ('(a b))
bind/0: regular $val
bind/0: after eval (a b)
bind/0: $val: (a b)
bind/0: ('$var $val)nil <-> (args '(a b))nil
bind/0: => {$val:(a b), $var:args, }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {$val:(a b), $var:args, }
bind/0: caller_scope: nil
bind/0: args: (a b)
bind/0: (x y) <-> (''a ''b)
bind/0: (x y)(x y) <-> (''a ''b)(''a ''b)
bind/0: skip keyword? (''a ''b)
bind/0: skipping ahead to (''a ''b)
bind/0: quoted x
bind/0: positional arg
bind/0: x: ''a
bind/0: regular arg
bind/0: (x y)(y) <-> (''a ''b)(''b)
bind/0: skip keyword? (''b)
bind/0: skipping ahead to (''b)
bind/0: quoted y
bind/0: positional arg
bind/0: y: ''b
bind/0: regular arg
bind/0: (x y)nil <-> (''a ''b)nil
bind/0: => {x:''a, y:''b, }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {x:''a, y:''b, }
bind/0: caller_scope: nil
bind/0: ('$x $scope) <-> (`(cons ,x ,y) caller_scope)
bind/0: ('$x $scope)('$x $scope) <-> (`(cons ,x ,y) caller_scope)(`(cons ,x ,y) caller_scope)
bind/0: skip keyword? (`(cons ,x ,y) caller_scope)
bind/0: skipping ahead to (`(cons ,x ,y) caller_scope)
bind/0: quoted $x
bind/0: positional arg
bind/0: $x: `(cons ,x ,y)
bind/0: regular arg
bind/0: ('$x $scope)($scope) <-> (`(cons ,x ,y) caller_scope)(caller_scope)
bind/0: skip keyword? (caller_scope)
bind/0: skipping ahead to (caller_scope)
bind/0: regular $scope
bind/0: after eval nil
bind/0: $scope: nil
bind/0: ('$x $scope)nil <-> (`(cons ,x ,y) caller_scope)nil
bind/0: => {$scope:nil, $x:`(cons ,x ,y), }
bind/0: Curr_lexical_scope: {x:''a, caller_scope:nil, y:''b, }
bind/0: Curr_lexical_scope: {$scope:nil, $x:`(cons ,x ,y), }->{x:''a, caller_scope:nil, y:''b, }
bind/0: caller_scope: {x:''a, caller_scope:nil, y:''b, }
bind/0: ($x $y) <-> (''''a ''''b)
bind/0: ($x $y)($x $y) <-> (''''a ''''b)(''''a ''''b)
bind/0: skip keyword? (''''a ''''b)
bind/0: skipping ahead to (''''a ''''b)
bind/0: regular $x
bind/0: after eval a
bind/0: $x: a
bind/0: ($x $y)($y) <-> (''''a ''''b)(''''b)
bind/0: skip keyword? (''''b)
bind/0: skipping ahead to (''''b)
bind/0: regular $y
bind/0: after eval b
bind/0: $y: b
bind/0: ($x $y)nil <-> (''''a ''''b)nil
bind/0: => {$y:b, $x:a, }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {$y:b, $x:a, }
bind/0: caller_scope: nil
- 29 test_eval_splice_on_backquoteless_macros_warns
bind/0: ('$var $val) <-> (m (fn '(x y) (eval (cons 'cons (cons x (cons y nil))) caller_scope)))
bind/0: ('$var $val)('$var $val) <-> (m (fn '(x y) (eval (cons 'cons (cons x (cons y nil))) caller_scope)))(m (fn '(x y) (eval (cons 'cons (cons x (cons y nil))) caller_scope)))
bind/0: skip keyword? (m (fn '(x y) (eval (cons 'cons (cons x (cons y nil))) caller_scope)))
bind/0: skipping ahead to (m (fn '(x y) (eval (cons 'cons (cons x (cons y nil))) caller_scope)))
bind/0: quoted $var
bind/0: positional arg
bind/0: $var: m
bind/0: regular arg
bind/0: ('$var $val)($val) <-> (m (fn '(x y) (eval (cons 'cons (cons x (cons y nil))) caller_scope)))((fn '(x y) (eval (cons 'cons (cons x (cons y nil))) caller_scope)))
bind/0: skip keyword? ((fn '(x y) (eval (cons 'cons (cons x (cons y nil))) caller_scope)))
bind/0: skipping ahead to ((fn '(x y) (eval (cons 'cons (cons x (cons y nil))) caller_scope)))
bind/0: regular $val
bind/0: ($params ... $body) <-> ('(x y) (eval (cons 'cons (cons x (cons y nil))) caller_scope))
bind/0: ($params ... $body)($params ... $body) <-> ('(x y) (eval (cons 'cons (cons x (cons y nil))) caller_scope))('(x y) (eval (cons 'cons (cons x (cons y nil))) caller_scope))
bind/0: skip keyword? ('(x y) (eval (cons 'cons (cons x (cons y nil))) caller_scope))
bind/0: skipping ahead to ('(x y) (eval (cons 'cons (cons x (cons y nil))) caller_scope))
bind/0: quoted $params
bind/0: positional arg
bind/0: $params: '(x y)
bind/0: regular arg
bind/0: ($params ... $body)$body <-> ('(x y) (eval (cons 'cons (cons x (cons y nil))) caller_scope))((eval (cons 'cons (cons x (cons y nil))) caller_scope))
bind/0: skip keyword? ((eval (cons 'cons (cons x (cons y nil))) caller_scope))
bind/0: skipping ahead to ((eval (cons 'cons (cons x (cons y nil))) caller_scope))
bind/0: quoted rest $body
bind/0: $body: ((eval (cons 'cons (cons x (cons y nil))) caller_scope))
bind/0: => {$params:'(x y), $body:((eval (cons 'cons (cons x (cons y nil))) caller_scope)), }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {$params:'(x y), $body:((eval (cons 'cons (cons x (cons y nil))) caller_scope)), }
bind/0: caller_scope: nil
bind/0: after eval (object function {sig:'(x y), body:((eval (cons 'cons (cons x (cons y nil))) caller_scope)), })
bind/0: $val: (object function {sig:'(x y), body:((eval (cons 'cons (cons x (cons y nil))) caller_scope)), })
bind/0: ('$var $val)nil <-> (m (fn '(x y) (eval (cons 'cons (cons x (cons y nil))) caller_scope)))nil
bind/0: => {$val:(object function {sig:'(x y), body:((eval (cons 'cons (cons x (cons y nil))) caller_scope)), }), $var:m, }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {$val:(object function {sig:'(x y), body:((eval (cons 'cons (cons x (cons y nil))) caller_scope)), }), $var:m, }
bind/0: caller_scope: nil
bind/0: m: (object function {sig:'(x y), body:((eval (cons 'cons (cons x (cons y nil))) caller_scope)), })
bind/0: ('$var $val) <-> (a 3)
bind/0: ('$var $val)('$var $val) <-> (a 3)(a 3)
bind/0: skip keyword? (a 3)
bind/0: skipping ahead to (a 3)
bind/0: quoted $var
bind/0: positional arg
bind/0: $var: a
bind/0: regular arg
bind/0: ('$var $val)($val) <-> (a 3)(3)
bind/0: skip keyword? (3)
bind/0: skipping ahead to (3)
bind/0: regular $val
bind/0: after eval 3
bind/0: $val: 3
bind/0: ('$var $val)nil <-> (a 3)nil
bind/0: => {$val:3, $var:a, }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {$val:3, $var:a, }
bind/0: caller_scope: nil
bind/0: a: 3
bind/0: ('$var $val) <-> (b 4)
bind/0: ('$var $val)('$var $val) <-> (b 4)(b 4)
bind/0: skip keyword? (b 4)
bind/0: skipping ahead to (b 4)
bind/0: quoted $var
bind/0: positional arg
bind/0: $var: b
bind/0: regular arg
bind/0: ('$var $val)($val) <-> (b 4)(4)
bind/0: skip keyword? (4)
bind/0: skipping ahead to (4)
bind/0: regular $val
bind/0: after eval 4
bind/0: $val: 4
bind/0: ('$var $val)nil <-> (b 4)nil
bind/0: => {$val:4, $var:b, }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {$val:4, $var:b, }
bind/0: caller_scope: nil
bind/0: b: 4
bind/0: ('$var $val) <-> (args '(a b))
bind/0: ('$var $val)('$var $val) <-> (args '(a b))(args '(a b))
bind/0: skip keyword? (args '(a b))
bind/0: skipping ahead to (args '(a b))
bind/0: quoted $var
bind/0: positional arg
bind/0: $var: args
bind/0: regular arg
bind/0: ('$var $val)($val) <-> (args '(a b))('(a b))
bind/0: skip keyword? ('(a b))
bind/0: skipping ahead to ('(a b))
bind/0: regular $val
bind/0: after eval (a b)
bind/0: $val: (a b)
bind/0: ('$var $val)nil <-> (args '(a b))nil
bind/0: => {$val:(a b), $var:args, }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {$val:(a b), $var:args, }
bind/0: caller_scope: nil
bind/0: args: (a b)
bind/0: (x y) <-> (''a ''b)
bind/0: (x y)(x y) <-> (''a ''b)(''a ''b)
bind/0: skip keyword? (''a ''b)
bind/0: skipping ahead to (''a ''b)
bind/0: quoted x
bind/0: positional arg
bind/0: x: ''a
bind/0: regular arg
bind/0: (x y)(y) <-> (''a ''b)(''b)
bind/0: skip keyword? (''b)
bind/0: skipping ahead to (''b)
bind/0: quoted y
bind/0: positional arg
bind/0: y: ''b
bind/0: regular arg
bind/0: (x y)nil <-> (''a ''b)nil
bind/0: => {x:''a, y:''b, }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {x:''a, y:''b, }
bind/0: caller_scope: nil
bind/0: ('$x $scope) <-> ((cons 'cons (cons x (cons y nil))) caller_scope)
bind/0: ('$x $scope)('$x $scope) <-> ((cons 'cons (cons x (cons y nil))) caller_scope)((cons 'cons (cons x (cons y nil))) caller_scope)
bind/0: skip keyword? ((cons 'cons (cons x (cons y nil))) caller_scope)
bind/0: skipping ahead to ((cons 'cons (cons x (cons y nil))) caller_scope)
bind/0: quoted $x
bind/0: positional arg
bind/0: $x: (cons 'cons (cons x (cons y nil)))
bind/0: regular arg
bind/0: ('$x $scope)($scope) <-> ((cons 'cons (cons x (cons y nil))) caller_scope)(caller_scope)
bind/0: skip keyword? (caller_scope)
bind/0: skipping ahead to (caller_scope)
bind/0: regular $scope
bind/0: after eval nil
bind/0: $scope: nil
bind/0: ('$x $scope)nil <-> ((cons 'cons (cons x (cons y nil))) caller_scope)nil
bind/0: => {$x:(cons 'cons (cons x (cons y nil))), $scope:nil, }
bind/0: Curr_lexical_scope: {caller_scope:nil, x:''a, y:''b, }
bind/0: Curr_lexical_scope: {$x:(cons 'cons (cons x (cons y nil))), $scope:nil, }->{caller_scope:nil, x:''a, y:''b, }
bind/0: caller_scope: {caller_scope:nil, x:''a, y:''b, }
bind/0: ($x $y) <-> ('cons (cons x (cons y nil)))
bind/0: ($x $y)($x $y) <-> ('cons (cons x (cons y nil)))('cons (cons x (cons y nil)))
bind/0: skip keyword? ('cons (cons x (cons y nil)))
bind/0: skipping ahead to ('cons (cons x (cons y nil)))
bind/0: regular $x
bind/0: after eval cons
bind/0: $x: cons
bind/0: ($x $y)($y) <-> ('cons (cons x (cons y nil)))((cons x (cons y nil)))
bind/0: skip keyword? ((cons x (cons y nil)))
bind/0: skipping ahead to ((cons x (cons y nil)))
bind/0: regular $y
bind/0: ($x $y) <-> (x (cons y nil))
bind/0: ($x $y)($x $y) <-> (x (cons y nil))(x (cons y nil))
bind/0: skip keyword? (x (cons y nil))
bind/0: skipping ahead to (x (cons y nil))
bind/0: regular $x
bind/0: after eval ''a
bind/0: $x: ''a
bind/0: ($x $y)($y) <-> (x (cons y nil))((cons y nil))
bind/0: skip keyword? ((cons y nil))
bind/0: skipping ahead to ((cons y nil))
bind/0: regular $y
bind/0: ($x $y) <-> (y nil)
bind/0: ($x $y)($x $y) <-> (y nil)(y nil)
bind/0: skip keyword? (y nil)
bind/0: skipping ahead to (y nil)
bind/0: regular $x
bind/0: after eval ''b
bind/0: $x: ''b
bind/0: ($x $y)($y) <-> (y nil)(nil)
bind/0: skip keyword? (nil)
bind/0: skipping ahead to (nil)
bind/0: regular $y
bind/0: after eval nil
bind/0: $y: nil
bind/0: ($x $y)nil <-> (y nil)nil
bind/0: => {$y:nil, $x:''b, }
bind/0: Curr_lexical_scope: {caller_scope:{caller_scope:nil, x:''a, y:''b, }, $x:(cons 'cons (cons x (cons y nil))), $scope:nil, }->{caller_scope:nil, x:''a, y:''b, }
bind/0: Curr_lexical_scope: {$y:nil, $x:''b, }->{caller_scope:{caller_scope:nil, x:''a, y:''b, }, $x:(cons 'cons (cons x (cons y nil))), $scope:nil, }->{caller_scope:nil, x:''a, y:''b, }
bind/0: caller_scope: {caller_scope:{caller_scope:nil, x:''a, y:''b, }, $x:(cons 'cons (cons x (cons y nil))), $scope:nil, }->{caller_scope:nil, x:''a, y:''b, }
bind/0: after eval (b)
bind/0: $y: (b)
bind/0: ($x $y)nil <-> (x (cons y nil))nil
bind/0: => {$y:(b), $x:''a, }
bind/0: Curr_lexical_scope: {caller_scope:{caller_scope:nil, x:''a, y:''b, }, $x:(cons 'cons (cons x (cons y nil))), $scope:nil, }->{caller_scope:nil, x:''a, y:''b, }
bind/0: Curr_lexical_scope: {$y:(b), $x:''a, }->{caller_scope:{caller_scope:nil, x:''a, y:''b, }, $x:(cons 'cons (cons x (cons y nil))), $scope:nil, }->{caller_scope:nil, x:''a, y:''b, }
bind/0: caller_scope: {caller_scope:{caller_scope:nil, x:''a, y:''b, }, $x:(cons 'cons (cons x (cons y nil))), $scope:nil, }->{caller_scope:nil, x:''a, y:''b, }
bind/0: after eval (a b)
bind/0: $y: (a b)
bind/0: ($x $y)nil <-> ('cons (cons x (cons y nil)))nil
bind/0: => {$y:(a b), $x:cons, }
bind/0: Curr_lexical_scope: {caller_scope:{caller_scope:nil, x:''a, y:''b, }, $x:(cons 'cons (cons x (cons y nil))), $scope:nil, }->{caller_scope:nil, x:''a, y:''b, }
bind/0: Curr_lexical_scope: {$y:(a b), $x:cons, }->{caller_scope:{caller_scope:nil, x:''a, y:''b, }, $x:(cons 'cons (cons x (cons y nil))), $scope:nil, }->{caller_scope:nil, x:''a, y:''b, }
bind/0: caller_scope: {caller_scope:{caller_scope:nil, x:''a, y:''b, }, $x:(cons 'cons (cons x (cons y nil))), $scope:nil, }->{caller_scope:nil, x:''a, y:''b, }
bind/0: ($x $y) <-> (a b)
bind/0: ($x $y)($x $y) <-> (a b)(a b)
bind/0: skip keyword? (a b)
bind/0: skipping ahead to (a b)
bind/0: regular $x
bind/0: after eval 3
bind/0: $x: 3
bind/0: ($x $y)($y) <-> (a b)(b)
bind/0: skip keyword? (b)
bind/0: skipping ahead to (b)
bind/0: regular $y
bind/0: after eval 4
bind/0: $y: 4
bind/0: ($x $y)nil <-> (a b)nil
bind/0: => {$y:4, $x:3, }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {$y:4, $x:3, }
bind/0: caller_scope: nil
- 30 test_eval_handles_keyword_args
bind/0: ($params ... $body) <-> ((a b) a)
bind/0: ($params ... $body)($params ... $body) <-> ((a b) a)((a b) a)
bind/0: skip keyword? ((a b) a)
bind/0: skipping ahead to ((a b) a)
bind/0: quoted $params
bind/0: positional arg
bind/0: $params: (a b)
bind/0: regular arg
bind/0: ($params ... $body)$body <-> ((a b) a)(a)
bind/0: skip keyword? (a)
bind/0: skipping ahead to (a)
bind/0: quoted rest $body
bind/0: $body: (a)
bind/0: => {$params:(a b), $body:(a), }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {$params:(a b), $body:(a), }
bind/0: caller_scope: nil
bind/0: (a b) <-> (2 :a 1)
bind/0: (a b)(a b) <-> (2 :a 1)(2 :a 1)
bind/0: skip keyword? (2 :a 1)
bind/0: skipping ahead to (2 :a 1)
bind/0: regular a
bind/0: regular keyword arg 1
bind/0: after eval 1
bind/0: a: 1
bind/0: (a b)(b) <-> (2 :a 1)(2 :a 1)
bind/0: skip keyword? (2 :a 1)
bind/0: skipping ahead to (2 :a 1)
bind/0: regular b
bind/0: after eval 2
bind/0: b: 2
bind/0: (a b)nil <-> (2 :a 1)(:a 1)
bind/0: => {a:1, b:2, }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {a:1, b:2, }
bind/0: caller_scope: nil
- 31 test_eval_handles_keyword_args2
bind/0: ($params ... $body) <-> ((a b c) c)
bind/0: ($params ... $body)($params ... $body) <-> ((a b c) c)((a b c) c)
bind/0: skip keyword? ((a b c) c)
bind/0: skipping ahead to ((a b c) c)
bind/0: quoted $params
bind/0: positional arg
bind/0: $params: (a b c)
bind/0: regular arg
bind/0: ($params ... $body)$body <-> ((a b c) c)(c)
bind/0: skip keyword? (c)
bind/0: skipping ahead to (c)
bind/0: quoted rest $body
bind/0: $body: (c)
bind/0: => {$params:(a b c), $body:(c), }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {$params:(a b c), $body:(c), }
bind/0: caller_scope: nil
bind/0: (a b c) <-> (:c 1 2)
bind/0: (a b c)(a b c) <-> (:c 1 2)(:c 1 2)
bind/0: skip keyword? (:c 1 2)
bind/0: match? c in (a b c)
bind/0: match? c in (b c)
bind/0: match? c in (c)
bind/0: skipping keyword arg:c 1
bind/0: skip keyword? (2)
bind/0: skipping ahead to (2)
bind/0: regular a
bind/0: after eval 2
bind/0: a: 2
bind/0: (a b c)(b c) <-> (:c 1 2)nil
bind/0: skip keyword? nil
bind/0: skipping ahead to nil
bind/0: regular b
bind/0: after eval nil
bind/0: b: nil
bind/0: (a b c)(c) <-> (:c 1 2)nil
bind/0: skip keyword? nil
bind/0: skipping ahead to nil
bind/0: regular c
bind/0: regular keyword arg 1
bind/0: after eval 1
bind/0: c: 1
bind/0: (a b c)nil <-> (:c 1 2)nil
bind/0: => {b:nil, c:1, a:2, }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {b:nil, c:1, a:2, }
bind/0: caller_scope: nil
- 32 test_eval_handles_nil_keyword_arg
bind/0: ($params ... $body) <-> ((a) a)
bind/0: ($params ... $body)($params ... $body) <-> ((a) a)((a) a)
bind/0: skip keyword? ((a) a)
bind/0: skipping ahead to ((a) a)
bind/0: quoted $params
bind/0: positional arg
bind/0: $params: (a)
bind/0: regular arg
bind/0: ($params ... $body)$body <-> ((a) a)(a)
bind/0: skip keyword? (a)
bind/0: skipping ahead to (a)
bind/0: quoted rest $body
bind/0: $body: (a)
bind/0: => {$params:(a), $body:(a), }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {$params:(a), $body:(a), }
bind/0: caller_scope: nil
bind/0: (a) <-> (2 :a nil)
bind/0: (a)(a) <-> (2 :a nil)(2 :a nil)
bind/0: skip keyword? (2 :a nil)
bind/0: skipping ahead to (2 :a nil)
bind/0: regular a
bind/0: regular keyword arg nil
bind/0: after eval nil
bind/0: a: nil
bind/0: (a)nil <-> (2 :a nil)(2 :a nil)
bind/0: => {a:nil, }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {a:nil, }
bind/0: caller_scope: nil
- 33 test_eval_handles_quoted_keyword_args
bind/0: ($params ... $body) <-> ((a b 'c) c)
bind/0: ($params ... $body)($params ... $body) <-> ((a b 'c) c)((a b 'c) c)
bind/0: skip keyword? ((a b 'c) c)
bind/0: skipping ahead to ((a b 'c) c)
bind/0: quoted $params
bind/0: positional arg
bind/0: $params: (a b 'c)
bind/0: regular arg
bind/0: ($params ... $body)$body <-> ((a b 'c) c)(c)
bind/0: skip keyword? (c)
bind/0: skipping ahead to (c)
bind/0: quoted rest $body
bind/0: $body: (c)
bind/0: => {$params:(a b 'c), $body:(c), }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {$params:(a b 'c), $body:(c), }
bind/0: caller_scope: nil
bind/0: (a b 'c) <-> (:c 1 2)
bind/0: (a b 'c)(a b 'c) <-> (:c 1 2)(:c 1 2)
bind/0: skip keyword? (:c 1 2)
bind/0: match? c in (a b 'c)
bind/0: match? c in (b 'c)
bind/0: match? c in ('c)
bind/0: skipping keyword arg:c 1
bind/0: skip keyword? (2)
bind/0: skipping ahead to (2)
bind/0: regular a
bind/0: after eval 2
bind/0: a: 2
bind/0: (a b 'c)(b 'c) <-> (:c 1 2)nil
bind/0: skip keyword? nil
bind/0: skipping ahead to nil
bind/0: regular b
bind/0: after eval nil
bind/0: b: nil
bind/0: (a b 'c)('c) <-> (:c 1 2)nil
bind/0: skip keyword? nil
bind/0: skipping ahead to nil
bind/0: quoted c
bind/0: keyword arg
bind/0: c: 1
bind/0: (a b 'c)nil <-> (:c 1 2)nil
bind/0: => {b:nil, c:1, a:2, }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {b:nil, c:1, a:2, }
bind/0: caller_scope: nil
- 34 test_eval_handles_quoted_keyword_args2
bind/0: ('$var $val) <-> (x 1)
bind/0: ('$var $val)('$var $val) <-> (x 1)(x 1)
bind/0: skip keyword? (x 1)
bind/0: skipping ahead to (x 1)
bind/0: quoted $var
bind/0: positional arg
bind/0: $var: x
bind/0: regular arg
bind/0: ('$var $val)($val) <-> (x 1)(1)
bind/0: skip keyword? (1)
bind/0: skipping ahead to (1)
bind/0: regular $val
bind/0: after eval 1
bind/0: $val: 1
bind/0: ('$var $val)nil <-> (x 1)nil
bind/0: => {$var:x, $val:1, }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {$var:x, $val:1, }
bind/0: caller_scope: nil
bind/0: x: 1
bind/0: ($params ... $body) <-> ((a b 'c) c)
bind/0: ($params ... $body)($params ... $body) <-> ((a b 'c) c)((a b 'c) c)
bind/0: skip keyword? ((a b 'c) c)
bind/0: skipping ahead to ((a b 'c) c)
bind/0: quoted $params
bind/0: positional arg
bind/0: $params: (a b 'c)
bind/0: regular arg
bind/0: ($params ... $body)$body <-> ((a b 'c) c)(c)
bind/0: skip keyword? (c)
bind/0: skipping ahead to (c)
bind/0: quoted rest $body
bind/0: $body: (c)
bind/0: => {$params:(a b 'c), $body:(c), }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {$params:(a b 'c), $body:(c), }
bind/0: caller_scope: nil
bind/0: (a b 'c) <-> (:c x 2)
bind/0: (a b 'c)(a b 'c) <-> (:c x 2)(:c x 2)
bind/0: skip keyword? (:c x 2)
bind/0: match? c in (a b 'c)
bind/0: match? c in (b 'c)
bind/0: match? c in ('c)
bind/0: skipping keyword arg:c x
bind/0: skip keyword? (2)
bind/0: skipping ahead to (2)
bind/0: regular a
bind/0: after eval 2
bind/0: a: 2
bind/0: (a b 'c)(b 'c) <-> (:c x 2)nil
bind/0: skip keyword? nil
bind/0: skipping ahead to nil
bind/0: regular b
bind/0: after eval nil
bind/0: b: nil
bind/0: (a b 'c)('c) <-> (:c x 2)nil
bind/0: skip keyword? nil
bind/0: skipping ahead to nil
bind/0: quoted c
bind/0: keyword arg
bind/0: c: x
bind/0: (a b 'c)nil <-> (:c x 2)nil
bind/0: => {c:x, b:nil, a:2, }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {c:x, b:nil, a:2, }
bind/0: caller_scope: nil
- 35 test_eval_handles_rest_keyword_arg
bind/0: ($params ... $body) <-> ((a ... b) b)
bind/0: ($params ... $body)($params ... $body) <-> ((a ... b) b)((a ... b) b)
bind/0: skip keyword? ((a ... b) b)
bind/0: skipping ahead to ((a ... b) b)
bind/0: quoted $params
bind/0: positional arg
bind/0: $params: (a ... b)
bind/0: regular arg
bind/0: ($params ... $body)$body <-> ((a ... b) b)(b)
bind/0: skip keyword? (b)
bind/0: skipping ahead to (b)
bind/0: quoted rest $body
bind/0: $body: (b)
bind/0: => {$params:(a ... b), $body:(b), }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {$params:(a ... b), $body:(b), }
bind/0: caller_scope: nil
bind/0: (a ... b) <-> (2 :b 1 3)
bind/0: (a ... b)(a ... b) <-> (2 :b 1 3)(2 :b 1 3)
bind/0: skip keyword? (2 :b 1 3)
bind/0: skipping ahead to (2 :b 1 3)
bind/0: regular a
bind/0: after eval 2
bind/0: a: 2
bind/0: (a ... b)b <-> (2 :b 1 3)(:b 1 3)
bind/0: skip keyword? (:b 1 3)
bind/0: match? b in (a ... b)
bind/0: match? b in b
bind/0: skipping rest keyword args
bind/0: skip keyword? nil
bind/0: skipping ahead to nil
bind/0: rest b
bind/0: until next keyword after (:b 1 3)
bind/0: rest keyword (1 3)
bind/0: b: (1 3)
bind/0: => {a:2, b:(1 3), }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {a:2, b:(1 3), }
bind/0: caller_scope: nil
- 36 test_eval_handles_rest_keyword_arg2
bind/0: ($params ... $body) <-> ((a ... b) b)
bind/0: ($params ... $body)($params ... $body) <-> ((a ... b) b)((a ... b) b)
bind/0: skip keyword? ((a ... b) b)
bind/0: skipping ahead to ((a ... b) b)
bind/0: quoted $params
bind/0: positional arg
bind/0: $params: (a ... b)
bind/0: regular arg
bind/0: ($params ... $body)$body <-> ((a ... b) b)(b)
bind/0: skip keyword? (b)
bind/0: skipping ahead to (b)
bind/0: quoted rest $body
bind/0: $body: (b)
bind/0: => {$params:(a ... b), $body:(b), }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {$params:(a ... b), $body:(b), }
bind/0: caller_scope: nil
bind/0: (a ... b) <-> (:b 1 2 3)
bind/0: (a ... b)(a ... b) <-> (:b 1 2 3)(:b 1 2 3)
bind/0: skip keyword? (:b 1 2 3)
bind/0: match? b in (a ... b)
bind/0: match? b in b
bind/0: skipping rest keyword args
bind/0: skip keyword? nil
bind/0: skipping ahead to nil
bind/0: regular a
bind/0: after eval nil
bind/0: a: nil
bind/0: (a ... b)b <-> (:b 1 2 3)nil
bind/0: skip keyword? nil
bind/0: skipping ahead to nil
bind/0: rest b
bind/0: until next keyword after (:b 1 2 3)
bind/0: rest keyword (1 2 3)
bind/0: b: (1 2 3)
bind/0: => {a:nil, b:(1 2 3), }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {a:nil, b:(1 2 3), }
bind/0: caller_scope: nil
- 37 test_eval_handles_args_after_rest_keyword
bind/0: ($params ... $body) <-> ((a ... b) b)
bind/0: ($params ... $body)($params ... $body) <-> ((a ... b) b)((a ... b) b)
bind/0: skip keyword? ((a ... b) b)
bind/0: skipping ahead to ((a ... b) b)
bind/0: quoted $params
bind/0: positional arg
bind/0: $params: (a ... b)
bind/0: regular arg
bind/0: ($params ... $body)$body <-> ((a ... b) b)(b)
bind/0: skip keyword? (b)
bind/0: skipping ahead to (b)
bind/0: quoted rest $body
bind/0: $body: (b)
bind/0: => {$params:(a ... b), $body:(b), }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {$params:(a ... b), $body:(b), }
bind/0: caller_scope: nil
bind/0: (a ... b) <-> (:b 1 2 :a 3)
bind/0: (a ... b)(a ... b) <-> (:b 1 2 :a 3)(:b 1 2 :a 3)
bind/0: skip keyword? (:b 1 2 :a 3)
bind/0: match? b in (a ... b)
bind/0: match? b in b
bind/0: skipping rest keyword args
bind/0: at keyword sym (:a 3)
bind/0: scanning for a
bind/0: match? a in (a ... b)
bind/0: skip keyword? (:a 3)
bind/0: match? a in (a ... b)
bind/0: skipping keyword arg:a 3
bind/0: skip keyword? nil
bind/0: skipping ahead to nil
bind/0: regular a
bind/0: regular keyword arg 3
bind/0: after eval 3
bind/0: a: 3
bind/0: (a ... b)b <-> (:b 1 2 :a 3)nil
bind/0: skip keyword? nil
bind/0: skipping ahead to nil
bind/0: rest b
bind/0: until next keyword after (:b 1 2 :a 3)
bind/0: at keyword sym (:a 3)
bind/0: scanning for a
bind/0: match? a in (a ... b)
bind/0: rest keyword (1 2)
bind/0: b: (1 2)
bind/0: => {a:3, b:(1 2), }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {a:3, b:(1 2), }
bind/0: caller_scope: nil
- 38 test_eval_handles_args_after_rest_keyword2
bind/0: ($params ... $body) <-> ((a b ... c) b)
bind/0: ($params ... $body)($params ... $body) <-> ((a b ... c) b)((a b ... c) b)
bind/0: skip keyword? ((a b ... c) b)
bind/0: skipping ahead to ((a b ... c) b)
bind/0: quoted $params
bind/0: positional arg
bind/0: $params: (a b ... c)
bind/0: regular arg
bind/0: ($params ... $body)$body <-> ((a b ... c) b)(b)
bind/0: skip keyword? (b)
bind/0: skipping ahead to (b)
bind/0: quoted rest $body
bind/0: $body: (b)
bind/0: => {$params:(a b ... c), $body:(b), }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {$params:(a b ... c), $body:(b), }
bind/0: caller_scope: nil
bind/0: (a b ... c) <-> (:c 1 2 :b 3)
bind/0: (a b ... c)(a b ... c) <-> (:c 1 2 :b 3)(:c 1 2 :b 3)
bind/0: skip keyword? (:c 1 2 :b 3)
bind/0: match? c in (a b ... c)
bind/0: match? c in (b ... c)
bind/0: match? c in c
bind/0: skipping rest keyword args
bind/0: at keyword sym (:b 3)
bind/0: scanning for b
bind/0: match? b in (a b ... c)
bind/0: match? b in (b ... c)
bind/0: skip keyword? (:b 3)
bind/0: match? b in (a b ... c)
bind/0: match? b in (b ... c)
bind/0: skipping keyword arg:b 3
bind/0: skip keyword? nil
bind/0: skipping ahead to nil
bind/0: regular a
bind/0: after eval nil
bind/0: a: nil
bind/0: (a b ... c)(b ... c) <-> (:c 1 2 :b 3)nil
bind/0: skip keyword? nil
bind/0: skipping ahead to nil
bind/0: regular b
bind/0: regular keyword arg 3
bind/0: after eval 3
bind/0: b: 3
bind/0: (a b ... c)c <-> (:c 1 2 :b 3)nil
bind/0: skip keyword? nil
bind/0: skipping ahead to nil
bind/0: rest c
bind/0: until next keyword after (:c 1 2 :b 3)
bind/0: at keyword sym (:b 3)
bind/0: scanning for b
bind/0: match? b in (a b ... c)
bind/0: match? b in (b ... c)
bind/0: rest keyword (1 2)
bind/0: c: (1 2)
bind/0: => {a:nil, b:3, c:(1 2), }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {a:nil, b:3, c:(1 2), }
bind/0: caller_scope: nil
- 39 test_eval_handles_quoted_rest_keyword_arg
bind/0: ('$var $val) <-> (x 2)
bind/0: ('$var $val)('$var $val) <-> (x 2)(x 2)
bind/0: skip keyword? (x 2)
bind/0: skipping ahead to (x 2)
bind/0: quoted $var
bind/0: positional arg
bind/0: $var: x
bind/0: regular arg
bind/0: ('$var $val)($val) <-> (x 2)(2)
bind/0: skip keyword? (2)
bind/0: skipping ahead to (2)
bind/0: regular $val
bind/0: after eval 2
bind/0: $val: 2
bind/0: ('$var $val)nil <-> (x 2)nil
bind/0: => {$var:x, $val:2, }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {$var:x, $val:2, }
bind/0: caller_scope: nil
bind/0: x: 2
bind/0: ($params ... $body) <-> ((a ' ... b) b)
bind/0: ($params ... $body)($params ... $body) <-> ((a ' ... b) b)((a ' ... b) b)
bind/0: skip keyword? ((a ' ... b) b)
bind/0: skipping ahead to ((a ' ... b) b)
bind/0: quoted $params
bind/0: positional arg
bind/0: $params: (a ' ... b)
bind/0: regular arg
bind/0: ($params ... $body)$body <-> ((a ' ... b) b)(b)
bind/0: skip keyword? (b)
bind/0: skipping ahead to (b)
bind/0: quoted rest $body
bind/0: $body: (b)
bind/0: => {$params:(a ' ... b), $body:(b), }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {$params:(a ' ... b), $body:(b), }
bind/0: caller_scope: nil
bind/0: (a ' ... b) <-> (:b 1 x 3)
bind/0: (a ' ... b)(a ' ... b) <-> (:b 1 x 3)(:b 1 x 3)
bind/0: skip keyword? (:b 1 x 3)
bind/0: match? b in (a ' ... b)
bind/0: match? b in 'b
bind/0: skipping rest keyword args
bind/0: skip keyword? nil
bind/0: skipping ahead to nil
bind/0: regular a
bind/0: after eval nil
bind/0: a: nil
bind/0: (a ' ... b)'b <-> (:b 1 x 3)nil
bind/0: skip keyword? nil
bind/0: skipping ahead to nil
bind/0: quoted rest b
bind/0: quoted rest keyword (1 x 3)
bind/0: b: (1 x 3)
bind/0: => {a:nil, b:(1 x 3), }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {a:nil, b:(1 x 3), }
bind/0: caller_scope: nil
- 40 test_eval_handles_non_keyword_arg_colon_syms
bind/0: ($params ... $body) <-> ((a b) a)
bind/0: ($params ... $body)($params ... $body) <-> ((a b) a)((a b) a)
bind/0: skip keyword? ((a b) a)
bind/0: skipping ahead to ((a b) a)
bind/0: quoted $params
bind/0: positional arg
bind/0: $params: (a b)
bind/0: regular arg
bind/0: ($params ... $body)$body <-> ((a b) a)(a)
bind/0: skip keyword? (a)
bind/0: skipping ahead to (a)
bind/0: quoted rest $body
bind/0: $body: (a)
bind/0: => {$params:(a b), $body:(a), }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {$params:(a b), $body:(a), }
bind/0: caller_scope: nil
bind/0: (a b) <-> (:x 1)
bind/0: (a b)(a b) <-> (:x 1)(:x 1)
bind/0: skip keyword? (:x 1)
bind/0: match? x in (a b)
bind/0: match? x in (b)
bind/0: match? x in nil
bind/0: done skipping (:x 1)
bind/0: skipping ahead to (:x 1)
bind/0: regular a
bind/0: after eval :x
bind/0: a: :x
bind/0: (a b)(b) <-> (:x 1)(1)
bind/0: skip keyword? (1)
bind/0: skipping ahead to (1)
bind/0: regular b
bind/0: after eval 1
bind/0: b: 1
bind/0: (a b)nil <-> (:x 1)nil
bind/0: => {b:1, a::x, }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {b:1, a::x, }
bind/0: caller_scope: nil
- 41 test_eval_handles_keyword_args_inside_splice
bind/0: ($params ... $body) <-> ((a b) b)
bind/0: ($params ... $body)($params ... $body) <-> ((a b) b)((a b) b)
bind/0: skip keyword? ((a b) b)
bind/0: skipping ahead to ((a b) b)
bind/0: quoted $params
bind/0: positional arg
bind/0: $params: (a b)
bind/0: regular arg
bind/0: ($params ... $body)$body <-> ((a b) b)(b)
bind/0: skip keyword? (b)
bind/0: skipping ahead to (b)
bind/0: quoted rest $body
bind/0: $body: (b)
bind/0: => {$params:(a b), $body:(b), }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {$params:(a b), $body:(b), }
bind/0: caller_scope: nil
bind/0: (a b) <-> (''3 :a ''4)
bind/0: (a b)(a b) <-> (''3 :a ''4)(''3 :a ''4)
bind/0: skip keyword? (''3 :a ''4)
bind/0: skipping ahead to (''3 :a ''4)
bind/0: regular a
bind/0: regular keyword arg ''4
bind/0: after eval 4
bind/0: a: 4
bind/0: (a b)(b) <-> (''3 :a ''4)(''3 :a ''4)
bind/0: skip keyword? (''3 :a ''4)
bind/0: skipping ahead to (''3 :a ''4)
bind/0: regular b
bind/0: after eval 3
bind/0: b: 3
bind/0: (a b)nil <-> (''3 :a ''4)(:a ''4)
bind/0: => {a:4, b:3, }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {a:4, b:3, }
bind/0: caller_scope: nil
- 42 test_eval_handles_keyword_args_inside_destructured_params
bind/0: ($params ... $body) <-> (((a b)) b)
bind/0: ($params ... $body)($params ... $body) <-> (((a b)) b)(((a b)) b)
bind/0: skip keyword? (((a b)) b)
bind/0: skipping ahead to (((a b)) b)
bind/0: quoted $params
bind/0: positional arg
bind/0: $params: ((a b))
bind/0: regular arg
bind/0: ($params ... $body)$body <-> (((a b)) b)(b)
bind/0: skip keyword? (b)
bind/0: skipping ahead to (b)
bind/0: quoted rest $body
bind/0: $body: (b)
bind/0: => {$params:((a b)), $body:(b), }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {$params:((a b)), $body:(b), }
bind/0: caller_scope: nil
bind/0: ((a b)) <-> ('(3 :a 4))
bind/0: ((a b))((a b)) <-> ('(3 :a 4))('(3 :a 4))
bind/0: skip keyword? ('(3 :a 4))
bind/0: skipping ahead to ('(3 :a 4))
bind/0: destructured (a b)
bind/0: regular arg
bind/0: (a b) <-> (3 :a 4)
bind/0: (a b)(a b) <-> (3 :a 4)(3 :a 4)
bind/0: skip keyword? (3 :a 4)
bind/0: skipping ahead to (3 :a 4)
bind/0: quoted a
bind/0: keyword arg
bind/0: a: 4
bind/0: (a b)(b) <-> (3 :a 4)(3 :a 4)
bind/0: skip keyword? (3 :a 4)
bind/0: skipping ahead to (3 :a 4)
bind/0: quoted b
bind/0: positional arg
bind/0: b: 3
bind/0: regular arg
bind/0: (a b)nil <-> (3 :a 4)(:a 4)
bind/0: => {a:4, b:3, }
bind/0: ((a b))nil <-> ('(3 :a 4))nil
bind/0: => {a:4, b:3, }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {a:4, b:3, }
bind/0: caller_scope: nil
- 43 test_eval_handles_param_aliases
bind/0: ($params ... $body) <-> (((| x y)) 3)
bind/0: ($params ... $body)($params ... $body) <-> (((| x y)) 3)(((| x y)) 3)
bind/0: skip keyword? (((| x y)) 3)
bind/0: skipping ahead to (((| x y)) 3)
bind/0: quoted $params
bind/0: positional arg
bind/0: $params: ((| x y))
bind/0: regular arg
bind/0: ($params ... $body)$body <-> (((| x y)) 3)(3)
bind/0: skip keyword? (3)
bind/0: skipping ahead to (3)
bind/0: quoted rest $body
bind/0: $body: (3)
bind/0: => {$params:((| x y)), $body:(3), }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {$params:((| x y)), $body:(3), }
bind/0: caller_scope: nil
bind/0: ((| x y)) <-> (4)
bind/0: ((| x y))((| x y)) <-> (4)(4)
bind/0: skip keyword? (4)
bind/0: skipping ahead to (4)
bind/0: alias (| x y)
bind/0: positional arg
bind/0: alias sym x
bind/0: eval arg
bind/0: x: 4
bind/0: alias sym y
bind/0: y: 4
bind/0: ((| x y))nil <-> (4)nil
bind/0: => {x:4, y:4, }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {x:4, y:4, }
bind/0: caller_scope: nil
- 44 test_eval_handles_aliased_keyword_args
bind/0: ($params ... $body) <-> ((a b (| c x)) c)
bind/0: ($params ... $body)($params ... $body) <-> ((a b (| c x)) c)((a b (| c x)) c)
bind/0: skip keyword? ((a b (| c x)) c)
bind/0: skipping ahead to ((a b (| c x)) c)
bind/0: quoted $params
bind/0: positional arg
bind/0: $params: (a b (| c x))
bind/0: regular arg
bind/0: ($params ... $body)$body <-> ((a b (| c x)) c)(c)
bind/0: skip keyword? (c)
bind/0: skipping ahead to (c)
bind/0: quoted rest $body
bind/0: $body: (c)
bind/0: => {$params:(a b (| c x)), $body:(c), }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {$params:(a b (| c x)), $body:(c), }
bind/0: caller_scope: nil
bind/0: (a b (| c x)) <-> (:x 1 2)
bind/0: (a b (| c x))(a b (| c x)) <-> (:x 1 2)(:x 1 2)
bind/0: skip keyword? (:x 1 2)
bind/0: match? x in (a b (| c x))
bind/0: match? x in (b (| c x))
bind/0: match? x in ((| c x))
bind/0: match alias
bind/0: skipping keyword arg:x 1
bind/0: skip keyword? (2)
bind/0: skipping ahead to (2)
bind/0: regular a
bind/0: after eval 2
bind/0: a: 2
bind/0: (a b (| c x))(b (| c x)) <-> (:x 1 2)nil
bind/0: skip keyword? nil
bind/0: skipping ahead to nil
bind/0: regular b
bind/0: after eval nil
bind/0: b: nil
bind/0: (a b (| c x))((| c x)) <-> (:x 1 2)nil
bind/0: skip keyword? nil
bind/0: skipping ahead to nil
bind/0: alias (| c x)
bind/0: keyword arg
bind/0: alias sym c
bind/0: eval arg
bind/0: c: 1
bind/0: alias sym x
bind/0: x: 1
bind/0: (a b (| c x))nil <-> (:x 1 2)nil
bind/0: => {b:nil, x:1, a:2, c:1, }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {b:nil, x:1, a:2, c:1, }
bind/0: caller_scope: nil
- 45 test_eval_handles_aliased_keyword_args2
bind/0: ($params ... $body) <-> (((| a x) b) b)
bind/0: ($params ... $body)($params ... $body) <-> (((| a x) b) b)(((| a x) b) b)
bind/0: skip keyword? (((| a x) b) b)
bind/0: skipping ahead to (((| a x) b) b)
bind/0: quoted $params
bind/0: positional arg
bind/0: $params: ((| a x) b)
bind/0: regular arg
bind/0: ($params ... $body)$body <-> (((| a x) b) b)(b)
bind/0: skip keyword? (b)
bind/0: skipping ahead to (b)
bind/0: quoted rest $body
bind/0: $body: (b)
bind/0: => {$params:((| a x) b), $body:(b), }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {$params:((| a x) b), $body:(b), }
bind/0: caller_scope: nil
bind/0: ((| a x) b) <-> (:x 1 2)
bind/0: ((| a x) b)((| a x) b) <-> (:x 1 2)(:x 1 2)
bind/0: skip keyword? (:x 1 2)
bind/0: match? x in ((| a x) b)
bind/0: match alias
bind/0: skipping keyword arg:x 1
bind/0: skip keyword? (2)
bind/0: skipping ahead to (2)
bind/0: alias (| a x)
bind/0: keyword arg
bind/0: alias sym a
bind/0: eval arg
bind/0: a: 1
bind/0: alias sym x
bind/0: x: 1
bind/0: ((| a x) b)(b) <-> (:x 1 2)(2)
bind/0: skip keyword? (2)
bind/0: skipping ahead to (2)
bind/0: regular b
bind/0: after eval 2
bind/0: b: 2
bind/0: ((| a x) b)nil <-> (:x 1 2)nil
bind/0: => {x:1, b:2, a:1, }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {x:1, b:2, a:1, }
bind/0: caller_scope: nil
- 46 test_eval_handles_quoted_param_aliases
bind/0: x: 3
bind/0: ($params ... $body) <-> (((| a 'b)) 3)
bind/0: ($params ... $body)($params ... $body) <-> (((| a 'b)) 3)(((| a 'b)) 3)
bind/0: skip keyword? (((| a 'b)) 3)
bind/0: skipping ahead to (((| a 'b)) 3)
bind/0: quoted $params
bind/0: positional arg
bind/0: $params: ((| a 'b))
bind/0: regular arg
bind/0: ($params ... $body)$body <-> (((| a 'b)) 3)(3)
bind/0: skip keyword? (3)
bind/0: skipping ahead to (3)
bind/0: quoted rest $body
bind/0: $body: (3)
bind/0: => {$params:((| a 'b)), $body:(3), }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {$params:((| a 'b)), $body:(3), }
bind/0: caller_scope: nil
bind/0: ((| a 'b)) <-> (x)
bind/0: ((| a 'b))((| a 'b)) <-> (x)(x)
bind/0: skip keyword? (x)
bind/0: skipping ahead to (x)
bind/0: alias (| a 'b)
bind/0: positional arg
bind/0: alias sym a
bind/0: eval arg
bind/0: a: 3
bind/0: quoted alias 'b
bind/0: b: x
bind/0: ((| a 'b))nil <-> (x)nil
bind/0: => {b:x, a:3, }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {b:x, a:3, }
bind/0: caller_scope: nil
- 47 test_eval_handles_quoted_param_aliases2
bind/0: ($params ... $body) <-> ('((| x y)) 3)
bind/0: ($params ... $body)($params ... $body) <-> ('((| x y)) 3)('((| x y)) 3)
bind/0: skip keyword? ('((| x y)) 3)
bind/0: skipping ahead to ('((| x y)) 3)
bind/0: quoted $params
bind/0: positional arg
bind/0: $params: '((| x y))
bind/0: regular arg
bind/0: ($params ... $body)$body <-> ('((| x y)) 3)(3)
bind/0: skip keyword? (3)
bind/0: skipping ahead to (3)
bind/0: quoted rest $body
bind/0: $body: (3)
bind/0: => {$params:'((| x y)), $body:(3), }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {$params:'((| x y)), $body:(3), }
bind/0: caller_scope: nil
bind/0: ((| x y)) <-> (a)
bind/0: ((| x y))((| x y)) <-> (a)(a)
bind/0: skip keyword? (a)
bind/0: skipping ahead to (a)
bind/0: alias (| x y)
bind/0: positional arg
bind/0: quoted alias x
bind/0: x: a
bind/0: quoted alias y
bind/0: y: a
bind/0: ((| x y))nil <-> (a)nil
bind/0: => {x:a, y:a, }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {x:a, y:a, }
bind/0: caller_scope: nil
- 48 test_eval_handles_aliased_rest_keyword_args
bind/0: ($params ... $body) <-> ((a | body do) body)
bind/0: ($params ... $body)($params ... $body) <-> ((a | body do) body)((a | body do) body)
bind/0: skip keyword? ((a | body do) body)
bind/0: skipping ahead to ((a | body do) body)
bind/0: quoted $params
bind/0: positional arg
bind/0: $params: (a | body do)
bind/0: regular arg
bind/0: ($params ... $body)$body <-> ((a | body do) body)(body)
bind/0: skip keyword? (body)
bind/0: skipping ahead to (body)
bind/0: quoted rest $body
bind/0: $body: (body)
bind/0: => {$params:(a | body do), $body:(body), }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {$params:(a | body do), $body:(body), }
bind/0: caller_scope: nil
bind/0: (a | body do) <-> (2 :do 1 3)
bind/0: (a | body do)(a | body do) <-> (2 :do 1 3)(2 :do 1 3)
bind/0: skip keyword? (2 :do 1 3)
bind/0: skipping ahead to (2 :do 1 3)
bind/0: regular a
bind/0: after eval 2
bind/0: a: 2
bind/0: (a | body do)(| body do) <-> (2 :do 1 3)(:do 1 3)
bind/0: skip keyword? (:do 1 3)
bind/0: match? do in (a | body do)
bind/0: match? do in (| body do)
bind/0: match rest alias
bind/0: skipping rest keyword args
bind/0: skip keyword? nil
bind/0: skipping ahead to nil
bind/0: rest alias (| body do)
bind/0: until next keyword after (:do 1 3)
bind/0: rest alias keyword (1 3)
bind/0: rest alias sym body
bind/0: body: (1 3)
bind/0: do: (1 3)
bind/0: => {body:(1 3), do:(1 3), a:2, }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {body:(1 3), do:(1 3), a:2, }
bind/0: caller_scope: nil
- 49 test_eval_handles_aliased_rest_keyword_args2
bind/0: ($params ... $body) <-> ((a b | body do) `(,a ,b ,body))
bind/0: ($params ... $body)($params ... $body) <-> ((a b | body do) `(,a ,b ,body))((a b | body do) `(,a ,b ,body))
bind/0: skip keyword? ((a b | body do) `(,a ,b ,body))
bind/0: skipping ahead to ((a b | body do) `(,a ,b ,body))
bind/0: quoted $params
bind/0: positional arg
bind/0: $params: (a b | body do)
bind/0: regular arg
bind/0: ($params ... $body)$body <-> ((a b | body do) `(,a ,b ,body))(`(,a ,b ,body))
bind/0: skip keyword? (`(,a ,b ,body))
bind/0: skipping ahead to (`(,a ,b ,body))
bind/0: quoted rest $body
bind/0: $body: (`(,a ,b ,body))
bind/0: => {$params:(a b | body do), $body:(`(,a ,b ,body)), }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {$params:(a b | body do), $body:(`(,a ,b ,body)), }
bind/0: caller_scope: nil
bind/0: (a b | body do) <-> (2 :do 1 3)
bind/0: (a b | body do)(a b | body do) <-> (2 :do 1 3)(2 :do 1 3)
bind/0: skip keyword? (2 :do 1 3)
bind/0: skipping ahead to (2 :do 1 3)
bind/0: regular a
bind/0: after eval 2
bind/0: a: 2
bind/0: (a b | body do)(b | body do) <-> (2 :do 1 3)(:do 1 3)
bind/0: skip keyword? (:do 1 3)
bind/0: match? do in (a b | body do)
bind/0: match? do in (b | body do)
bind/0: match? do in (| body do)
bind/0: match rest alias
bind/0: skipping rest keyword args
bind/0: skip keyword? nil
bind/0: skipping ahead to nil
bind/0: regular b
bind/0: after eval nil
bind/0: b: nil
bind/0: (a b | body do)(| body do) <-> (2 :do 1 3)nil
bind/0: skip keyword? nil
bind/0: skipping ahead to nil
bind/0: rest alias (| body do)
bind/0: until next keyword after (:do 1 3)
bind/0: rest alias keyword (1 3)
bind/0: rest alias sym body
bind/0: body: (1 3)
bind/0: do: (1 3)
bind/0: => {body:(1 3), b:nil, a:2, do:(1 3), }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {body:(1 3), b:nil, a:2, do:(1 3), }
bind/0: caller_scope: nil
- 50 test_eval_handles_quoted_rest_param_aliases
bind/0: x: 3
bind/0: ($params ... $body) <-> ((| a 'b) 3)
bind/0: ($params ... $body)($params ... $body) <-> ((| a 'b) 3)((| a 'b) 3)
bind/0: skip keyword? ((| a 'b) 3)
bind/0: skipping ahead to ((| a 'b) 3)
bind/0: quoted $params
bind/0: positional arg
bind/0: $params: (| a 'b)
bind/0: regular arg
bind/0: ($params ... $body)$body <-> ((| a 'b) 3)(3)
bind/0: skip keyword? (3)
bind/0: skipping ahead to (3)
bind/0: quoted rest $body
bind/0: $body: (3)
bind/0: => {$params:(| a 'b), $body:(3), }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {$params:(| a 'b), $body:(3), }
bind/0: caller_scope: nil
bind/0: (| a 'b) <-> (x)
bind/0: (| a 'b)(| a 'b) <-> (x)(x)
bind/0: skip keyword? (x)
bind/0: skipping ahead to (x)
bind/0: rest alias (| a 'b)
bind/0: rest alias sym a
bind/0: a: (3)
bind/0: quoted rest alias 'b
bind/0: b: (x)
bind/0: => {b:(x), a:(3), }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {b:(x), a:(3), }
bind/0: caller_scope: nil
- 51 test_eval_handles_quoted_rest_param_aliases2
bind/0: ($params ... $body) <-> ('(| a b) 3)
bind/0: ($params ... $body)($params ... $body) <-> ('(| a b) 3)('(| a b) 3)
bind/0: skip keyword? ('(| a b) 3)
bind/0: skipping ahead to ('(| a b) 3)
bind/0: quoted $params
bind/0: positional arg
bind/0: $params: '(| a b)
bind/0: regular arg
bind/0: ($params ... $body)$body <-> ('(| a b) 3)(3)
bind/0: skip keyword? (3)
bind/0: skipping ahead to (3)
bind/0: quoted rest $body
bind/0: $body: (3)
bind/0: => {$params:'(| a b), $body:(3), }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {$params:'(| a b), $body:(3), }
bind/0: caller_scope: nil
bind/0: (| a b) <-> (x)
bind/0: (| a b)(| a b) <-> (x)(x)
bind/0: skip keyword? (x)
bind/0: skipping ahead to (x)
bind/0: rest alias (| a b)
bind/0: quoted rest alias sym a
bind/0: a: (x)
bind/0: b: (x)
bind/0: => {a:(x), b:(x), }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {a:(x), b:(x), }
bind/0: caller_scope: nil
- 52 test_eval_binds_as_params
bind/0: ($params ... $body) <-> ((| a (b c)) 3)
bind/0: ($params ... $body)($params ... $body) <-> ((| a (b c)) 3)((| a (b c)) 3)
bind/0: skip keyword? ((| a (b c)) 3)
bind/0: skipping ahead to ((| a (b c)) 3)
bind/0: quoted $params
bind/0: positional arg
bind/0: $params: (| a (b c))
bind/0: regular arg
bind/0: ($params ... $body)$body <-> ((| a (b c)) 3)(3)
bind/0: skip keyword? (3)
bind/0: skipping ahead to (3)
bind/0: quoted rest $body
bind/0: $body: (3)
bind/0: => {$params:(| a (b c)), $body:(3), }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {$params:(| a (b c)), $body:(3), }
bind/0: caller_scope: nil
bind/0: (| a (b c)) <-> (1 2)
bind/0: (| a (b c))(| a (b c)) <-> (1 2)(1 2)
bind/0: skip keyword? (1 2)
bind/0: skipping ahead to (1 2)
bind/0: rest alias (| a (b c))
bind/0: rest alias sym a
bind/0: a: (1 2)
bind/0: multiple rest aliases (b c)
bind/0: rest alias cons (as-param) (b c)
bind/0: (b c) <-> (1 2)
bind/0: (b c)(b c) <-> (1 2)(1 2)
bind/0: skip keyword? (1 2)
bind/0: skipping ahead to (1 2)
bind/0: regular b
bind/0: after eval 1
bind/0: b: 1
bind/0: (b c)(c) <-> (1 2)(2)
bind/0: skip keyword? (2)
bind/0: skipping ahead to (2)
bind/0: regular c
bind/0: after eval 2
bind/0: c: 2
bind/0: (b c)nil <-> (1 2)nil
bind/0: => {c:2, a:(1 2), b:1, }
bind/0: => {c:2, a:(1 2), b:1, }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {c:2, a:(1 2), b:1, }
bind/0: caller_scope: nil
- 53 test_eval_binds_as_params2
bind/0: x: (1 2)
bind/0: ($params ... $body) <-> (((| a (b c))) 3)
bind/0: ($params ... $body)($params ... $body) <-> (((| a (b c))) 3)(((| a (b c))) 3)
bind/0: skip keyword? (((| a (b c))) 3)
bind/0: skipping ahead to (((| a (b c))) 3)
bind/0: quoted $params
bind/0: positional arg
bind/0: $params: ((| a (b c)))
bind/0: regular arg
bind/0: ($params ... $body)$body <-> (((| a (b c))) 3)(3)
bind/0: skip keyword? (3)
bind/0: skipping ahead to (3)
bind/0: quoted rest $body
bind/0: $body: (3)
bind/0: => {$params:((| a (b c))), $body:(3), }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {$params:((| a (b c))), $body:(3), }
bind/0: caller_scope: nil
bind/0: ((| a (b c))) <-> (x)
bind/0: ((| a (b c)))((| a (b c))) <-> (x)(x)
bind/0: skip keyword? (x)
bind/0: skipping ahead to (x)
bind/0: alias (| a (b c))
bind/0: positional arg
bind/0: alias sym a
bind/0: eval arg
bind/0: a: (1 2)
bind/0: destructured alias (as-param) (b c)
bind/0: (b c) <-> (1 2)
bind/0: (b c)(b c) <-> (1 2)(1 2)
bind/0: skip keyword? (1 2)
bind/0: skipping ahead to (1 2)
bind/0: quoted b
bind/0: positional arg
bind/0: b: 1
bind/0: regular arg
bind/0: (b c)(c) <-> (1 2)(2)
bind/0: skip keyword? (2)
bind/0: skipping ahead to (2)
bind/0: quoted c
bind/0: positional arg
bind/0: c: 2
bind/0: regular arg
bind/0: (b c)nil <-> (1 2)nil
bind/0: => {b:1, c:2, a:(1 2), }
bind/0: ((| a (b c)))nil <-> (x)nil
bind/0: => {b:1, c:2, a:(1 2), }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {b:1, c:2, a:(1 2), }
bind/0: caller_scope: nil
- 54 test_eval_binds_as_params_with_keyword_args
bind/0: ($params ... $body) <-> ((| a (b c)) 3)
bind/0: ($params ... $body)($params ... $body) <-> ((| a (b c)) 3)((| a (b c)) 3)
bind/0: skip keyword? ((| a (b c)) 3)
bind/0: skipping ahead to ((| a (b c)) 3)
bind/0: quoted $params
bind/0: positional arg
bind/0: $params: (| a (b c))
bind/0: regular arg
bind/0: ($params ... $body)$body <-> ((| a (b c)) 3)(3)
bind/0: skip keyword? (3)
bind/0: skipping ahead to (3)
bind/0: quoted rest $body
bind/0: $body: (3)
bind/0: => {$params:(| a (b c)), $body:(3), }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {$params:(| a (b c)), $body:(3), }
bind/0: caller_scope: nil
bind/0: (| a (b c)) <-> (1 :b 2)
bind/0: (| a (b c))(| a (b c)) <-> (1 :b 2)(1 :b 2)
bind/0: skip keyword? (1 :b 2)
bind/0: skipping ahead to (1 :b 2)
bind/0: rest alias (| a (b c))
bind/0: rest alias sym a
bind/0: a: (1 :b 2)
bind/0: multiple rest aliases (b c)
bind/0: rest alias cons (as-param) (b c)
bind/0: (b c) <-> (1 :b 2)
bind/0: (b c)(b c) <-> (1 :b 2)(1 :b 2)
bind/0: skip keyword? (1 :b 2)
bind/0: skipping ahead to (1 :b 2)
bind/0: regular b
bind/0: regular keyword arg 2
bind/0: after eval 2
bind/0: b: 2
bind/0: (b c)(c) <-> (1 :b 2)(1 :b 2)
bind/0: skip keyword? (1 :b 2)
bind/0: skipping ahead to (1 :b 2)
bind/0: regular c
bind/0: after eval 1
bind/0: c: 1
bind/0: (b c)nil <-> (1 :b 2)(:b 2)
bind/0: => {b:2, c:1, a:(1 :b 2), }
bind/0: => {b:2, c:1, a:(1 :b 2), }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {b:2, c:1, a:(1 :b 2), }
bind/0: caller_scope: nil
- 55 test_eval_binds_as_params_with_keyword_args2
bind/0: ($params ... $body) <-> ((| a (b c d)) 3)
bind/0: ($params ... $body)($params ... $body) <-> ((| a (b c d)) 3)((| a (b c d)) 3)
bind/0: skip keyword? ((| a (b c d)) 3)
bind/0: skipping ahead to ((| a (b c d)) 3)
bind/0: quoted $params
bind/0: positional arg
bind/0: $params: (| a (b c d))
bind/0: regular arg
bind/0: ($params ... $body)$body <-> ((| a (b c d)) 3)(3)
bind/0: skip keyword? (3)
bind/0: skipping ahead to (3)
bind/0: quoted rest $body
bind/0: $body: (3)
bind/0: => {$params:(| a (b c d)), $body:(3), }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {$params:(| a (b c d)), $body:(3), }
bind/0: caller_scope: nil
bind/0: (| a (b c d)) <-> (1 :b 2 3)
bind/0: (| a (b c d))(| a (b c d)) <-> (1 :b 2 3)(1 :b 2 3)
bind/0: skip keyword? (1 :b 2 3)
bind/0: skipping ahead to (1 :b 2 3)
bind/0: rest alias (| a (b c d))
bind/0: rest alias sym a
bind/0: a: (1 :b 2 3)
bind/0: multiple rest aliases (b c d)
bind/0: rest alias cons (as-param) (b c d)
bind/0: (b c d) <-> (1 :b 2 3)
bind/0: (b c d)(b c d) <-> (1 :b 2 3)(1 :b 2 3)
bind/0: skip keyword? (1 :b 2 3)
bind/0: skipping ahead to (1 :b 2 3)
bind/0: regular b
bind/0: regular keyword arg 2
bind/0: after eval 2
bind/0: b: 2
bind/0: (b c d)(c d) <-> (1 :b 2 3)(1 :b 2 3)
bind/0: skip keyword? (1 :b 2 3)
bind/0: skipping ahead to (1 :b 2 3)
bind/0: regular c
bind/0: after eval 1
bind/0: c: 1
bind/0: (b c d)(d) <-> (1 :b 2 3)(:b 2 3)
bind/0: skip keyword? (:b 2 3)
bind/0: match? b in (b c d)
bind/0: skipping keyword arg:b 2
bind/0: skip keyword? (3)
bind/0: skipping ahead to (3)
bind/0: regular d
bind/0: after eval 3
bind/0: d: 3
bind/0: (b c d)nil <-> (1 :b 2 3)nil
bind/0: => {d:3, b:2, c:1, a:(1 :b 2 3), }
bind/0: => {d:3, b:2, c:1, a:(1 :b 2 3), }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {d:3, b:2, c:1, a:(1 :b 2 3), }
bind/0: caller_scope: nil
- 56 test_eval_warns_on_keyword_args_for_conflicting_aliases
bind/0: ($params ... $body) <-> (((| a b) (| c d)) (cons a (cons b (cons c d))))
bind/0: ($params ... $body)($params ... $body) <-> (((| a b) (| c d)) (cons a (cons b (cons c d))))(((| a b) (| c d)) (cons a (cons b (cons c d))))
bind/0: skip keyword? (((| a b) (| c d)) (cons a (cons b (cons c d))))
bind/0: skipping ahead to (((| a b) (| c d)) (cons a (cons b (cons c d))))
bind/0: quoted $params
bind/0: positional arg
bind/0: $params: ((| a b) (| c d))
bind/0: regular arg
bind/0: ($params ... $body)$body <-> (((| a b) (| c d)) (cons a (cons b (cons c d))))((cons a (cons b (cons c d))))
bind/0: skip keyword? ((cons a (cons b (cons c d))))
bind/0: skipping ahead to ((cons a (cons b (cons c d))))
bind/0: quoted rest $body
bind/0: $body: ((cons a (cons b (cons c d))))
bind/0: => {$params:((| a b) (| c d)), $body:((cons a (cons b (cons c d)))), }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {$params:((| a b) (| c d)), $body:((cons a (cons b (cons c d)))), }
bind/0: caller_scope: nil
bind/0: ((| a b) (| c d)) <-> (:c 1 :d 2)
bind/0: ((| a b) (| c d))((| a b) (| c d)) <-> (:c 1 :d 2)(:c 1 :d 2)
bind/0: skip keyword? (:c 1 :d 2)
bind/0: match? c in ((| a b) (| c d))
bind/0: match alias
bind/0: match? c in ((| c d))
bind/0: match alias
bind/0: skipping keyword arg:c 1
bind/0: skip keyword? (:d 2)
bind/0: match? d in ((| a b) (| c d))
bind/0: match alias
bind/0: match? d in ((| c d))
bind/0: match alias
bind/0: skipping keyword arg:d 2
bind/0: skip keyword? nil
bind/0: skipping ahead to nil
bind/0: alias (| a b)
bind/0: positional arg
bind/0: alias sym a
bind/0: eval arg
bind/0: a: nil
bind/0: alias sym b
bind/0: b: nil
bind/0: ((| a b) (| c d))((| c d)) <-> (:c 1 :d 2)nil
bind/0: skip keyword? nil
bind/0: skipping ahead to nil
bind/0: alias (| c d)
bind/0: keyword arg
bind/0: alias sym c
bind/0: eval arg
bind/0: c: 2
bind/0: alias sym d
bind/0: d: 2
bind/0: ((| a b) (| c d))nil <-> (:c 1 :d 2)nil
bind/0: => {a:nil, c:2, b:nil, d:2, }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {a:nil, c:2, b:nil, d:2, }
bind/0: caller_scope: nil
bind/0: ($x $y) <-> (a (cons b (cons c d)))
bind/0: ($x $y)($x $y) <-> (a (cons b (cons c d)))(a (cons b (cons c d)))
bind/0: skip keyword? (a (cons b (cons c d)))
bind/0: skipping ahead to (a (cons b (cons c d)))
bind/0: regular $x
bind/0: after eval nil
bind/0: $x: nil
bind/0: ($x $y)($y) <-> (a (cons b (cons c d)))((cons b (cons c d)))
bind/0: skip keyword? ((cons b (cons c d)))
bind/0: skipping ahead to ((cons b (cons c d)))
bind/0: regular $y
bind/0: ($x $y) <-> (b (cons c d))
bind/0: ($x $y)($x $y) <-> (b (cons c d))(b (cons c d))
bind/0: skip keyword? (b (cons c d))
bind/0: skipping ahead to (b (cons c d))
bind/0: regular $x
bind/0: after eval nil
bind/0: $x: nil
bind/0: ($x $y)($y) <-> (b (cons c d))((cons c d))
bind/0: skip keyword? ((cons c d))
bind/0: skipping ahead to ((cons c d))
bind/0: regular $y
bind/0: ($x $y) <-> (c d)
bind/0: ($x $y)($x $y) <-> (c d)(c d)
bind/0: skip keyword? (c d)
bind/0: skipping ahead to (c d)
bind/0: regular $x
bind/0: after eval 2
bind/0: $x: 2
bind/0: ($x $y)($y) <-> (c d)(d)
bind/0: skip keyword? (d)
bind/0: skipping ahead to (d)
bind/0: regular $y
bind/0: after eval 2
bind/0: $y: 2
bind/0: ($x $y)nil <-> (c d)nil
bind/0: => {$y:2, $x:2, }
bind/0: Curr_lexical_scope: {a:nil, c:2, caller_scope:nil, b:nil, d:2, }
bind/0: Curr_lexical_scope: {$y:2, $x:2, }->{a:nil, c:2, caller_scope:nil, b:nil, d:2, }
bind/0: caller_scope: {a:nil, c:2, caller_scope:nil, b:nil, d:2, }
bind/0: after eval (2 ... 2)
bind/0: $y: (2 ... 2)
bind/0: ($x $y)nil <-> (b (cons c d))nil
bind/0: => {$y:(2 ... 2), $x:nil, }
bind/0: Curr_lexical_scope: {a:nil, c:2, caller_scope:nil, b:nil, d:2, }
bind/0: Curr_lexical_scope: {$y:(2 ... 2), $x:nil, }->{a:nil, c:2, caller_scope:nil, b:nil, d:2, }
bind/0: caller_scope: {a:nil, c:2, caller_scope:nil, b:nil, d:2, }
bind/0: after eval (nil 2 ... 2)
bind/0: $y: (nil 2 ... 2)
bind/0: ($x $y)nil <-> (a (cons b (cons c d)))nil
bind/0: => {$y:(nil 2 ... 2), $x:nil, }
bind/0: Curr_lexical_scope: {a:nil, c:2, caller_scope:nil, b:nil, d:2, }
bind/0: Curr_lexical_scope: {$y:(nil 2 ... 2), $x:nil, }->{a:nil, c:2, caller_scope:nil, b:nil, d:2, }
bind/0: caller_scope: {a:nil, c:2, caller_scope:nil, b:nil, d:2, }
- 57 test_eval_binds_as_params_recursively
bind/0: ($params ... $body) <-> ((| a (b | c (d e))) 3)
bind/0: ($params ... $body)($params ... $body) <-> ((| a (b | c (d e))) 3)((| a (b | c (d e))) 3)
bind/0: skip keyword? ((| a (b | c (d e))) 3)
bind/0: skipping ahead to ((| a (b | c (d e))) 3)
bind/0: quoted $params
bind/0: positional arg
bind/0: $params: (| a (b | c (d e)))
bind/0: regular arg
bind/0: ($params ... $body)$body <-> ((| a (b | c (d e))) 3)(3)
bind/0: skip keyword? (3)
bind/0: skipping ahead to (3)
bind/0: quoted rest $body
bind/0: $body: (3)
bind/0: => {$params:(| a (b | c (d e))), $body:(3), }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {$params:(| a (b | c (d e))), $body:(3), }
bind/0: caller_scope: nil
bind/0: (| a (b | c (d e))) <-> (1 2 3)
bind/0: (| a (b | c (d e)))(| a (b | c (d e))) <-> (1 2 3)(1 2 3)
bind/0: skip keyword? (1 2 3)
bind/0: skipping ahead to (1 2 3)
bind/0: rest alias (| a (b | c (d e)))
bind/0: rest alias sym a
bind/0: a: (1 2 3)
bind/0: multiple rest aliases (b | c (d e))
bind/0: rest alias cons (as-param) (b | c (d e))
bind/0: (b | c (d e)) <-> (1 2 3)
bind/0: (b | c (d e))(b | c (d e)) <-> (1 2 3)(1 2 3)
bind/0: skip keyword? (1 2 3)
bind/0: skipping ahead to (1 2 3)
bind/0: regular b
bind/0: after eval 1
bind/0: b: 1
bind/0: (b | c (d e))(| c (d e)) <-> (1 2 3)(2 3)
bind/0: skip keyword? (2 3)
bind/0: skipping ahead to (2 3)
bind/0: rest alias (| c (d e))
bind/0: rest alias sym c
bind/0: c: (2 3)
bind/0: multiple rest aliases (d e)
bind/0: rest alias cons (as-param) (d e)
bind/0: (d e) <-> (2 3)
bind/0: (d e)(d e) <-> (2 3)(2 3)
bind/0: skip keyword? (2 3)
bind/0: skipping ahead to (2 3)
bind/0: regular d
bind/0: after eval 2
bind/0: d: 2
bind/0: (d e)(e) <-> (2 3)(3)
bind/0: skip keyword? (3)
bind/0: skipping ahead to (3)
bind/0: regular e
bind/0: after eval 3
bind/0: e: 3
bind/0: (d e)nil <-> (2 3)nil
bind/0: => {e:3, a:(1 2 3), c:(2 3), b:1, d:2, }
bind/0: => {e:3, a:(1 2 3), c:(2 3), b:1, d:2, }
bind/0: => {e:3, a:(1 2 3), c:(2 3), b:1, d:2, }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {e:3, a:(1 2 3), c:(2 3), b:1, d:2, }
bind/0: caller_scope: nil
- 58 test_eval_binds_as_params_recursively_using_keyword_args
bind/0: ($params ... $body) <-> ((| a (b | c (d e))) 3)
bind/0: ($params ... $body)($params ... $body) <-> ((| a (b | c (d e))) 3)((| a (b | c (d e))) 3)
bind/0: skip keyword? ((| a (b | c (d e))) 3)
bind/0: skipping ahead to ((| a (b | c (d e))) 3)
bind/0: quoted $params
bind/0: positional arg
bind/0: $params: (| a (b | c (d e)))
bind/0: regular arg
bind/0: ($params ... $body)$body <-> ((| a (b | c (d e))) 3)(3)
bind/0: skip keyword? (3)
bind/0: skipping ahead to (3)
bind/0: quoted rest $body
bind/0: $body: (3)
bind/0: => {$params:(| a (b | c (d e))), $body:(3), }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {$params:(| a (b | c (d e))), $body:(3), }
bind/0: caller_scope: nil
bind/0: (| a (b | c (d e))) <-> (1 :e 2 3)
bind/0: (| a (b | c (d e)))(| a (b | c (d e))) <-> (1 :e 2 3)(1 :e 2 3)
bind/0: skip keyword? (1 :e 2 3)
bind/0: skipping ahead to (1 :e 2 3)
bind/0: rest alias (| a (b | c (d e)))
bind/0: rest alias sym a
bind/0: a: (1 :e 2 3)
bind/0: multiple rest aliases (b | c (d e))
bind/0: rest alias cons (as-param) (b | c (d e))
bind/0: (b | c (d e)) <-> (1 :e 2 3)
bind/0: (b | c (d e))(b | c (d e)) <-> (1 :e 2 3)(1 :e 2 3)
bind/0: skip keyword? (1 :e 2 3)
bind/0: skipping ahead to (1 :e 2 3)
bind/0: regular b
bind/0: after eval 1
bind/0: b: 1
bind/0: (b | c (d e))(| c (d e)) <-> (1 :e 2 3)(:e 2 3)
bind/0: skip keyword? (:e 2 3)
bind/0: match? e in (b | c (d e))
bind/0: match? e in (| c (d e))
bind/0: match rest alias
bind/0: match? e in (c (d e))
bind/0: match? e in ((d e))
bind/0: match? e in nil
bind/0: done skipping (:e 2 3)
bind/0: skipping ahead to (:e 2 3)
bind/0: rest alias (| c (d e))
bind/0: rest alias sym c
bind/0: c: (:e 2 3)
bind/0: multiple rest aliases (d e)
bind/0: rest alias cons (as-param) (d e)
bind/0: (d e) <-> (:e 2 3)
bind/0: (d e)(d e) <-> (:e 2 3)(:e 2 3)
bind/0: skip keyword? (:e 2 3)
bind/0: match? e in (d e)
bind/0: match? e in (e)
bind/0: skipping keyword arg:e 2
bind/0: skip keyword? (3)
bind/0: skipping ahead to (3)
bind/0: regular d
bind/0: after eval 3
bind/0: d: 3
bind/0: (d e)(e) <-> (:e 2 3)nil
bind/0: skip keyword? nil
bind/0: skipping ahead to nil
bind/0: regular e
bind/0: regular keyword arg 2
bind/0: after eval 2
bind/0: e: 2
bind/0: (d e)nil <-> (:e 2 3)nil
bind/0: => {b:1, d:3, e:2, a:(1 :e 2 3), c:(:e 2 3), }
bind/0: => {b:1, d:3, e:2, a:(1 :e 2 3), c:(:e 2 3), }
bind/0: => {b:1, d:3, e:2, a:(1 :e 2 3), c:(:e 2 3), }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {b:1, d:3, e:2, a:(1 :e 2 3), c:(:e 2 3), }
bind/0: caller_scope: nil
- 59 test_eval_binds_quoted_as_params_recursively_using_keyword_args
bind/0: ($params ... $body) <-> ((| 'a ((| b c))) 3)
bind/0: ($params ... $body)($params ... $body) <-> ((| 'a ((| b c))) 3)((| 'a ((| b c))) 3)
bind/0: skip keyword? ((| 'a ((| b c))) 3)
bind/0: skipping ahead to ((| 'a ((| b c))) 3)
bind/0: quoted $params
bind/0: positional arg
bind/0: $params: (| 'a ((| b c)))
bind/0: regular arg
bind/0: ($params ... $body)$body <-> ((| 'a ((| b c))) 3)(3)
bind/0: skip keyword? (3)
bind/0: skipping ahead to (3)
bind/0: quoted rest $body
bind/0: $body: (3)
bind/0: => {$params:(| 'a ((| b c))), $body:(3), }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {$params:(| 'a ((| b c))), $body:(3), }
bind/0: caller_scope: nil
bind/0: (| 'a ((| b c))) <-> (:b 1)
bind/0: (| 'a ((| b c)))(| 'a ((| b c))) <-> (:b 1)(:b 1)
bind/0: skip keyword? (:b 1)
bind/0: match? b in (| 'a ((| b c)))
bind/0: match rest alias
bind/0: match? b in ('a ((| b c)))
bind/0: match? b in (((| b c)))
bind/0: match? b in nil
bind/0: done skipping (:b 1)
bind/0: skipping ahead to (:b 1)
bind/0: rest alias (| 'a ((| b c)))
bind/0: quoted rest alias 'a
bind/0: a: (:b 1)
bind/0: multiple rest aliases ((| b c))
bind/0: rest alias cons (as-param) ((| b c))
bind/0: ((| b c)) <-> (:b 1)
bind/0: ((| b c))((| b c)) <-> (:b 1)(:b 1)
bind/0: skip keyword? (:b 1)
bind/0: match? b in ((| b c))
bind/0: match alias
bind/0: skipping keyword arg:b 1
bind/0: skip keyword? nil
bind/0: skipping ahead to nil
bind/0: alias (| b c)
bind/0: keyword arg
bind/0: alias sym b
bind/0: eval arg
bind/0: b: 1
bind/0: alias sym c
bind/0: c: 1
bind/0: ((| b c))nil <-> (:b 1)nil
bind/0: => {c:1, b:1, a:(:b 1), }
bind/0: => {c:1, b:1, a:(:b 1), }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {c:1, b:1, a:(:b 1), }
bind/0: caller_scope: nil
- 60 test_eval_binds_quoted_rest_as_params_recursively_using_keyword_args
bind/0: ($params ... $body) <-> ((| 'a (b | c (d e))) 3)
bind/0: ($params ... $body)($params ... $body) <-> ((| 'a (b | c (d e))) 3)((| 'a (b | c (d e))) 3)
bind/0: skip keyword? ((| 'a (b | c (d e))) 3)
bind/0: skipping ahead to ((| 'a (b | c (d e))) 3)
bind/0: quoted $params
bind/0: positional arg
bind/0: $params: (| 'a (b | c (d e)))
bind/0: regular arg
bind/0: ($params ... $body)$body <-> ((| 'a (b | c (d e))) 3)(3)
bind/0: skip keyword? (3)
bind/0: skipping ahead to (3)
bind/0: quoted rest $body
bind/0: $body: (3)
bind/0: => {$params:(| 'a (b | c (d e))), $body:(3), }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {$params:(| 'a (b | c (d e))), $body:(3), }
bind/0: caller_scope: nil
bind/0: (| 'a (b | c (d e))) <-> (1 :e 2 3)
bind/0: (| 'a (b | c (d e)))(| 'a (b | c (d e))) <-> (1 :e 2 3)(1 :e 2 3)
bind/0: skip keyword? (1 :e 2 3)
bind/0: skipping ahead to (1 :e 2 3)
bind/0: rest alias (| 'a (b | c (d e)))
bind/0: quoted rest alias 'a
bind/0: a: (1 :e 2 3)
bind/0: multiple rest aliases (b | c (d e))
bind/0: rest alias cons (as-param) (b | c (d e))
bind/0: (b | c (d e)) <-> (1 :e 2 3)
bind/0: (b | c (d e))(b | c (d e)) <-> (1 :e 2 3)(1 :e 2 3)
bind/0: skip keyword? (1 :e 2 3)
bind/0: skipping ahead to (1 :e 2 3)
bind/0: regular b
bind/0: after eval 1
bind/0: b: 1
bind/0: (b | c (d e))(| c (d e)) <-> (1 :e 2 3)(:e 2 3)
bind/0: skip keyword? (:e 2 3)
bind/0: match? e in (b | c (d e))
bind/0: match? e in (| c (d e))
bind/0: match rest alias
bind/0: match? e in (c (d e))
bind/0: match? e in ((d e))
bind/0: match? e in nil
bind/0: done skipping (:e 2 3)
bind/0: skipping ahead to (:e 2 3)
bind/0: rest alias (| c (d e))
bind/0: rest alias sym c
bind/0: c: (:e 2 3)
bind/0: multiple rest aliases (d e)
bind/0: rest alias cons (as-param) (d e)
bind/0: (d e) <-> (:e 2 3)
bind/0: (d e)(d e) <-> (:e 2 3)(:e 2 3)
bind/0: skip keyword? (:e 2 3)
bind/0: match? e in (d e)
bind/0: match? e in (e)
bind/0: skipping keyword arg:e 2
bind/0: skip keyword? (3)
bind/0: skipping ahead to (3)
bind/0: regular d
bind/0: after eval 3
bind/0: d: 3
bind/0: (d e)(e) <-> (:e 2 3)nil
bind/0: skip keyword? nil
bind/0: skipping ahead to nil
bind/0: regular e
bind/0: regular keyword arg 2
bind/0: after eval 2
bind/0: e: 2
bind/0: (d e)nil <-> (:e 2 3)nil
bind/0: => {b:1, d:3, e:2, a:(1 :e 2 3), c:(:e 2 3), }
bind/0: => {b:1, d:3, e:2, a:(1 :e 2 3), c:(:e 2 3), }
bind/0: => {b:1, d:3, e:2, a:(1 :e 2 3), c:(:e 2 3), }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {b:1, d:3, e:2, a:(1 :e 2 3), c:(:e 2 3), }
bind/0: caller_scope: nil
- 61 test_eval_binds_quoted_as_params
bind/0: x: 3
bind/0: ($params ... $body) <-> ((| 'a ('b)) 3)
bind/0: ($params ... $body)($params ... $body) <-> ((| 'a ('b)) 3)((| 'a ('b)) 3)
bind/0: skip keyword? ((| 'a ('b)) 3)
bind/0: skipping ahead to ((| 'a ('b)) 3)
bind/0: quoted $params
bind/0: positional arg
bind/0: $params: (| 'a ('b))
bind/0: regular arg
bind/0: ($params ... $body)$body <-> ((| 'a ('b)) 3)(3)
bind/0: skip keyword? (3)
bind/0: skipping ahead to (3)
bind/0: quoted rest $body
bind/0: $body: (3)
bind/0: => {$params:(| 'a ('b)), $body:(3), }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {$params:(| 'a ('b)), $body:(3), }
bind/0: caller_scope: nil
bind/0: (| 'a ('b)) <-> (x)
bind/0: (| 'a ('b))(| 'a ('b)) <-> (x)(x)
bind/0: skip keyword? (x)
bind/0: skipping ahead to (x)
bind/0: rest alias (| 'a ('b))
bind/0: quoted rest alias 'a
bind/0: a: (x)
bind/0: multiple rest aliases ('b)
bind/0: rest alias cons (as-param) ('b)
bind/0: ('b) <-> (x)
bind/0: ('b)('b) <-> (x)(x)
bind/0: skip keyword? (x)
bind/0: skipping ahead to (x)
bind/0: quoted b
bind/0: positional arg
bind/0: b: x
bind/0: regular arg
bind/0: ('b)nil <-> (x)nil
bind/0: => {a:(x), b:x, }
bind/0: => {a:(x), b:x, }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {a:(x), b:x, }
bind/0: caller_scope: nil
- 62 test_eval_binds_quoted_as_params2
bind/0: x: 3
bind/0: ($params ... $body) <-> ((| a ('b)) 3)
bind/0: ($params ... $body)($params ... $body) <-> ((| a ('b)) 3)((| a ('b)) 3)
bind/0: skip keyword? ((| a ('b)) 3)
bind/0: skipping ahead to ((| a ('b)) 3)
bind/0: quoted $params
bind/0: positional arg
bind/0: $params: (| a ('b))
bind/0: regular arg
bind/0: ($params ... $body)$body <-> ((| a ('b)) 3)(3)
bind/0: skip keyword? (3)
bind/0: skipping ahead to (3)
bind/0: quoted rest $body
bind/0: $body: (3)
bind/0: => {$params:(| a ('b)), $body:(3), }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {$params:(| a ('b)), $body:(3), }
bind/0: caller_scope: nil
bind/0: (| a ('b)) <-> (x)
bind/0: (| a ('b))(| a ('b)) <-> (x)(x)
bind/0: skip keyword? (x)
bind/0: skipping ahead to (x)
bind/0: rest alias (| a ('b))
bind/0: rest alias sym a
bind/0: a: (3)
bind/0: multiple rest aliases ('b)
bind/0: rest alias cons (as-param) ('b)
bind/0: ('b) <-> (x)
bind/0: ('b)('b) <-> (x)(x)
bind/0: skip keyword? (x)
bind/0: skipping ahead to (x)
bind/0: quoted b
bind/0: positional arg
bind/0: b: x
bind/0: regular arg
bind/0: ('b)nil <-> (x)nil
bind/0: => {b:x, a:(3), }
bind/0: => {b:x, a:(3), }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {b:x, a:(3), }
bind/0: caller_scope: nil
- 63 test_eval_binds_destructured_as_params
bind/0: ($params ... $body) <-> (((| a (b c))) 3)
bind/0: ($params ... $body)($params ... $body) <-> (((| a (b c))) 3)(((| a (b c))) 3)
bind/0: skip keyword? (((| a (b c))) 3)
bind/0: skipping ahead to (((| a (b c))) 3)
bind/0: quoted $params
bind/0: positional arg
bind/0: $params: ((| a (b c)))
bind/0: regular arg
bind/0: ($params ... $body)$body <-> (((| a (b c))) 3)(3)
bind/0: skip keyword? (3)
bind/0: skipping ahead to (3)
bind/0: quoted rest $body
bind/0: $body: (3)
bind/0: => {$params:((| a (b c))), $body:(3), }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {$params:((| a (b c))), $body:(3), }
bind/0: caller_scope: nil
bind/0: ((| a (b c))) <-> ('(1 2))
bind/0: ((| a (b c)))((| a (b c))) <-> ('(1 2))('(1 2))
bind/0: skip keyword? ('(1 2))
bind/0: skipping ahead to ('(1 2))
bind/0: alias (| a (b c))
bind/0: positional arg
bind/0: alias sym a
bind/0: eval arg
bind/0: a: (1 2)
bind/0: destructured alias (as-param) (b c)
bind/0: (b c) <-> (1 2)
bind/0: (b c)(b c) <-> (1 2)(1 2)
bind/0: skip keyword? (1 2)
bind/0: skipping ahead to (1 2)
bind/0: quoted b
bind/0: positional arg
bind/0: b: 1
bind/0: regular arg
bind/0: (b c)(c) <-> (1 2)(2)
bind/0: skip keyword? (2)
bind/0: skipping ahead to (2)
bind/0: quoted c
bind/0: positional arg
bind/0: c: 2
bind/0: regular arg
bind/0: (b c)nil <-> (1 2)nil
bind/0: => {b:1, c:2, a:(1 2), }
bind/0: ((| a (b c)))nil <-> ('(1 2))nil
bind/0: => {b:1, c:2, a:(1 2), }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {b:1, c:2, a:(1 2), }
bind/0: caller_scope: nil
- 64 test_eval_warns_on_unary_as
bind/0: ($params ... $body) <-> ((| a) 3)
bind/0: ($params ... $body)($params ... $body) <-> ((| a) 3)((| a) 3)
bind/0: skip keyword? ((| a) 3)
bind/0: skipping ahead to ((| a) 3)
bind/0: quoted $params
bind/0: positional arg
bind/0: $params: (| a)
bind/0: regular arg
bind/0: ($params ... $body)$body <-> ((| a) 3)(3)
bind/0: skip keyword? (3)
bind/0: skipping ahead to (3)
bind/0: quoted rest $body
bind/0: $body: (3)
bind/0: => {$params:(| a), $body:(3), }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {$params:(| a), $body:(3), }
bind/0: caller_scope: nil
bind/0: (| a) <-> (1 2)
bind/0: (| a)(| a) <-> (1 2)(1 2)
bind/0: skip keyword? (1 2)
bind/0: skipping ahead to (1 2)
bind/0: rest alias (| a)
bind/0: rest alias sym a
bind/0: a: (1 2)
bind/0: => {a:(1 2), }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {a:(1 2), }
bind/0: caller_scope: nil
- 65 test_eval_warns_on_unary_as2
bind/0: ($params ... $body) <-> (((| a)) 3)
bind/0: ($params ... $body)($params ... $body) <-> (((| a)) 3)(((| a)) 3)
bind/0: skip keyword? (((| a)) 3)
bind/0: skipping ahead to (((| a)) 3)
bind/0: quoted $params
bind/0: positional arg
bind/0: $params: ((| a))
bind/0: regular arg
bind/0: ($params ... $body)$body <-> (((| a)) 3)(3)
bind/0: skip keyword? (3)
bind/0: skipping ahead to (3)
bind/0: quoted rest $body
bind/0: $body: (3)
bind/0: => {$params:((| a)), $body:(3), }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {$params:((| a)), $body:(3), }
bind/0: caller_scope: nil
bind/0: ((| a)) <-> (1 2)
bind/0: ((| a))((| a)) <-> (1 2)(1 2)
bind/0: skip keyword? (1 2)
bind/0: skipping ahead to (1 2)
bind/0: alias (| a)
bind/0: positional arg
bind/0: alias sym a
bind/0: eval arg
bind/0: a: 1
bind/0: ((| a))nil <-> (1 2)(2)
bind/0: => {a:1, }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {a:1, }
bind/0: caller_scope: nil
- 66 test_eval_warns_on_conflicting_as_params
bind/0: ($params ... $body) <-> ((| (a b) c))
bind/0: ($params ... $body)($params ... $body) <-> ((| (a b) c))((| (a b) c))
bind/0: skip keyword? ((| (a b) c))
bind/0: skipping ahead to ((| (a b) c))
bind/0: quoted $params
bind/0: positional arg
bind/0: $params: (| (a b) c)
bind/0: regular arg
bind/0: ($params ... $body)$body <-> ((| (a b) c))nil
bind/0: skip keyword? nil
bind/0: skipping ahead to nil
bind/0: quoted rest $body
bind/0: $body: nil
bind/0: => {$params:(| (a b) c), $body:nil, }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {$params:(| (a b) c), $body:nil, }
bind/0: caller_scope: nil
bind/0: (| (a b) c) <-> (1 2)
bind/0: (| (a b) c)(| (a b) c) <-> (1 2)(1 2)
bind/0: skip keyword? (1 2)
bind/0: skipping ahead to (1 2)
bind/0: rest alias (| (a b) c)
bind/0: rest alias sym c
bind/0: c: (1 2)
bind/0: => {c:(1 2), }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {c:(1 2), }
bind/0: caller_scope: nil
- 67 test_eval_warns_on_double_quoting
bind/0: ($params ... $body) <-> ((''a))
bind/0: ($params ... $body)($params ... $body) <-> ((''a))((''a))
bind/0: skip keyword? ((''a))
bind/0: skipping ahead to ((''a))
bind/0: quoted $params
bind/0: positional arg
bind/0: $params: (''a)
bind/0: regular arg
bind/0: ($params ... $body)$body <-> ((''a))nil
bind/0: skip keyword? nil
bind/0: skipping ahead to nil
bind/0: quoted rest $body
bind/0: $body: nil
bind/0: => {$params:(''a), $body:nil, }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {$params:(''a), $body:nil, }
bind/0: caller_scope: nil
bind/0: (''a) <-> (1)
bind/0: (''a)(''a) <-> (1)(1)
bind/0: skip keyword? (1)
bind/0: skipping ahead to (1)
bind/0: quoted destructured 'a
bind/0: 'a <-> 1
bind/0: a: 1
bind/0: => {a:1, }
bind/0: (''a)nil <-> (1)nil
bind/0: => {a:1, }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {a:1, }
bind/0: caller_scope: nil
- 68 test_eval_warns_on_double_quoting2
bind/0: ($params ... $body) <-> (''a)
bind/0: ($params ... $body)($params ... $body) <-> (''a)(''a)
bind/0: skip keyword? (''a)
bind/0: skipping ahead to (''a)
bind/0: quoted $params
bind/0: positional arg
bind/0: $params: ''a
bind/0: regular arg
bind/0: ($params ... $body)$body <-> (''a)nil
bind/0: skip keyword? nil
bind/0: skipping ahead to nil
bind/0: quoted rest $body
bind/0: $body: nil
bind/0: => {$params:''a, $body:nil, }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {$params:''a, $body:nil, }
bind/0: caller_scope: nil
bind/0: 'a <-> (1)
bind/0: 'a'a <-> (1)(1)
bind/0: skip keyword? (1)
bind/0: skipping ahead to (1)
bind/0: => {}
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {}
bind/0: caller_scope: nil
- 69 test_eval_warns_on_double_quoting3
bind/0: ($params ... $body) <-> ('('a))
bind/0: ($params ... $body)($params ... $body) <-> ('('a))('('a))
bind/0: skip keyword? ('('a))
bind/0: skipping ahead to ('('a))
bind/0: quoted $params
bind/0: positional arg
bind/0: $params: '('a)
bind/0: regular arg
bind/0: ($params ... $body)$body <-> ('('a))nil
bind/0: skip keyword? nil
bind/0: skipping ahead to nil
bind/0: quoted rest $body
bind/0: $body: nil
bind/0: => {$params:'('a), $body:nil, }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {$params:'('a), $body:nil, }
bind/0: caller_scope: nil
bind/0: ('a) <-> (1)
bind/0: ('a)('a) <-> (1)(1)
bind/0: skip keyword? (1)
bind/0: skipping ahead to (1)
bind/0: => {}
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {}
bind/0: caller_scope: nil
- 70 test_eval_warns_on_double_quoting4
bind/0: ($params ... $body) <-> ('(a 'b))
bind/0: ($params ... $body)($params ... $body) <-> ('(a 'b))('(a 'b))
bind/0: skip keyword? ('(a 'b))
bind/0: skipping ahead to ('(a 'b))
bind/0: quoted $params
bind/0: positional arg
bind/0: $params: '(a 'b)
bind/0: regular arg
bind/0: ($params ... $body)$body <-> ('(a 'b))nil
bind/0: skip keyword? nil
bind/0: skipping ahead to nil
bind/0: quoted rest $body
bind/0: $body: nil
bind/0: => {$params:'(a 'b), $body:nil, }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {$params:'(a 'b), $body:nil, }
bind/0: caller_scope: nil
bind/0: (a 'b) <-> (1 2)
bind/0: (a 'b)(a 'b) <-> (1 2)(1 2)
bind/0: skip keyword? (1 2)
bind/0: skipping ahead to (1 2)
bind/0: quoted a
bind/0: positional arg
bind/0: a: 1
bind/0: regular arg
bind/0: (a 'b)('b) <-> (1 2)(2)
bind/0: skip keyword? (2)
bind/0: skipping ahead to (2)
bind/0: => {a:1, }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {a:1, }
bind/0: caller_scope: nil
- 71 test_eval_warns_on_double_quoting5
bind/0: ($params ... $body) <-> ('((| a 'b)))
bind/0: ($params ... $body)($params ... $body) <-> ('((| a 'b)))('((| a 'b)))
bind/0: skip keyword? ('((| a 'b)))
bind/0: skipping ahead to ('((| a 'b)))
bind/0: quoted $params
bind/0: positional arg
bind/0: $params: '((| a 'b))
bind/0: regular arg
bind/0: ($params ... $body)$body <-> ('((| a 'b)))nil
bind/0: skip keyword? nil
bind/0: skipping ahead to nil
bind/0: quoted rest $body
bind/0: $body: nil
bind/0: => {$params:'((| a 'b)), $body:nil, }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {$params:'((| a 'b)), $body:nil, }
bind/0: caller_scope: nil
bind/0: ((| a 'b)) <-> (1)
bind/0: ((| a 'b))((| a 'b)) <-> (1)(1)
bind/0: skip keyword? (1)
bind/0: skipping ahead to (1)
bind/0: alias (| a 'b)
bind/0: positional arg
bind/0: quoted alias a
bind/0: a: 1
bind/0: ((| a 'b))nil <-> (1)nil
bind/0: => {a:1, }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {a:1, }
bind/0: caller_scope: nil
- 72 test_eval_binds_missing_as_params_to_nil
bind/0: ($params ... $body) <-> (((| a (b c))) 3)
bind/0: ($params ... $body)($params ... $body) <-> (((| a (b c))) 3)(((| a (b c))) 3)
bind/0: skip keyword? (((| a (b c))) 3)
bind/0: skipping ahead to (((| a (b c))) 3)
bind/0: quoted $params
bind/0: positional arg
bind/0: $params: ((| a (b c)))
bind/0: regular arg
bind/0: ($params ... $body)$body <-> (((| a (b c))) 3)(3)
bind/0: skip keyword? (3)
bind/0: skipping ahead to (3)
bind/0: quoted rest $body
bind/0: $body: (3)
bind/0: => {$params:((| a (b c))), $body:(3), }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {$params:((| a (b c))), $body:(3), }
bind/0: caller_scope: nil
bind/0: ((| a (b c))) <-> (1)
bind/0: ((| a (b c)))((| a (b c))) <-> (1)(1)
bind/0: skip keyword? (1)
bind/0: skipping ahead to (1)
bind/0: alias (| a (b c))
bind/0: positional arg
bind/0: alias sym a
bind/0: eval arg
bind/0: a: 1
bind/0: destructured alias (as-param) (b c)
bind/0: (b c) <-> nil
bind/0: (b c)(b c) <-> nilnil
bind/0: skip keyword? nil
bind/0: skipping ahead to nil
bind/0: quoted b
bind/0: positional arg
bind/0: b: nil
bind/0: regular arg
bind/0: (b c)(c) <-> nilnil
bind/0: skip keyword? nil
bind/0: skipping ahead to nil
bind/0: quoted c
bind/0: positional arg
bind/0: c: nil
bind/0: regular arg
bind/0: (b c)nil <-> nilnil
bind/0: => {b:nil, c:nil, a:1, }
bind/0: ((| a (b c)))nil <-> (1)nil
bind/0: => {b:nil, c:nil, a:1, }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {b:nil, c:nil, a:1, }
bind/0: caller_scope: nil
- 73 test_eval_handles_duplicate_destructured_aliases
bind/0: ($params ... $body) <-> (((a (| b x)) (c (| d x))) 3)
bind/0: ($params ... $body)($params ... $body) <-> (((a (| b x)) (c (| d x))) 3)(((a (| b x)) (c (| d x))) 3)
bind/0: skip keyword? (((a (| b x)) (c (| d x))) 3)
bind/0: skipping ahead to (((a (| b x)) (c (| d x))) 3)
bind/0: quoted $params
bind/0: positional arg
bind/0: $params: ((a (| b x)) (c (| d x)))
bind/0: regular arg
bind/0: ($params ... $body)$body <-> (((a (| b x)) (c (| d x))) 3)(3)
bind/0: skip keyword? (3)
bind/0: skipping ahead to (3)
bind/0: quoted rest $body
bind/0: $body: (3)
bind/0: => {$params:((a (| b x)) (c (| d x))), $body:(3), }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {$params:((a (| b x)) (c (| d x))), $body:(3), }
bind/0: caller_scope: nil
bind/0: ((a (| b x)) (c (| d x))) <-> ('(1 :x 2) '(3 :x 4))
bind/0: ((a (| b x)) (c (| d x)))((a (| b x)) (c (| d x))) <-> ('(1 :x 2) '(3 :x 4))('(1 :x 2) '(3 :x 4))
bind/0: skip keyword? ('(1 :x 2) '(3 :x 4))
bind/0: skipping ahead to ('(1 :x 2) '(3 :x 4))
bind/0: destructured (a (| b x))
bind/0: regular arg
bind/0: (a (| b x)) <-> (1 :x 2)
bind/0: (a (| b x))(a (| b x)) <-> (1 :x 2)(1 :x 2)
bind/0: skip keyword? (1 :x 2)
bind/0: skipping ahead to (1 :x 2)
bind/0: quoted a
bind/0: positional arg
bind/0: a: 1
bind/0: regular arg
bind/0: (a (| b x))((| b x)) <-> (1 :x 2)(:x 2)
bind/0: skip keyword? (:x 2)
bind/0: match? x in (a (| b x))
bind/0: match? x in ((| b x))
bind/0: match alias
bind/0: skipping keyword arg:x 2
bind/0: skip keyword? nil
bind/0: skipping ahead to nil
bind/0: alias (| b x)
bind/0: keyword arg
bind/0: quoted alias b
bind/0: b: 2
bind/0: quoted alias x
bind/0: x: 2
bind/0: (a (| b x))nil <-> (1 :x 2)nil
bind/0: => {b:2, a:1, x:2, }
bind/0: ((a (| b x)) (c (| d x)))((c (| d x))) <-> ('(1 :x 2) '(3 :x 4))('(3 :x 4))
bind/0: skip keyword? ('(3 :x 4))
bind/0: skipping ahead to ('(3 :x 4))
bind/0: destructured (c (| d x))
bind/0: regular arg
bind/0: (c (| d x)) <-> (3 :x 4)
bind/0: (c (| d x))(c (| d x)) <-> (3 :x 4)(3 :x 4)
bind/0: skip keyword? (3 :x 4)
bind/0: skipping ahead to (3 :x 4)
bind/0: quoted c
bind/0: positional arg
bind/0: c: 3
bind/0: regular arg
bind/0: (c (| d x))((| d x)) <-> (3 :x 4)(:x 4)
bind/0: skip keyword? (:x 4)
bind/0: match? x in (c (| d x))
bind/0: match? x in ((| d x))
bind/0: match alias
bind/0: skipping keyword arg:x 4
bind/0: skip keyword? nil
bind/0: skipping ahead to nil
bind/0: alias (| d x)
bind/0: keyword arg
bind/0: quoted alias d
bind/0: d: 4
bind/0: quoted alias x
bind/0: (c (| d x))nil <-> (3 :x 4)nil
bind/0: => {c:3, b:2, a:1, x:2, d:4, }
bind/0: ((a (| b x)) (c (| d x)))nil <-> ('(1 :x 2) '(3 :x 4))nil
bind/0: => {c:3, b:2, a:1, x:2, d:4, }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {c:3, b:2, a:1, x:2, d:4, }
bind/0: caller_scope: nil
- 74 test_eval_handles_duplicate_destructured_aliases2
bind/0: ($params ... $body) <-> (((a (| b x)) (c (| d x))) 3)
bind/0: ($params ... $body)($params ... $body) <-> (((a (| b x)) (c (| d x))) 3)(((a (| b x)) (c (| d x))) 3)
bind/0: skip keyword? (((a (| b x)) (c (| d x))) 3)
bind/0: skipping ahead to (((a (| b x)) (c (| d x))) 3)
bind/0: quoted $params
bind/0: positional arg
bind/0: $params: ((a (| b x)) (c (| d x)))
bind/0: regular arg
bind/0: ($params ... $body)$body <-> (((a (| b x)) (c (| d x))) 3)(3)
bind/0: skip keyword? (3)
bind/0: skipping ahead to (3)
bind/0: quoted rest $body
bind/0: $body: (3)
bind/0: => {$params:((a (| b x)) (c (| d x))), $body:(3), }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {$params:((a (| b x)) (c (| d x))), $body:(3), }
bind/0: caller_scope: nil
bind/0: ((a (| b x)) (c (| d x))) <-> ('(1 :x 2) '(3 :x 4))
bind/0: ((a (| b x)) (c (| d x)))((a (| b x)) (c (| d x))) <-> ('(1 :x 2) '(3 :x 4))('(1 :x 2) '(3 :x 4))
bind/0: skip keyword? ('(1 :x 2) '(3 :x 4))
bind/0: skipping ahead to ('(1 :x 2) '(3 :x 4))
bind/0: destructured (a (| b x))
bind/0: regular arg
bind/0: (a (| b x)) <-> (1 :x 2)
bind/0: (a (| b x))(a (| b x)) <-> (1 :x 2)(1 :x 2)
bind/0: skip keyword? (1 :x 2)
bind/0: skipping ahead to (1 :x 2)
bind/0: quoted a
bind/0: positional arg
bind/0: a: 1
bind/0: regular arg
bind/0: (a (| b x))((| b x)) <-> (1 :x 2)(:x 2)
bind/0: skip keyword? (:x 2)
bind/0: match? x in (a (| b x))
bind/0: match? x in ((| b x))
bind/0: match alias
bind/0: skipping keyword arg:x 2
bind/0: skip keyword? nil
bind/0: skipping ahead to nil
bind/0: alias (| b x)
bind/0: keyword arg
bind/0: quoted alias b
bind/0: b: 2
bind/0: quoted alias x
bind/0: x: 2
bind/0: (a (| b x))nil <-> (1 :x 2)nil
bind/0: => {b:2, a:1, x:2, }
bind/0: ((a (| b x)) (c (| d x)))((c (| d x))) <-> ('(1 :x 2) '(3 :x 4))('(3 :x 4))
bind/0: skip keyword? ('(3 :x 4))
bind/0: skipping ahead to ('(3 :x 4))
bind/0: destructured (c (| d x))
bind/0: regular arg
bind/0: (c (| d x)) <-> (3 :x 4)
bind/0: (c (| d x))(c (| d x)) <-> (3 :x 4)(3 :x 4)
bind/0: skip keyword? (3 :x 4)
bind/0: skipping ahead to (3 :x 4)
bind/0: quoted c
bind/0: positional arg
bind/0: c: 3
bind/0: regular arg
bind/0: (c (| d x))((| d x)) <-> (3 :x 4)(:x 4)
bind/0: skip keyword? (:x 4)
bind/0: match? x in (c (| d x))
bind/0: match? x in ((| d x))
bind/0: match alias
bind/0: skipping keyword arg:x 4
bind/0: skip keyword? nil
bind/0: skipping ahead to nil
bind/0: alias (| d x)
bind/0: keyword arg
bind/0: quoted alias d
bind/0: d: 4
bind/0: quoted alias x
bind/0: (c (| d x))nil <-> (3 :x 4)nil
bind/0: => {d:4, c:3, b:2, a:1, x:2, }
bind/0: ((a (| b x)) (c (| d x)))nil <-> ('(1 :x 2) '(3 :x 4))nil
bind/0: => {d:4, c:3, b:2, a:1, x:2, }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {d:4, c:3, b:2, a:1, x:2, }
bind/0: caller_scope: nil
- 75 test_eval_handles_already_evald_aliased_arg
bind/0: a: 3
bind/0: ($params ... $body) <-> (((| x y)) 3)
bind/0: ($params ... $body)($params ... $body) <-> (((| x y)) 3)(((| x y)) 3)
bind/0: skip keyword? (((| x y)) 3)
bind/0: skipping ahead to (((| x y)) 3)
bind/0: quoted $params
bind/0: positional arg
bind/0: $params: ((| x y))
bind/0: regular arg
bind/0: ($params ... $body)$body <-> (((| x y)) 3)(3)
bind/0: skip keyword? (3)
bind/0: skipping ahead to (3)
bind/0: quoted rest $body
bind/0: $body: (3)
bind/0: => {$params:((| x y)), $body:(3), }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {$params:((| x y)), $body:(3), }
bind/0: caller_scope: nil
bind/0: ((| x y)) <-> (''a)
bind/0: ((| x y))((| x y)) <-> (''a)(''a)
bind/0: skip keyword? (''a)
bind/0: skipping ahead to (''a)
bind/0: alias (| x y)
bind/0: positional arg
bind/0: alias sym x
bind/0: eval arg
bind/0: x: a
bind/0: alias sym y
bind/0: y: a
bind/0: ((| x y))nil <-> (''a)nil
bind/0: => {y:a, x:a, }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {y:a, x:a, }
bind/0: caller_scope: nil
- 76 test_eval_handles_args_with_cycles
- 77 test_fn_evals_arg_only_when_necessary
bind/0: ($params ... $body) <-> ((| 'a (| 'b 'c)) 3)
bind/0: ($params ... $body)($params ... $body) <-> ((| 'a (| 'b 'c)) 3)((| 'a (| 'b 'c)) 3)
bind/0: skip keyword? ((| 'a (| 'b 'c)) 3)
bind/0: skipping ahead to ((| 'a (| 'b 'c)) 3)
bind/0: quoted $params
bind/0: positional arg
bind/0: $params: (| 'a (| 'b 'c))
bind/0: regular arg
bind/0: ($params ... $body)$body <-> ((| 'a (| 'b 'c)) 3)(3)
bind/0: skip keyword? (3)
bind/0: skipping ahead to (3)
bind/0: quoted rest $body
bind/0: $body: (3)
bind/0: => {$params:(| 'a (| 'b 'c)), $body:(3), }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {$params:(| 'a (| 'b 'c)), $body:(3), }
bind/0: caller_scope: nil
bind/0: (| 'a (| 'b 'c)) <-> (x)
bind/0: (| 'a (| 'b 'c))(| 'a (| 'b 'c)) <-> (x)(x)
bind/0: skip keyword? (x)
bind/0: skipping ahead to (x)
bind/0: rest alias (| 'a (| 'b 'c))
bind/0: quoted rest alias 'a
bind/0: a: (x)
bind/0: multiple rest aliases (| 'b 'c)
bind/0: rest alias cons (as-param) (| 'b 'c)
bind/0: (| 'b 'c) <-> (x)
bind/0: (| 'b 'c)(| 'b 'c) <-> (x)(x)
bind/0: skip keyword? (x)
bind/0: skipping ahead to (x)
bind/0: rest alias (| 'b 'c)
bind/0: quoted rest alias 'b
bind/0: b: (x)
bind/0: quoted rest alias 'c
bind/0: c: (x)
bind/0: => {c:(x), b:(x), a:(x), }
bind/0: => {c:(x), b:(x), a:(x), }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {c:(x), b:(x), a:(x), }
bind/0: caller_scope: nil
- 78 test_fn_evals_arg_only_when_necessary2
bind/0: y: 3
bind/0: ($params ... $body) <-> ((| 'a ('b c)) 3)
bind/0: ($params ... $body)($params ... $body) <-> ((| 'a ('b c)) 3)((| 'a ('b c)) 3)
bind/0: skip keyword? ((| 'a ('b c)) 3)
bind/0: skipping ahead to ((| 'a ('b c)) 3)
bind/0: quoted $params
bind/0: positional arg
bind/0: $params: (| 'a ('b c))
bind/0: regular arg
bind/0: ($params ... $body)$body <-> ((| 'a ('b c)) 3)(3)
bind/0: skip keyword? (3)
bind/0: skipping ahead to (3)
bind/0: quoted rest $body
bind/0: $body: (3)
bind/0: => {$params:(| 'a ('b c)), $body:(3), }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {$params:(| 'a ('b c)), $body:(3), }
bind/0: caller_scope: nil
bind/0: (| 'a ('b c)) <-> (x y)
bind/0: (| 'a ('b c))(| 'a ('b c)) <-> (x y)(x y)
bind/0: skip keyword? (x y)
bind/0: skipping ahead to (x y)
bind/0: rest alias (| 'a ('b c))
bind/0: quoted rest alias 'a
bind/0: a: (x y)
bind/0: multiple rest aliases ('b c)
bind/0: rest alias cons (as-param) ('b c)
bind/0: ('b c) <-> (x y)
bind/0: ('b c)('b c) <-> (x y)(x y)
bind/0: skip keyword? (x y)
bind/0: skipping ahead to (x y)
bind/0: quoted b
bind/0: positional arg
bind/0: b: x
bind/0: regular arg
bind/0: ('b c)(c) <-> (x y)(y)
bind/0: skip keyword? (y)
bind/0: skipping ahead to (y)
bind/0: regular c
bind/0: after eval 3
bind/0: c: 3
bind/0: ('b c)nil <-> (x y)nil
bind/0: => {b:x, a:(x y), c:3, }
bind/0: => {b:x, a:(x y), c:3, }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {b:x, a:(x y), c:3, }
bind/0: caller_scope: nil
- 79 test_fn_evals_destructured_arg_only_when_necessary
bind/0: ($params ... $body) <-> (((| 'a 'b)) 3)
bind/0: ($params ... $body)($params ... $body) <-> (((| 'a 'b)) 3)(((| 'a 'b)) 3)
bind/0: skip keyword? (((| 'a 'b)) 3)
bind/0: skipping ahead to (((| 'a 'b)) 3)
bind/0: quoted $params
bind/0: positional arg
bind/0: $params: ((| 'a 'b))
bind/0: regular arg
bind/0: ($params ... $body)$body <-> (((| 'a 'b)) 3)(3)
bind/0: skip keyword? (3)
bind/0: skipping ahead to (3)
bind/0: quoted rest $body
bind/0: $body: (3)
bind/0: => {$params:((| 'a 'b)), $body:(3), }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {$params:((| 'a 'b)), $body:(3), }
bind/0: caller_scope: nil
bind/0: ((| 'a 'b)) <-> (x)
bind/0: ((| 'a 'b))((| 'a 'b)) <-> (x)(x)
bind/0: skip keyword? (x)
bind/0: skipping ahead to (x)
bind/0: alias (| 'a 'b)
bind/0: positional arg
bind/0: quoted alias 'a
bind/0: a: x
bind/0: quoted alias 'b
bind/0: b: x
bind/0: ((| 'a 'b))nil <-> (x)nil
bind/0: => {b:x, a:x, }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {b:x, a:x, }
bind/0: caller_scope: nil
- 80 test_fn_evals_destructured_arg_only_when_necessary2
bind/0: ($params ... $body) <-> (((| 'a (| 'b 'c))) 3)
bind/0: ($params ... $body)($params ... $body) <-> (((| 'a (| 'b 'c))) 3)(((| 'a (| 'b 'c))) 3)
bind/0: skip keyword? (((| 'a (| 'b 'c))) 3)
bind/0: skipping ahead to (((| 'a (| 'b 'c))) 3)
bind/0: quoted $params
bind/0: positional arg
bind/0: $params: ((| 'a (| 'b 'c)))
bind/0: regular arg
bind/0: ($params ... $body)$body <-> (((| 'a (| 'b 'c))) 3)(3)
bind/0: skip keyword? (3)
bind/0: skipping ahead to (3)
bind/0: quoted rest $body
bind/0: $body: (3)
bind/0: => {$params:((| 'a (| 'b 'c))), $body:(3), }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {$params:((| 'a (| 'b 'c))), $body:(3), }
bind/0: caller_scope: nil
bind/0: ((| 'a (| 'b 'c))) <-> (x)
bind/0: ((| 'a (| 'b 'c)))((| 'a (| 'b 'c))) <-> (x)(x)
bind/0: skip keyword? (x)
bind/0: skipping ahead to (x)
bind/0: alias (| 'a (| 'b 'c))
bind/0: positional arg
bind/0: quoted alias 'a
bind/0: a: x
bind/0: nested alias (as-param) (| 'b 'c)
bind/0: quoted alias 'b
bind/0: b: x
bind/0: quoted alias 'c
bind/0: c: x
bind/0: ((| 'a (| 'b 'c)))nil <-> (x)nil
bind/0: => {b:x, a:x, c:x, }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {b:x, a:x, c:x, }
bind/0: caller_scope: nil
- 81 test_fn_evals_destructured_arg_only_when_necessary3
bind/0: x: 3
bind/0: ($params ... $body) <-> (((| 'a (| 'b c))) 3)
bind/0: ($params ... $body)($params ... $body) <-> (((| 'a (| 'b c))) 3)(((| 'a (| 'b c))) 3)
bind/0: skip keyword? (((| 'a (| 'b c))) 3)
bind/0: skipping ahead to (((| 'a (| 'b c))) 3)
bind/0: quoted $params
bind/0: positional arg
bind/0: $params: ((| 'a (| 'b c)))
bind/0: regular arg
bind/0: ($params ... $body)$body <-> (((| 'a (| 'b c))) 3)(3)
bind/0: skip keyword? (3)
bind/0: skipping ahead to (3)
bind/0: quoted rest $body
bind/0: $body: (3)
bind/0: => {$params:((| 'a (| 'b c))), $body:(3), }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {$params:((| 'a (| 'b c))), $body:(3), }
bind/0: caller_scope: nil
bind/0: ((| 'a (| 'b c))) <-> (x)
bind/0: ((| 'a (| 'b c)))((| 'a (| 'b c))) <-> (x)(x)
bind/0: skip keyword? (x)
bind/0: skipping ahead to (x)
bind/0: alias (| 'a (| 'b c))
bind/0: positional arg
bind/0: quoted alias 'a
bind/0: a: x
bind/0: nested alias (as-param) (| 'b c)
bind/0: quoted alias 'b
bind/0: b: x
bind/0: alias sym c
bind/0: eval arg
bind/0: c: 3
bind/0: ((| 'a (| 'b c)))nil <-> (x)nil
bind/0: => {b:x, a:x, c:3, }
bind/0: Curr_lexical_scope: nil
bind/0: Curr_lexical_scope: {b:x, a:x, c:3, }
bind/0: caller_scope: nil
