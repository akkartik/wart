void test_nil_evals_to_itself() {
  run("()");
  CHECK_TRACE_TOP("eval", "nil branch=> nil");
}

void test_num_evals_to_itself() {
  run("34");
  CHECK_TRACE_TOP("eval", "literal=> 34");
}

void test_keyword_sym_evals_to_itself() {
  run(":abc");
  CHECK_TRACE_TOP("eval", "keyword sym=> :abc");
}

void test_string_evals_to_itself() {
  run("\"ac bd\"");
  CHECK_TRACE_TOP("eval", "literal=> \"ac bd\"");
}

void test_sym_evals_to_value() {
  new_dynamic_scope("a", new_num(34));
  run("a");
  CHECK_TRACE_TOP("eval", "sym=> 34");
  end_dynamic_scope("a");
}

void test_sym_evals_to_itself() {
  new_dynamic_scope("a", new_sym("a"));
  run("a");
  CHECK_TRACE_TOP("eval", "sym=> a");
  end_dynamic_scope("a");
}

void test_eval_handles_quoted_atoms() {
  run("'a\n'34");
  CHECK_TRACE_CONTENTS("eval", "'aquote=> a'34quote=> 34");
}

void test_object_expr_evals_to_itself() {
  run("(object foo 4)");
  CHECK_TRACE_TOP("eval", "object=> (object foo 4)");
}

void test_eval_handles_quoted_lists() {
  run("'(a b)");
  CHECK_TRACE_TOP("eval", "quote=> (a b)");
}

void test_eval_handles_backquoted_lists() {
  run("`(a b)");
  CHECK_TRACE_TOP("eval", "backquote=> (a b)");
  CHECK_TRACE_CONTENTS("backquote", "atom: aatom: b");
}

void test_eval_handles_unquote() {
  new_dynamic_scope("b", new_num(34));
  run("`(a ,b)");
  CHECK_TRACE_TOP("eval", "backquote=> (a 34)");
  CHECK_TRACE_CONTENTS("backquote", "atom: aeval: 34");
  end_dynamic_scope("b");
}

void test_eval_handles_unquote_splice() {
  TEMP(val, read("(34 35)"));
  new_dynamic_scope("b", val);
  run("`(a ,@b)");
  CHECK_TRACE_TOP("eval", "backquote=> (a 34 35)");
  CHECK_TRACE_CONTENTS("backquote", /*any frame*/ "atom: asplice: (34 35)");
  end_dynamic_scope("b");
}

void test_eval_splices_copies_of_lists() {
  TEMP(a, read("(3)"));
  new_dynamic_scope("a", a);
  TEMP(result, eval("`(,@a)"));
  CHECK(result != a);
  end_dynamic_scope("a");
}

void test_eval_handles_unquote_and_unquote_splice() {
  TEMP(a, read("(3)"));
  new_dynamic_scope("a", a);
  TEMP(b, read("(4)"));
  new_dynamic_scope("b", b);
  run("`(,@a ,b)");
  CHECK_TRACE_TOP("eval", "backquote=> (3 (4))");
  CHECK_TRACE_CONTENTS("backquote", /*any frame*/ "splice: (3)eval: (4)");
  end_dynamic_scope("b");
  end_dynamic_scope("a");
}


void test_eval_handles_unquote_splice_of_nil() {
  new_dynamic_scope("b", nil);
  run("`(a ,@b 3)");
  CHECK_TRACE_TOP("eval", "backquote=> (a 3)");
  CHECK_TRACE_CONTENTS("backquote", /*any frame*/ "atom: asplice: nilatom: 3");
  end_dynamic_scope("b");
}

void test_eval_quotes_quote_comma() {
  new_dynamic_scope("b", new_sym("x"));
  run("`(a ',b)");
  CHECK_TRACE_TOP("eval", "backquote=> (a 'x)");
  CHECK_TRACE_CONTENTS("backquote", /*any frame*/ "atom: aeval: x");
  end_dynamic_scope("b");
}

void test_eval_evals_comma_quote() {
  new_dynamic_scope("b", new_sym("x"));
  run("`(a ,'b)");
  CHECK_TRACE_TOP("eval", "backquote=> (a b)");
  CHECK_TRACE_CONTENTS("backquote", /*any frame*/ "atom: aeval: b");
  end_dynamic_scope("b");
}

void test_eval_handles_nested_quotes() {
  new_dynamic_scope("a", new_sym("x"));
  TEMP(b, read("(x y)"));
  new_dynamic_scope("b", b);
  run("`(,a `(,a ,,a ,,@b))");
  CHECK_TRACE_TOP("eval", "backquote=> (x `(,a x x y))");
  CHECK_TRACE_CONTENTS("backquote", /*any frame*/ "(,a `(,a ,,a ,,@b)) 1eval: x(,a ,,a ,,@b) 2not deep enough: ,aeval: xsplice: (x y)");
  end_dynamic_scope("a");
  end_dynamic_scope("b");
}



void test_eval_handles_fn_calls() {
  run("((fn () 34))");
  CHECK_TRACE_TOP("eval", "=> 34");
}

void test_eval_expands_syms_in_fn_bodies() {
  new_dynamic_scope("x", new_num(34));
  run("((fn () x))");
  CHECK_TRACE_TOP("eval", "=> 34");
  end_dynamic_scope("x");
}

void test_eval_expands_lexically_scoped_syms_in_fn_bodies() {
  new_lexical_scope();
  add_lexical_binding("a", new_num(34));
    run("((fn () a))");
    CHECK_TRACE_TOP("eval", "=> 34");
  end_lexical_scope();
}

void test_eval_expands_syms_in_original_lexical_scope() {
  new_dynamic_scope("a", new_num(23));
  new_lexical_scope();
  add_lexical_binding("a", new_num(34));
    run("(<- f (fn () a))");
  end_lexical_scope();
  run("(f)");
  CHECK_TRACE_TOP("eval", "=> 34");
  end_dynamic_scope("f");
  end_dynamic_scope("a");
}

void test_eval_expands_args_in_caller_scope() {
  new_dynamic_scope("a", new_num(23));
  new_lexical_scope();
  add_lexical_binding("a", new_num(34));
    run("(<- f (fn (a) a))");
  end_lexical_scope();
  run("(f a)");
  CHECK_TRACE_TOP("eval", "=> 23");
  end_dynamic_scope("f");
  end_dynamic_scope("a");
}

void test_eval_handles_multiple_body_exprs() {
  run("((fn () 1 2))");
  CHECK_TRACE_TOP("eval", "=> 2");
}

void test_eval_evals_arg() {
  new_dynamic_scope("x", new_num(3));
  CLEAR_TRACE;
  run("((fn (a)) x)");
  CHECK_TRACE_CONTENTS("bind", "a: 3");
  end_dynamic_scope("x");
}

void test_eval_evals_arg2() {
  run("((fn (f) (f)) (fn () 34))");
  CHECK_TRACE_TOP("eval", "=> 34");
}

void test_eval_handles_multiple_args() {
  run("((fn (a b) b) 1 2)");
  CHECK_TRACE_TOP("eval", "=> 2");
}

void test_eval_binds_missing_params() {
  new_dynamic_scope("x", new_num(3));
  CLEAR_TRACE;
  run("((fn (a b)) x)");
  CHECK_TRACE_CONTENTS("bind", "a: 3b: nil");
  end_dynamic_scope("x");
}

void test_eval_binds_quoted_param() {
  run("((fn ('a)) x)");
  CHECK_TRACE_CONTENTS("bind", "a: x");
}

void test_eval_handles_quoted_param_list() {
  new_dynamic_scope("a", new_num(23));
  run("((fn '(arg1) arg1) a)");
  CHECK_TRACE_CONTENTS("bind", "arg1: a");
  CHECK_TRACE_TOP("eval", "=> a");
  end_dynamic_scope("a");
}

void test_eval_handles_vararg_param() {
  run("((fn args args) 1)");
  CHECK_TRACE_TOP("eval", "=> (1)");
}

void test_eval_evals_vararg_args() {
  new_dynamic_scope("x", new_num(3));
  new_dynamic_scope("y", new_num(4));
  CLEAR_TRACE;
  run("((fn args) x y)");
  CHECK_TRACE_CONTENTS("eval", "xsym=> 3ysym=> 4");
  CHECK_TRACE_CONTENTS("bind", "args: (3 4)");
  end_dynamic_scope("x");
  end_dynamic_scope("y");
}

void test_eval_binds_quoted_varargs_param() {
  new_dynamic_scope("x", new_num(3));
  new_dynamic_scope("y", new_num(4));
  CLEAR_TRACE;
  run("((fn 'args) x y)");
  CHECK_TRACE_CONTENTS("bind", "args: (x y)");
  end_dynamic_scope("x");
  end_dynamic_scope("y");
}

void test_eval_handles_rest_params() {
  run("((fn (a b ... c) c) 1 2 3 4 5)");
  CHECK_TRACE_TOP("eval", "=> (3 4 5)");
}

void test_eval_evals_rest_args() {
  new_dynamic_scope("x", new_num(3));
  new_dynamic_scope("y", new_num(4));
  CLEAR_TRACE;
  run("((fn (a ... b)) x y)");
  CHECK_TRACE_CONTENTS("bind", "a: 3b: (4)");
  end_dynamic_scope("x");
  end_dynamic_scope("y");
}

void test_eval_binds_quoted_rest_param() {
  new_dynamic_scope("x", new_num(3));
  new_dynamic_scope("y", new_num(4));
  CLEAR_TRACE;
  run("((fn (a ... 'b)) x y)");
  CHECK_TRACE_CONTENTS("bind", "a: 3b: (y)");
  end_dynamic_scope("x");
  end_dynamic_scope("y");
}

void test_eval_handles_destructured_params() {
  run("((fn ((a b)) b) '(1 2))");
  CHECK_TRACE_CONTENTS("bind", "a: 1b: 2");
}

void test_eval_evals_destructured_args() {
  new_dynamic_scope("x", new_num(3));
  new_dynamic_scope("y", new_num(4));
  CLEAR_TRACE;
  run("((fn ((a b))) `(,x ,y))");
  CHECK_TRACE_CONTENTS("bind", "a: 3b: 4");
  end_dynamic_scope("x");
  end_dynamic_scope("y");
}

void test_eval_handles_quoted_destructured_params() {
  run("((fn ('(a b)) b) (1 2))");
  CHECK_TRACE_CONTENTS("bind", "a: 1b: 2");
}



void test_eval_splices_args() {
  TEMP(val, read("(3 4)"));
  new_dynamic_scope("b", val);
  run("cons @b");
  CHECK_TRACE_CONTENTS("splice", "(''3 ''4)");
  CHECK_TRACE_TOP("eval", "=> (3 ... 4)");
  end_dynamic_scope("b");
}

void test_eval_splices_args2() {
  new_dynamic_scope("a", new_num(3));
  TEMP(b, read("(4 5)"));
  new_dynamic_scope("b", b);
  run("((fn nil 3) a @b a)");
  CHECK_TRACE_CONTENTS("splice", "(a ''4 ''5 a)");  // other args get eval'd later
  end_dynamic_scope("b");
  end_dynamic_scope("a");
}

void test_eval_splices_args3() {
  run("f <- (fn (x y) (cons x y))");
  run("(a <- 3) (b <- 4)");
  run("args <- '(a b)");
  CLEAR_TRACE;
  run("f @args");
  CHECK_TRACE_TOP("eval", "=> (a ... b)");
  end_dynamic_scope("args");
  end_dynamic_scope("b");
  end_dynamic_scope("a");
  end_dynamic_scope("f");
}

void test_eval_splices_nil_args() {
  new_dynamic_scope("a", new_num(3));
  new_dynamic_scope("b", nil);
  run("((fn nil 3) a @b a)");
  CHECK_TRACE_CONTENTS("splice", "(a a)");
  end_dynamic_scope("b");
  end_dynamic_scope("a");
}

void test_eval_splices_keyword_syms_into_args() {
  new_dynamic_scope("a", new_num(3));
  TEMP(b, read("(4 :x)"));
  new_dynamic_scope("b", b);
  run("((fn nil 3) a @b a)");
  CHECK_TRACE_CONTENTS("splice", "(a ''4 :x a)");   // keyword syms aren't tagged with ''
  end_dynamic_scope("b");
  end_dynamic_scope("a");
}

void test_eval_handles_splice_inside_fn_body() {
  run("((fn x (cons @x)) 1 2)");
  CHECK_TRACE_TOP("eval", "=> (1 ... 2)");
}

void test_eval_handles_splice_and_selective_quoting() {
  run("f <- (fn ('x y) (cons x y))");
  run("(a <- 3) (b <- 4)");
  run("args <- '(b)");
  CLEAR_TRACE;
  run("f a @args");
  CHECK_TRACE_TOP("eval", "=> (a ... b)");
  end_dynamic_scope("args");
  end_dynamic_scope("b");
  end_dynamic_scope("a");
  end_dynamic_scope("f");
}

void test_eval_overrides_quoted_params_with_spliced_args() {
  run("f <- (fn (x 'y) (cons x y))");
  run("(a <- 3) (b <- 4)");
  run("args <- '(a b)");
  CLEAR_TRACE;
  run("f @args");
  CHECK_TRACE_TOP("eval", "=> (a ... b)");
  end_dynamic_scope("args");
  end_dynamic_scope("b");
  end_dynamic_scope("a");
  end_dynamic_scope("f");
}

void test_eval_handles_already_evald_arg() {
  new_dynamic_scope("a", new_num(3));
  TEMP(call, read("((fn (x) 3))"));
  cell* arg = tag_already_evald(new_sym("a"));  // ''a but can't go through read
  append(call, new_cons(arg));
  In_macro.push(true);
  rmref(eval(call));
  CHECK_TRACE_CONTENTS("bind", "x: a");
  In_macro.pop();
  end_dynamic_scope("a");
}

void test_eval_handles_multiply_already_evald_arg() {
  TEMP(call, read("((fn (x) 3))"));
  cell* arg = tag_already_evald(tag_already_evald(new_sym("a")));  // ''''a
  append(call, new_cons(arg));
  In_macro.push(true);
  rmref(eval(call));
  CHECK_TRACE_CONTENTS("bind", "x: a");
  In_macro.pop();
}

void test_eval_handles_already_evald_rest_arg() {
  new_dynamic_scope("a", new_num(3));
  TEMP(call, read("((fn x 3))"));
  cell* arg = tag_already_evald(new_sym("a"));  // ''a but can't go through read
  append(call, new_cons(arg));
  In_macro.push(true);
  rmref(eval(call));
  CHECK_TRACE_CONTENTS("bind", "x: (a)");
  In_macro.pop();
  end_dynamic_scope("a");
}

void test_eval_splice_on_macros_with_backquote() {
  run("m <- (fn '(x y) (eval `(cons ,x ,y) caller_scope))");
  run("(a <- 3) (b <- 4)");
  run("args <- '(a b)");
  run("m @args");
  CHECK_TRACE_TOP("eval", "=> (a ... b)");  // spliced args override quoted params
  CHECK_EQ(Raise_count, 0);
  end_dynamic_scope("args");
  end_dynamic_scope("b");
  end_dynamic_scope("a");
  end_dynamic_scope("m");
}

void test_eval_splice_on_backquoteless_macros_warns() {
  run("m <- (fn '(x y) (eval (cons 'cons (cons x (cons y nil))) caller_scope))");
  run("(a <- 3) (b <- 4)");
  run("args <- '(a b)");
  run("m @args");
  CHECK_EQ(Raise_count, 1);   Raise_count=0;
  end_dynamic_scope("args");
  end_dynamic_scope("b");
  end_dynamic_scope("a");
  end_dynamic_scope("m");
}



void test_reorder_keyword_args_keeps_nil_rest_args() {
  run("((fn a 3) nil)");
  CHECK_TRACE_CONTENTS("ordered_args", "unchanged: (nil)");
  run("((fn 'a 3) nil)");
  CHECK_TRACE_CONTENTS("ordered_args", "unchanged: (nil)");
}

void test_reorder_keyword_args_handles_improper_lists() {
  TEMP(args, read("(3 ... 4)"));
  TEMP(params, read("(a ... b)"));
  rmref(reorder_keyword_args(args, params));
  CHECK_TRACE_CONTENTS("ordered_args", "=> (3 ... 4)");
}

void test_reorder_keyword_args_handles_overlong_lists() {
  run("((fn (a b) 3) 3 4 5)");
  CHECK_TRACE_CONTENTS("ordered_args", "=> (3 4 5)");
}

void test_eval_handles_keyword_args() {
  run("((fn (a b c) c) :c 1 2)");
  CHECK_TRACE_CONTENTS("ordered_args", "=> (2 nil 1)");
  CHECK_TRACE_TOP("eval", "=> 1");
}

void test_eval_handles_quoted_keyword_args() {
  run("((fn (a b 'c) c) :c 1 2)");
  CHECK_TRACE_CONTENTS("ordered_args", "=> (2 nil 1)");
  CHECK_TRACE_TOP("eval", "=> 1");
}

void test_eval_handles_quoted_keyword_args2() {
  run("x <- 1");
  run("((fn (a b 'c) c) :c x 2)");
  CHECK_TRACE_CONTENTS("ordered_args", "=> (2 nil x)");
  CHECK_TRACE_TOP("eval", "=> x");
  end_dynamic_scope("x");
}

void test_eval_handles_rest_keyword_arg() {
  run("((fn (a ... b) b) 2 :b 1 3)");
  CHECK_TRACE_CONTENTS("ordered_args", "=> (2 1 3)");
  CHECK_TRACE_TOP("eval", "=> (1 3)");
}

void test_eval_handles_rest_keyword_arg_at_end2() {
  run("((fn (a ... b) b) :b 1 2 3)");
  CHECK_TRACE_CONTENTS("ordered_args", "=> (nil 1 2 3)");
  CHECK_TRACE_TOP("eval", "=> (1 2 3)");
}

void test_eval_handles_args_after_rest_keyword() {
  run("((fn (a ... b) b) :b 1 2 :a 3)");
  CHECK_TRACE_CONTENTS("ordered_args", "=> (3 1 2)");
  CHECK_TRACE_TOP("eval", "=> (1 2)");
}

void test_eval_handles_quoted_rest_keyword_arg() {
  run("x <- 2");
  run("((fn (a ... 'b) b) :b 1 x 3)");
  CHECK_TRACE_CONTENTS("ordered_args", "=> (nil 1 x 3)");
  CHECK_TRACE_TOP("eval", "=> (1 x 3)");
  end_dynamic_scope("x");
}

void test_eval_handles_non_keyword_arg_colon_syms() {
  run("((fn (a b) a) :x 1)");
  CHECK_TRACE_CONTENTS("ordered_args", "=> (:x 1)");
  CHECK_TRACE_TOP("eval", "=> :x");
}

void test_eval_handles_keyword_args_inside_splice() {
  run("((fn (a b) b) @'(3 :a 4))");
  CHECK_TRACE_CONTENTS("ordered_args", "=> (''4 ''3)");
  CHECK_TRACE_TOP("eval", "=> 3");
}

void test_eval_handles_keyword_args_inside_destructured_params() {
  run("((fn ((a b)) b) '(3 :a 4))");
  CHECK_TRACE_CONTENTS("ordered_args", "=> (4 3)");
  CHECK_TRACE_TOP("eval", "=> 3");
}



void test_eval_bind_all_handles_already_evald_aliased_arg() {
  cell* params = read("(x|y)");
  cell* args = mkref(new_cons(tag_already_evald(new_sym("a"))));   // (''a)
  cell* scope = mkref(new_table());
  set(scope, "a", new_num(3));
  cell* new_scope = mkref(new_table());
  In_macro.push(true);
  eval_bind_all(params, args, scope, new_scope);
  In_macro.pop();
  CHECK_EQ(unsafe_get(new_scope, "x"), new_sym("a"));
  CHECK_EQ(unsafe_get(new_scope, "y"), new_sym("a"));
  rmref(new_scope);
  rmref(scope);
  rmref(args);
  rmref(params);
}

void test_eval_bind_all_handles_param_aliases() {
  cell* params = read("(a|b)");
  cell* args = read("(3)");
  cell* new_scope = mkref(new_table());
  eval_bind_all(params, args, nil, new_scope);
  // {a: 3, b: 3}
  CHECK_EQ(unsafe_get(new_scope, "a"), new_num(3));
  CHECK_EQ(unsafe_get(new_scope, "b"), new_num(3));
  rmref(new_scope);
  rmref(args);
  rmref(params);
}

void test_eval_bind_all_only_reorders_when_necessary() {
  cell* params = read("((a|b))");
  cell* x = read("(3)");
  Trace_stream = NULL;  // leak; trace can't handle cycles yet
  set_cdr(x, x);   // cycle
  cell* scope = mkref(new_table());
  set(scope, "x", x);
  cell* arg = read("((fn args args) x)");   // (list x)
  cell* args = mkref(new_cons(arg));
  cell* new_scope = mkref(new_table());
  eval_bind_all(params, args, scope, new_scope);
  // should terminate
  set_cdr(x, nil);
  rmref(new_scope);
  rmref(args);
  rmref(arg);
  rmref(scope);
  rmref(x);
  rmref(params);
}

void test_eval_bind_all_binds_as_params() {
  cell* params = read("(a | (b c))");
  cell* args = read("(1 2)");
  cell* new_scope = mkref(new_table());
  eval_bind_all(params, args, nil, new_scope);
  // {a: (1 2), b: 1, c: 2}
  CHECK_EQ(car(unsafe_get(new_scope, "a")), new_num(1));
  CHECK_EQ(car(cdr(unsafe_get(new_scope, "a"))), new_num(2));
  CHECK_EQ(cdr(cdr(unsafe_get(new_scope, "a"))), nil);
  CHECK_EQ(unsafe_get(new_scope, "b"), new_num(1));
  CHECK_EQ(unsafe_get(new_scope, "c"), new_num(2));
  rmref(new_scope);
  rmref(args);
  rmref(params);
}

void test_eval_bind_all_binds_as_params_recursively() {
  cell* params = read("(a | (b ... (c | (d e))))");
  cell* args = read("(1 2 3)");
  cell* new_scope = mkref(new_table());
  eval_bind_all(params, args, nil, new_scope);
  // {a: (1 2 3), b: 1, c: (2 3), d: 2, e: 3}
  CHECK_EQ(car(unsafe_get(new_scope, "a")), new_num(1));
  CHECK_EQ(car(cdr(unsafe_get(new_scope, "a"))), new_num(2));
  CHECK_EQ(car(cdr(cdr(unsafe_get(new_scope, "a")))), new_num(3));
  CHECK_EQ(cdr(cdr(cdr(unsafe_get(new_scope, "a")))), nil);
  CHECK_EQ(unsafe_get(new_scope, "b"), new_num(1));
  CHECK_EQ(car(unsafe_get(new_scope, "c")), new_num(2));
  CHECK_EQ(car(cdr(unsafe_get(new_scope, "c"))), new_num(3));
  CHECK_EQ(cdr(cdr(unsafe_get(new_scope, "c"))), nil);
  CHECK_EQ(unsafe_get(new_scope, "d"), new_num(2));
  CHECK_EQ(unsafe_get(new_scope, "e"), new_num(3));
  rmref(new_scope);
  rmref(args);
  rmref(params);
}

void test_eval_bind_all_handles_quoted_param_aliases() {
  cell* params = read("((a | 'b))");
  cell* args = read("(x)");
  cell* scope = mkref(new_table());
  set(scope, "x", new_num(3));
  cell* new_scope = mkref(new_table());
  eval_bind_all(params, args, scope, new_scope);
  // {a: 3, b: x}
  CHECK_EQ(unsafe_get(new_scope, "a"), new_num(3));
  CHECK_EQ(unsafe_get(new_scope, "b"), new_sym("x"));
  rmref(new_scope);
  rmref(scope);
  rmref(args);
  rmref(params);
}

void test_eval_bind_all_handles_quoted_rest_param_aliases() {
  cell* params = read("(a | 'b)");
  cell* args = read("(x)");
  cell* scope = mkref(new_table());
  set(scope, "x", new_num(3));
  cell* new_scope = mkref(new_table());
  eval_bind_all(params, args, scope, new_scope);
  // {a: (3), b: (x)}
  CHECK_EQ(car(unsafe_get(new_scope, "a")), new_num(3));
  CHECK_EQ(car(unsafe_get(new_scope, "b")), new_sym("x"));
  rmref(new_scope);
  rmref(scope);
  rmref(args);
  rmref(params);
}

void test_eval_bind_all_handles_quoted_destructured_rest_param_aliases0() {
  cell* params = read("('a | ('b))");
  cell* args = read("(x)");
  cell* scope = mkref(new_table());
  set(scope, "x", new_num(3));
  cell* new_scope = mkref(new_table());
  eval_bind_all(params, args, scope, new_scope);
  // {a: (x), b: x}
  CHECK_EQ(car(unsafe_get(new_scope, "a")), new_sym("x"));
  CHECK_EQ(unsafe_get(new_scope, "b"), new_sym("x"));
  rmref(new_scope);
  rmref(scope);
  rmref(args);
  rmref(params);
}

void test_eval_bind_all_handles_quoted_destructured_rest_param_aliases() {
  cell* params = read("(a | ('b))");
  cell* args = read("(x)");
  cell* scope = mkref(new_table());
  set(scope, "x", new_num(3));
  cell* new_scope = mkref(new_table());
  eval_bind_all(params, args, scope, new_scope);
  // {a: (3), b: x}
  CHECK_EQ(car(unsafe_get(new_scope, "a")), new_num(3));
  CHECK_EQ(unsafe_get(new_scope, "b"), new_sym("x"));
  rmref(new_scope);
  rmref(scope);
  rmref(args);
  rmref(params);
}

void test_eval_bind_all_evals_aliases_only_when_necessary() {
  cell* params = read("(('a | 'b))");
  cell* args = read("(x)");
  cell* new_scope = mkref(new_table());
  long old_eval_count = Eval_count;
  eval_bind_all(params, args, nil, new_scope);
  CHECK_EQ(Eval_count-old_eval_count, 0);
  // {a: x, b: x}
  CHECK_EQ(unsafe_get(new_scope, "a"), new_sym("x"));
  CHECK_EQ(unsafe_get(new_scope, "b"), new_sym("x"));
  rmref(new_scope);
  rmref(args);
  rmref(params);
}

void test_eval_bind_all_evals_aliases_only_when_necessary2() {
  cell* params = read("('a | ('b))");
  cell* args = read("(x)");
  cell* new_scope = mkref(new_table());
  long old_eval_count = Eval_count;
  eval_bind_all(params, args, nil, new_scope);
  CHECK(Eval_count-old_eval_count > 0);
  // {a: (x), b: x}
  CHECK_EQ(car(unsafe_get(new_scope, "a")), new_sym("x"));
  CHECK_EQ(unsafe_get(new_scope, "b"), new_sym("x"));
  rmref(new_scope);
  rmref(args);
  rmref(params);
}

void test_eval_bind_all_evals_aliases_only_when_necessary3() {
  cell* params = read("(| 'a ('b c))");
  cell* args = read("(x y)");
  cell* scope = mkref(new_table());
  set(scope, "y", new_num(3));
  cell* new_scope = mkref(new_table());
  long old_eval_count = Eval_count;
  eval_bind_all(params, args, scope, new_scope);
  CHECK(Eval_count-old_eval_count > 0);
  // {a: (x y), b: x, c: 3}
  CHECK_EQ(car(unsafe_get(new_scope, "a")), new_sym("x"));
  CHECK_EQ(car(cdr(unsafe_get(new_scope, "a"))), new_sym("y"));
  CHECK_EQ(cdr(cdr(unsafe_get(new_scope, "a"))), nil);
  CHECK_EQ(unsafe_get(new_scope, "b"), new_sym("x"));
  CHECK_EQ(unsafe_get(new_scope, "c"), new_num(3));
  rmref(new_scope);
  rmref(scope);
  rmref(args);
  rmref(params);
}

void test_eval_bind_all_evals_aliases_only_when_necessary4() {
  cell* params = read("((| 'a (| 'b c)))");
  cell* args = read("(x)");
  cell* scope = mkref(new_table());
  set(scope, "x", new_num(3));
  cell* new_scope = mkref(new_table());
  long old_eval_count = Eval_count;
  eval_bind_all(params, args, scope, new_scope);
  CHECK(Eval_count-old_eval_count > 0);
  // {a: x, b: x, c: 3}
  CHECK_EQ(unsafe_get(new_scope, "a"), new_sym("x"));
  CHECK_EQ(unsafe_get(new_scope, "b"), new_sym("x"));
  CHECK_EQ(unsafe_get(new_scope, "c"), new_num(3));
  rmref(new_scope);
  rmref(scope);
  rmref(args);
  rmref(params);
}

void test_eval_bind_all_evals_aliases_only_when_necessary5() {
  cell* params = read("((| 'a (| 'b 'c)))");
  cell* args = read("(x)");
  cell* new_scope = mkref(new_table());
  long old_eval_count = Eval_count;
  eval_bind_all(params, args, nil, new_scope);
  CHECK_EQ(Eval_count-old_eval_count, 0);
  // {a: x, b: x, c: x}
  CHECK_EQ(unsafe_get(new_scope, "a"), new_sym("x"));
  CHECK_EQ(unsafe_get(new_scope, "b"), new_sym("x"));
  CHECK_EQ(unsafe_get(new_scope, "c"), new_sym("x"));
  rmref(new_scope);
  rmref(args);
  rmref(params);
}

void test_eval_bind_all_evals_aliases_only_when_necessary6() {
  cell* params = read("(| 'a (| 'b 'c))");
  cell* args = read("(x)");
  cell* new_scope = mkref(new_table());
  eval_bind_all(params, args, nil, new_scope);
  CHECK_EQ(Raise_count, 0);
  // {a: (x), b: (x), c: (x)}
  CHECK_EQ(car(unsafe_get(new_scope, "a")), new_sym("x"));
  CHECK_EQ(car(unsafe_get(new_scope, "b")), new_sym("x"));
  CHECK_EQ(car(unsafe_get(new_scope, "c")), new_sym("x"));
  rmref(new_scope);
  rmref(args);
  rmref(params);
}

// gotcha: a|(b c) won't work
void test_eval_bind_all_warns_on_unary_as() {
  cell* params = read("(| a)");
  cell* args = read("(1 2)");
  cell* new_scope = mkref(new_table());
  eval_bind_all(params, args, nil, new_scope);
  CHECK_EQ(Raise_count, 1);   Raise_count=0;
  rmref(new_scope);
  rmref(args);
  rmref(params);
}

void test_eval_bind_all_binds_missing_as_params_to_nil() {
  cell* params = read("(a | (b c))");
  cell* args = read("1");
  cell* new_scope = mkref(new_table());
  eval_bind_all(params, args, nil, new_scope);
  CHECK_EQ(Raise_count, 0);
  // {a: x}
  CHECK_EQ(unsafe_get(new_scope, "a"), new_num(1));
  CHECK_EQ(unsafe_get(new_scope, "b"), nil);
  CHECK_EQ(unsafe_get(new_scope, "c"), nil);
  rmref(new_scope);
  rmref(args);
  rmref(params);
}

void test_eval_bind_all_binds_alternatives_to_non_cons() {
  cell* params = read("(a b|c)");
  cell* args = read("(1 2)");
  cell* new_scope = mkref(new_table());
  eval_bind_all(params, args, nil, new_scope);
  CHECK_EQ(Raise_count, 0);
  // {a: 1, b: 2, c: 2}
  CHECK_EQ(unsafe_get(new_scope, "a"), new_num(1));
  CHECK_EQ(unsafe_get(new_scope, "b"), new_num(2));
  CHECK_EQ(unsafe_get(new_scope, "c"), new_num(2));
  rmref(new_scope);
  rmref(args);
  rmref(params);
}

void test_eval_bind_all_handles_duplicate_destructured_aliases() {
  cell* params = read("((a b|x) (c d|x))");
  cell* args = read("('(1 :x 2) '(3 :x 4))");
  cell* new_scope = mkref(new_table());
  eval_bind_all(params, args, nil, new_scope);
  // {a: 1, b: 2, c: 3, d: 4, x: 2 or 4}
  CHECK_EQ(unsafe_get(new_scope, "a"), new_num(1));
  CHECK_EQ(unsafe_get(new_scope, "b"), new_num(2));
  CHECK_EQ(unsafe_get(new_scope, "c"), new_num(3));
  CHECK_EQ(unsafe_get(new_scope, "d"), new_num(4));
  cell* x = unsafe_get(new_scope, "x");
  CHECK(x == new_num(2) || x == new_num(4));
  rmref(new_scope);
  rmref(args);
  rmref(params);
}

void test_eval_handles_keyword_args_for_fns2() {
  cell* fn = read("(fn (a b c|x) c)");
  cell* f = eval(fn);
  new_dynamic_scope("f", f);
  cell* call = read("(f :c 1 2)");
  cell* result = eval(call);
  CHECK_EQ(result, new_num(1));
  rmref(result);
  rmref(call);
  rmref(f);
  rmref(fn);
  end_dynamic_scope("f");
}

void test_eval_handles_body_keyword_synonym() {
  cell* fn = read("(fn (a ... body|do) body)");
  cell* f = eval(fn);
  new_dynamic_scope("f", f);
  cell* call = read("(f 2 :do 1 3)");
  cell* result = eval(call);
  // (1 3)
  CHECK_EQ(car(result), new_num(1));
  CHECK_EQ(car(cdr(result)), new_num(3));
  CHECK_EQ(cdr(cdr(result)), nil);
  rmref(result);
  rmref(call);
  rmref(f);
  rmref(fn);
  end_dynamic_scope("f");
}

void test_eval_handles_body_keyword_synonym2() {
  cell* fn = read("(fn (a b ... body|do) `(,a ,b ,body))");
  cell* f = eval(fn);
  new_dynamic_scope("f", f);
  cell* call = read("(f 2 :do 1 3)");
  cell* result = eval(call);
  // (2 nil (1 3))
  CHECK(is_cons(result));
  CHECK_EQ(car(result), new_num(2));
  CHECK_EQ(car(cdr(result)), nil);
  CHECK(is_cons(car(cdr(cdr(result)))));
  CHECK_EQ(car(car(cdr(cdr(result)))), new_num(1));
  CHECK_EQ(car(cdr(car(cdr(cdr(result))))), new_num(3));
  CHECK_EQ(cdr(cdr(car(cdr(cdr(result))))), nil);
  rmref(result);
  rmref(call);
  rmref(f);
  rmref(fn);
  end_dynamic_scope("f");
}



void test_eval_handles_assigned_fn_calls() {
  run("(<- f (fn () 34))");
  CLEAR_TRACE;
  run("(f)");
  CHECK_TRACE_TOP("eval", "=> 34");
  end_dynamic_scope("f");
}
