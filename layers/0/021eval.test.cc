void test_splice_args_works() {
  new_dynamic_scope("a", new_num(3));
  new_dynamic_scope("b", new_cons(new_num(4), new_cons(new_num(5))));   // (4 5)
  cell* args = read("(a @b a)");
  cell* f = read("(fn nil 3)");
  cell* fn = eval(f);
  cell* spliced_args = splice_args(args, nil, fn);
  // (a ''4 ''5 a)
  CHECK_EQ(car(spliced_args), new_sym("a"));
  CHECK_EQ(car(car(cdr(spliced_args))), sym_already_evald);
  CHECK_EQ(cdr(car(cdr(spliced_args))), new_num(4));
  CHECK_EQ(car(car(cdr(cdr(spliced_args)))), sym_already_evald);
  CHECK_EQ(cdr(car(cdr(cdr(spliced_args)))), new_num(5));
  CHECK_EQ(car(cdr(cdr(cdr(spliced_args)))), new_sym("a"));
  CHECK_EQ(cdr(cdr(cdr(cdr(spliced_args)))), nil);
  rmref(spliced_args);
  rmref(fn);
  rmref(f);
  rmref(args);
  end_dynamic_scope("b");
  end_dynamic_scope("a");
}

void test_splice_args_works_with_nil() {
  new_dynamic_scope("a", new_num(3));
  new_dynamic_scope("b", nil);
  cell* args = read("(a @b a)");
  cell* f = read("(fn nil 3)");
  cell* fn = eval(f);
  cell* spliced_args = splice_args(args, nil, fn);
  // (a a)
  CHECK_EQ(car(spliced_args), new_sym("a"));
  CHECK_EQ(car(cdr(spliced_args)), new_sym("a"));
  CHECK_EQ(cdr(cdr(spliced_args)), nil);
  rmref(spliced_args);
  rmref(fn);
  rmref(f);
  rmref(args);
  end_dynamic_scope("b");
  end_dynamic_scope("a");
}

void test_splice_args_works_with_keywords() {
  new_dynamic_scope("a", new_num(3));
  new_dynamic_scope("b", new_cons(new_num(4), new_cons(new_sym(":x"))));  // (4 :x)
  cell* args = read("(a @b a)");
  cell* f = read("(fn nil 3)");
  cell* fn = eval(f);
  cell* spliced_args = splice_args(args, nil, fn);
  // (a ''4 :x a)
  CHECK_EQ(car(spliced_args), new_sym("a"));
  CHECK_EQ(car(car(cdr(spliced_args))), sym_already_evald);
  CHECK_EQ(cdr(car(cdr(spliced_args))), new_num(4));
  CHECK_EQ(car(cdr(cdr(spliced_args))), new_sym(":x"));
  CHECK_EQ(car(cdr(cdr(cdr(spliced_args)))), new_sym("a"));
  CHECK_EQ(cdr(cdr(cdr(cdr(spliced_args)))), nil);
  rmref(spliced_args);
  rmref(fn);
  rmref(f);
  rmref(args);
  end_dynamic_scope("b");
  end_dynamic_scope("a");
}

void test_reorder_keyword_args_keeps_nil_rest_args() {
  CHECK_EQ(reorder_keyword_args(nil, new_sym("a")), nil);
  cell* params = mkref(new_cons(sym_quote, new_sym("a")));
  CHECK_EQ(reorder_keyword_args(nil, params), nil);
  rmref(params);
}

void test_reorder_keyword_args_handles_improper_lists() {
  cell* args = mkref(new_cons(new_num(3), new_num(4)));
  cell* params = mkref(new_cons(new_sym("a"), new_sym("b")));
  cell* ordered_args = reorder_keyword_args(args, params);
  // args == ordered_args
  CHECK_EQ(car(ordered_args), car(args));
  CHECK_EQ(cdr(ordered_args), cdr(args));
  rmref(ordered_args);
  rmref(args);
  rmref(params);
}

void test_reorder_keyword_args_handles_overlong_lists() {
  cell* args = mkref(new_cons(new_num(3), new_cons(new_num(4), new_cons(new_num(5)))));  // (3 4 5)
  cell* params = mkref(new_cons(new_sym("a"), new_cons(new_sym("b"))));  // (a b)
  cell* ordered_args = reorder_keyword_args(args, params);
  // args == ordered_args
  CHECK_EQ(car(ordered_args), car(args));
  CHECK_EQ(car(cdr(ordered_args)), car(cdr(args)));
  CHECK_EQ(car(cdr(cdr(ordered_args))), car(cdr(cdr(args))));
  rmref(ordered_args);
  rmref(args);
  rmref(params);
}



void test_eval_bind_all_handles_unquoted_param() {
  cell* params = read("(x)");
  cell* args = read("(a)");
  cell* scope = mkref(new_table());
  set(scope, "a", new_num(3));
  cell* new_scope = mkref(new_table());
  eval_bind_all(params, args, scope, new_scope);
  CHECK_EQ(unsafe_get(new_scope, "x"), new_num(3));
  rmref(scope);
  rmref(new_scope);
  rmref(args);
  rmref(params);
}

void test_eval_bind_all_binds_missing_params() {
  cell* params = read("(x y)");
  cell* args = read("(a)");
  cell* scope = mkref(new_table());
  set(scope, "a", new_num(3));
  cell* new_scope = mkref(new_table());
  eval_bind_all(params, args, scope, new_scope);
  CHECK_EQ(unsafe_get(new_scope, "x"), new_num(3));
  CHECK_EQ(unsafe_get(new_scope, new_sym("y")), nil);
  rmref(scope);
  rmref(new_scope);
  rmref(args);
  rmref(params);
}

void test_eval_bind_all_handles_quoted_param() {
  cell* params = read("('x)");
  cell* args = read("(a)");
  cell* new_scope = mkref(new_table());
  eval_bind_all(params, args, nil, new_scope);
  CHECK_EQ(unsafe_get(new_scope, "x"), new_sym("a"));
  rmref(new_scope);
  rmref(args);
  rmref(params);
}

void test_eval_bind_all_handles_already_evald_arg() {
  cell* params = read("(x)");
  cell* args = mkref(new_cons(tag_already_evald(new_sym("a"))));   // (''a)
  cell* scope = mkref(new_table());
  set(scope, "a", new_num(3));
  cell* new_scope = mkref(new_table());
  In_macro.push(true);
  eval_bind_all(params, args, scope, new_scope);
  In_macro.pop();
  CHECK_EQ(unsafe_get(new_scope, "x"), new_sym("a"));
  rmref(new_scope);
  rmref(scope);
  rmref(args);
  rmref(params);
}

void test_eval_bind_all_handles_multiply_already_evald_arg() {
  cell* params = read("(x)");
  cell* args = mkref(new_cons(tag_already_evald(tag_already_evald(new_sym("a")))));  // (''''a)
  cell* scope = mkref(new_table());
  set(scope, "a", new_num(3));
  cell* new_scope = mkref(new_table());
  In_macro.push(true);
  eval_bind_all(params, args, scope, new_scope);
  In_macro.pop();
  CHECK_EQ(unsafe_get(new_scope, "x"), new_sym("a"));
  rmref(new_scope);
  rmref(scope);
  rmref(args);
  rmref(params);
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

void test_eval_bind_all_handles_already_evald_rest_arg() {
  cell* params = read("x");
  cell* args = mkref(new_cons(tag_already_evald(new_sym("a"))));
  cell* scope = mkref(new_table());
  set(scope, "a", new_num(3));
  cell* new_scope = mkref(new_table());
  In_macro.push(true);
  eval_bind_all(params, args, scope, new_scope);
  In_macro.pop();
  CHECK_EQ(car(unsafe_get(new_scope, "x")), new_sym("a"));
  rmref(new_scope);
  rmref(scope);
  rmref(args);
  rmref(params);
}

void test_eval_bind_all_handles_varargs_param() {
  cell* params = read("x");
  cell* args = read("(a b)");
  cell* scope = mkref(new_table());
  set(scope, "a", new_num(3));
  set(scope, "b", new_num(4));
  cell* new_scope = mkref(new_table());
  eval_bind_all(params, args, scope, new_scope);
  // {x: (3 4)}
  CHECK_EQ(car(unsafe_get(new_scope, "x")), new_num(3));
  CHECK_EQ(car(cdr(unsafe_get(new_scope, "x"))), new_num(4));
  CHECK_EQ(cdr(cdr(unsafe_get(new_scope, "x"))), nil);
  rmref(new_scope);
  rmref(scope);
  rmref(args);
  rmref(params);
}

void test_eval_bind_all_handles_quoted_varargs_param() {
  cell* params = read("'x");
  cell* args = read("(a b)");
  cell* scope = mkref(new_table());
  set(scope, "a", new_num(3));
  set(scope, "b", new_num(4));
  cell* new_scope = mkref(new_table());
  eval_bind_all(params, args, scope, new_scope);
  // {x: (a b)}
  CHECK_EQ(car(unsafe_get(new_scope, "x")), new_sym("a"));
  CHECK_EQ(car(cdr(unsafe_get(new_scope, "x"))), new_sym("b"));
  CHECK_EQ(cdr(cdr(unsafe_get(new_scope, "x"))), nil);
  rmref(new_scope);
  rmref(scope);
  rmref(args);
  rmref(params);
}

void test_eval_bind_all_handles_rest_param() {
  cell* params = read("(x ... y)");
  cell* args = read("(a b)");
  cell* scope = mkref(new_table());
  set(scope, "a", new_num(3));
  set(scope, "b", new_num(4));
  cell* new_scope = mkref(new_table());
  eval_bind_all(params, args, scope, new_scope);
  // {x: 3, y: (4)}
  CHECK_EQ(unsafe_get(new_scope, "x"), new_num(3));
  CHECK_EQ(car(unsafe_get(new_scope, "y")), new_num(4));
  CHECK_EQ(cdr(unsafe_get(new_scope, "y")), nil);
  rmref(new_scope);
  rmref(scope);
  rmref(args);
  rmref(params);
}

void test_eval_bind_all_handles_quoted_rest_param() {
  cell* params = read("(x ... 'y)");
  cell* args = read("(a b)");
  cell* scope = mkref(new_table());
  set(scope, "a", new_num(3));
  set(scope, "b", new_num(4));
  cell* new_scope = mkref(new_table());
  eval_bind_all(params, args, scope, new_scope);
  // {x: 3, y: (b)}
  CHECK_EQ(unsafe_get(new_scope, "x"), new_num(3));
  CHECK_EQ(car(unsafe_get(new_scope, "y")), new_sym("b"));
  CHECK_EQ(cdr(unsafe_get(new_scope, "y")), nil);
  rmref(new_scope);
  rmref(scope);
  rmref(args);
  rmref(params);
}

void test_eval_bind_all_handles_destructured_params() {
  cell* params = read("((a b))");
  cell* args = read("(`(,x ,y))");
  cell* scope = mkref(new_table());
  set(scope, "x", new_num(3));
  set(scope, "y", new_num(4));
  cell* new_scope = mkref(new_table());
  eval_bind_all(params, args, scope, new_scope);
  // {a: 3, b: 4}
  CHECK_EQ(unsafe_get(new_scope, "a"), new_num(3));
  CHECK_EQ(unsafe_get(new_scope, "b"), new_num(4));
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



cell* process_unquotes(cell* x, long depth) {
  return process_unquotes(x, depth, Curr_lexical_scope);
}

void test_process_unquotes_handles_unquote() {
  new_dynamic_scope("a", new_num(3));
  cell* expr = read("(,a)");
  cell* result = process_unquotes(expr, 1);
  CHECK(is_cons(result));
  // (3)
  CHECK_EQ(car(result), new_num(3));
  CHECK_EQ(cdr(result), nil);
  rmref(result);
  rmref(expr);
  end_dynamic_scope("a");
}

void test_process_unquotes_handles_unquote_splice() {
  new_dynamic_scope("a", new_cons(new_num(3)));
  cell* expr = read("(,@a)");
  cell* result = process_unquotes(expr, 1);
  CHECK(is_cons(result));
  // (3)
  CHECK_EQ(car(result), new_num(3));
  CHECK_EQ(cdr(result), nil);
  rmref(result);
  rmref(expr);
  end_dynamic_scope("a");
}

void test_process_unquotes_handles_unquote_splice_and_unquote() {
  new_dynamic_scope("a", new_cons(new_num(3)));
  new_dynamic_scope("b", new_cons(new_num(4)));
  cell* expr = read("(,@a ,b)");
  cell* result = process_unquotes(expr, 1);
  // (3 (4))
  CHECK(is_cons(result));
  CHECK_EQ(car(result), new_num(3));
  CHECK(is_cons(car(cdr(result))));
  CHECK_EQ(car(car(cdr(result))), new_num(4));
  CHECK_EQ(cdr(car(cdr(result))), nil);
  CHECK_EQ(cdr(cdr(result)), nil);
  rmref(result);
  rmref(expr);
  end_dynamic_scope("b");
  end_dynamic_scope("a");
}

void test_process_unquotes_splices_copies_of_lists() {
  new_dynamic_scope("a", new_cons(new_num(3)));
  new_dynamic_scope("b", new_cons(new_num(4)));
  cell* expr = read("(,@a ,b)");
  cell* result = process_unquotes(expr, 1);
  CHECK(is_cons(result));
  CHECK(result != lookup("a"))
  rmref(result);
  rmref(expr);
  end_dynamic_scope("b");
  end_dynamic_scope("a");
}



void test_nil_evals_to_itself() {
  cell* expr = read("()");
  cell* result = eval(expr);
  CHECK_EQ(result, nil);
  rmref(result);
  rmref(expr);
}

void test_num_evals_to_itself() {
  cell* expr = read("34");
  cell* result = eval(expr);
  CHECK_EQ(result, expr);
  rmref(result);
  rmref(expr);
}

void test_colonsym_evals_to_itself() {
  cell* expr = read(":abc");
  cell* result = eval(expr);
  CHECK_EQ(result, expr);
  rmref(result);
  rmref(expr);
}

void test_colon_evals() {
  cell* expr = read(":");
  new_dynamic_scope(":", nil);
  cell* result = eval(expr);
  CHECK_EQ(result, nil);
  end_dynamic_scope(":");
  rmref(expr);
}

void test_string_evals_to_itself() {
  cell* expr = read("\"ac bd\"");
  cell* result = eval(expr);
  CHECK_EQ(result, expr);
  rmref(result);
  rmref(expr);
}

void test_sym_evals_to_value() {
  new_dynamic_scope("a", new_num(34));
  cell* expr = read("a");
  cell* result = eval(expr);
  CHECK_EQ(result, new_num(34));
  rmref(result);
  rmref(expr);
  end_dynamic_scope("a");
}

void test_sym_evals_to_itself() {
  new_dynamic_scope("a", new_sym("a"));
  cell* expr = read("a");
  cell* result = eval(expr);
  CHECK_EQ(result, expr);
  rmref(result);
  rmref(expr);
  end_dynamic_scope("a");
}

void test_eval_on_incomplete_eval_retries() {
  cell* expr = read("a");
  cell* incomplete_result = eval(expr);
  CHECK(is_object(incomplete_result));
  CHECK_EQ(type(incomplete_result), sym_incomplete_eval);
  new_dynamic_scope("a", new_num(34));
  cell* doubly_evald_result = eval(incomplete_result);
  CHECK_EQ(doubly_evald_result, new_num(34));
  rmref(doubly_evald_result);
  end_dynamic_scope("a");
  rmref(incomplete_result);
  rmref(expr);
}

void test_object_expr_evals_to_itself() {
  cell* expr = read("(object foo 4)");
  cell* result = eval(expr);
  CHECK_EQ(result, expr);
  rmref(result);
  rmref(expr);
}

void test_eval_handles_quoted_atoms() {
  cell* expr = read("'a");
  cell* result = eval(expr);
  CHECK_EQ(result, new_sym("a"));
  rmref(result);
  rmref(expr);

  expr = read("'34");
  result = eval(expr);
  CHECK_EQ(result, new_num(34));
  rmref(result);
  rmref(expr);
}

void test_eval_handles_quoted_lists() {
  cell* expr = read("'(a b)");
  cell* result = eval(expr);
  // (a b)
  CHECK_EQ(car(result), new_sym("a"));
  CHECK_EQ(car(cdr(result)), new_sym("b"));
  CHECK_EQ(cdr(cdr(result)), nil);
  rmref(result);
  rmref(expr);
}

void test_eval_handles_backquoted_lists() {
  cell* expr = read("`(a b)");
  cell* result = eval(expr);
  // (a b)
  CHECK_EQ(car(result), new_sym("a"));
  CHECK_EQ(car(cdr(result)), new_sym("b"));
  CHECK_EQ(cdr(cdr(result)), nil);
  rmref(result);
  rmref(expr);
}

void test_eval_handles_unquote() {
  cell* expr = read("`(a ,b)");
  new_dynamic_scope("b", new_num(34));
  cell* result = eval(expr);
  // (a 34)
  CHECK_EQ(car(result), new_sym("a"));
  CHECK_EQ(car(cdr(result)), new_num(34));
  CHECK_EQ(cdr(cdr(result)), nil);
  rmref(result);
  end_dynamic_scope("b");
  rmref(expr);
}

void test_eval_handles_unquote_splice() {
  cell* expr = read("`(a ,@b)");
  cell* val = read("(34 35)");
  new_dynamic_scope("b", val);
  cell* result = eval(expr);
  // (a 34 35)
  CHECK_EQ(car(result), new_sym("a"));
  CHECK_EQ(car(cdr(result)), new_num(34));
  CHECK_EQ(car(cdr(cdr(result))), new_num(35));
  CHECK_EQ(cdr(cdr(cdr(result))), nil);
  rmref(result);
  end_dynamic_scope("b");
  rmref(val);
  rmref(expr);
}

void test_eval_handles_unquote_splice_of_nil() {
  cell* expr = read("`(a ,@b 3)");
  new_dynamic_scope("b", nil);
  cell* result = eval(expr);
  CHECK_EQ(cdr(nil), nil);
  // (a 3)
  CHECK_EQ(car(result), new_sym("a"));
  CHECK_EQ(car(cdr(result)), new_num(3));
  CHECK_EQ(cdr(cdr(result)), nil);
  rmref(result);
  end_dynamic_scope("b");
  rmref(expr);
}

void test_eval_quotes_quote_comma() {
  cell* expr = read("`(a ',b)");
  new_dynamic_scope("b", new_sym("x"));
  cell* result = eval(expr);
  // (a 'x)
  CHECK_EQ(car(result), new_sym("a"));
  CHECK(is_cons(car(cdr(result))));
  CHECK_EQ(car(car(cdr(result))), new_sym("'"));
  CHECK_EQ(cdr(car(cdr(result))), new_sym("x"));
  CHECK_EQ(cdr(cdr(result)), nil);
  rmref(result);
  end_dynamic_scope("b");
  rmref(expr);
}

void test_eval_evals_comma_quote() {
  cell* expr = read("`(a ,'b)");
  new_dynamic_scope("b", new_sym("x"));
  cell* result = eval(expr);
  // (a b)
  CHECK_EQ(car(result), new_sym("a"));
  CHECK_EQ(car(cdr(result)), new_sym("b"));
  CHECK_EQ(cdr(cdr(result)), nil);
  rmref(result);
  end_dynamic_scope("b");
  rmref(expr);
}

void test_eval_handles_nested_quotes() {
  cell* expr = read("`(,a `(,a ,,a ,,@b))");
  new_dynamic_scope("a", new_sym("x"));
  new_dynamic_scope("b", new_cons(new_sym("x"), new_cons(new_sym("y"))));  // (x y)
  cell* result = eval(expr);
  // (x `(,a x x y))
  CHECK_EQ(car(result), new_sym("x"));
  cell* nested_expr = car(cdr(result));
  CHECK(is_cons(nested_expr));
  CHECK_EQ(car(nested_expr), new_sym("`"));
  CHECK(is_cons(cdr(nested_expr)));
  cell* nested_expr2 = cdr(nested_expr);
  CHECK(is_cons(car(nested_expr2)));
  CHECK_EQ(car(car(nested_expr2)), new_sym(","));
  CHECK_EQ(cdr(car(nested_expr2)), new_sym("a"));
  nested_expr2 = cdr(nested_expr2);
  CHECK_EQ(car(nested_expr2), new_sym("x"));
  nested_expr2 = cdr(nested_expr2);
  CHECK_EQ(car(nested_expr2), new_sym("x"));
  CHECK_EQ(car(cdr(nested_expr2)), new_sym("y"));
  CHECK_EQ(cdr(cdr(nested_expr2)), nil);
  rmref(result);
  end_dynamic_scope("b");
  end_dynamic_scope("a");
  rmref(expr);
}

void test_eval_handles_quoted_destructured_params() {
  cell* call = read("((fn ('(a b)) b) (1 2))");
  cell* result = eval(call);
  CHECK(is_num(result));
  CHECK_EQ(to_int(result), 2);
  rmref(result);
  rmref(call);
}

void test_eval_handles_rest_params() {
  cell* call = read("((fn (a b ... c) c) 1 2 3 4 5)");
  cell* result = eval(call);
  CHECK(is_cons(result));
  CHECK(is_num(car(result)));
  // (3 4 5)
  CHECK_EQ(to_int(car(result)), 3);
  CHECK(is_num(car(cdr(result))));
  CHECK_EQ(to_int(car(cdr(result))), 4);
  CHECK_EQ(to_int(car(cdr(cdr(result)))), 5);
  CHECK_EQ(cdr(cdr(cdr(result))), nil);
  rmref(result);
  rmref(call);
}

void test_eval_handles_splice() {
  cell* expr = read("(cons @b)");
  cell* val = read("(3 4)");
  new_dynamic_scope("b", val);
  cell* result = eval(expr);
  // (3 ... 4)
  CHECK(is_cons(result));
  CHECK_EQ(car(result), new_num(3));
  CHECK_EQ(cdr(result), new_num(4));
  rmref(result);
  end_dynamic_scope("b");
  rmref(val);
  rmref(expr);
}

void test_eval_handles_splice2() {
  cell* fn = read("(fn x (cons @x))");
  cell* def = eval(fn);
  new_dynamic_scope("f", def);
  cell* call1 = read("(f 1 2)");
  cell* result = eval(call1);
  // (1 ... 2)
  CHECK(is_cons(result));
  CHECK_EQ(car(result), new_num(1));
  CHECK_EQ(cdr(result), new_num(2));
  rmref(result);

  cell* call2 = read("(f 3 4)");
  result = eval(call2);
  // (3 ... 4)
  CHECK(is_cons(result));
  CHECK_EQ(car(result), new_num(3));
  CHECK_EQ(cdr(result), new_num(4));
  rmref(result);

  rmref(call2);
  rmref(call1);
  end_dynamic_scope("f");
  rmref(def);
  rmref(fn);
}

void test_eval_handles_splice3() {
  cell* fn = read("(fn (x y) (cons x y))");
  cell* def = eval(fn);
  new_dynamic_scope("f", def);
  new_dynamic_scope("a", new_num(3));
  new_dynamic_scope("b", new_num(4));
  cell* argval = read("(a b)");
  new_dynamic_scope("args", argval);
  cell* call = read("(f @args)");
  cell* result = eval(call);
  // (a ... b)
  CHECK(is_cons(result));
  CHECK_EQ(car(result), new_sym("a"));
  CHECK_EQ(cdr(result), new_sym("b"));
  rmref(result);
  rmref(call);
  end_dynamic_scope("args");
  rmref(argval);
  end_dynamic_scope("b");
  end_dynamic_scope("a");
  end_dynamic_scope("f");
  rmref(def);
  rmref(fn);
}

void test_eval_handles_splice4() {
  cell* fn = read("(fn ('x y) (cons x y))");
  cell* def = eval(fn);
  new_dynamic_scope("f", def);
  new_dynamic_scope("a", new_num(3));
  new_dynamic_scope("b", new_num(4));
  cell* argval = read("(b)");
  new_dynamic_scope("args", argval);
  cell* call = read("(f a @args)");
  cell* result = eval(call);
  // (a ... b)
  CHECK(is_cons(result));
  CHECK_EQ(car(result), new_sym("a"));
  CHECK_EQ(cdr(result), new_sym("b"));
  rmref(result);
  rmref(call);
  end_dynamic_scope("args");
  rmref(argval);
  end_dynamic_scope("b");
  end_dynamic_scope("a");
  end_dynamic_scope("f");
  rmref(def);
  rmref(fn);
}

void test_eval_handles_splice5() {
  cell* fn = read("(fn (x y) (cons x y))");
  cell* def = eval(fn);
  new_dynamic_scope("f", def);
  new_dynamic_scope("a", new_num(3));
  new_dynamic_scope("b", new_num(4));
  cell* argval = read("(b)");
  new_dynamic_scope("args", argval);
  cell* call = read("(f a @args)");
  cell* result = eval(call);
  // (3 ... b)
  CHECK(is_cons(result));
  CHECK_EQ(car(result), new_num(3));
  CHECK_EQ(cdr(result), new_sym("b"));
  rmref(result);
  rmref(call);
  end_dynamic_scope("args");
  rmref(argval);
  end_dynamic_scope("b");
  end_dynamic_scope("a");
  end_dynamic_scope("f");
  rmref(def);
  rmref(fn);
}

void test_eval_handles_splice6() {
  cell* fn = read("(fn (x 'y) (cons x y))");
  cell* def = eval(fn);
  new_dynamic_scope("f", def);
  new_dynamic_scope("a", new_num(3));
  new_dynamic_scope("b", new_num(4));
  cell* argval = read("(a b)");
  new_dynamic_scope("args", argval);
  cell* call = read("(f @args)");
  cell* result = eval(call);
  // (a ... b)
  CHECK(is_cons(result));
  CHECK_EQ(car(result), new_sym("a"));
  CHECK_EQ(cdr(result), new_sym("b"));
  rmref(result);
  rmref(call);
  end_dynamic_scope("args");
  rmref(argval);
  end_dynamic_scope("b");
  end_dynamic_scope("a");
  end_dynamic_scope("f");
  rmref(def);
  rmref(fn);
}

void test_eval_splice_on_macros_warns() {
  cell* expr = read("(fn '(x y) (eval (cons 'cons (cons x (cons y nil))) caller_scope))");
  cell* fn = eval(expr);
  new_dynamic_scope("f", fn);
  new_dynamic_scope("a", new_num(3));
  new_dynamic_scope("b", new_num(4));
  cell* argval = read("(a b)");
  new_dynamic_scope("args", argval);
  cell* call = read("(f @args)");
  cell* result = eval(call);
  CHECK_EQ(Raise_count, 1);   Raise_count=0;
  rmref(result);
  rmref(call);
  end_dynamic_scope("args");
  rmref(argval);
  end_dynamic_scope("b");
  end_dynamic_scope("a");
  end_dynamic_scope("f");
  rmref(fn);
  rmref(expr);
}

void test_eval_splice_on_macros_with_backquote() {
  cell* expr = read("(fn '(x y) (eval `(cons ,x ,y) caller_scope))");
  cell* fn = eval(expr);
  new_dynamic_scope("f", fn);
  new_dynamic_scope("a", new_num(3));
  new_dynamic_scope("b", new_num(4));
  cell* argval = read("(a b)");
  new_dynamic_scope("args", argval);
  cell* call = read("(f @args)");
  cell* result = eval(call);
  CHECK_EQ(Raise_count, 0);
  rmref(result);
  rmref(call);
  end_dynamic_scope("args");
  rmref(argval);
  end_dynamic_scope("b");
  end_dynamic_scope("a");
  end_dynamic_scope("f");
  rmref(fn);
  rmref(expr);
}

void test_eval_handles_simple_fn() {
  cell* expr = read("(fn () 34)");
  cell* fn = eval(expr);
  // (object function {sig: nil, body: (34), env: nil})
  CHECK_EQ(type(fn), new_sym("function"));
  CHECK_EQ(sig(fn), nil);
  CHECK(is_cons(body(fn)));
  CHECK_EQ(car(body(fn)), new_num(34));
  CHECK_EQ(env(fn), nil);
  rmref(fn);
  rmref(expr);
}

void test_eval_on_fn_is_idempotent() {
  cell* expr = read("(fn () 34)");
  cell* fn = eval(expr);
  cell* fn2 = eval(fn);
  // fn == fn2
  CHECK_EQ(type(fn2), new_sym("function"));
  CHECK_EQ(sig(fn2), nil);
  CHECK(is_cons(body(fn2)));
  CHECK_EQ(car(body(fn2)), new_num(34));
  CHECK_EQ(env(fn2), nil);
  rmref(fn2);
  rmref(fn);
  rmref(expr);
}

void test_eval_handles_closure() {
  cell* expr = read("(fn () 34)");
  new_lexical_scope();
    cell* new_lexical_scope = Curr_lexical_scope;
    CHECK_EQ(new_lexical_scope->nrefs, 1);
    cell* result = eval(expr);
    CHECK_EQ(new_lexical_scope->nrefs, 2);
  end_lexical_scope();
  CHECK_EQ(new_lexical_scope->nrefs, 1);
  // (object function {sig: nil, body: (34), env: {}})
  CHECK_EQ(type(result), new_sym("function"));
  CHECK_EQ(sig(result), nil);
  CHECK_EQ(car(body(result)), new_num(34));
  CHECK_EQ(env(result), new_lexical_scope);
  rmref(result);
  CHECK_EQ(new_lexical_scope->nrefs, 0);
  rmref(expr);
}

void test_eval_handles_fn_calls() {
  cell* call = read("((fn () 34))");
  cell* result = eval(call);
  CHECK_EQ(result, new_num(34));
  rmref(result);
  rmref(call);
}

void test_eval_expands_syms_in_fn_bodies() {
  cell* fn = read("((fn () a))");
  new_dynamic_scope("a", new_num(34));
  cell* result = eval(fn);
  CHECK_EQ(result, new_num(34));
  end_dynamic_scope("a");
  rmref(result);
  rmref(fn);
}

void test_eval_handles_assigned_fn_calls() {
  cell* fn = read("(fn () 34)");
  cell* f = eval(fn);
  new_dynamic_scope("f", f);
    cell* call = read("(f)");
    cell* result = eval(call);
    CHECK_EQ(result, new_num(34));
  end_dynamic_scope("f");
  rmref(result);
  rmref(call);
  rmref(f);
  rmref(fn);
}

void test_eval_expands_lexically_scoped_syms_in_fn_bodies() {
  cell* call = read("((fn () a))");
  new_lexical_scope();
    add_lexical_binding("a", new_num(34));
    cell* result = eval(call);
    CHECK_EQ(result, new_num(34));
  end_lexical_scope();
  rmref(result);
  rmref(call);
}

void test_eval_expands_syms_in_original_lexical_scope() {
  new_dynamic_scope("a", new_num(23));
  cell* fn = read("(fn () a)");
  new_lexical_scope();
  add_lexical_binding("a", new_num(34));
    cell* f = eval(fn);
    new_dynamic_scope("f", f);
  end_lexical_scope();
  cell* call = read("(f)");
  cell* result = eval(call);
  CHECK_EQ(result, new_num(34));
  rmref(result);
  rmref(call);
  rmref(f);
  rmref(fn);
  end_dynamic_scope("f");
  end_dynamic_scope("a");
}

void test_eval_expands_args_in_caller_scope() {
  new_dynamic_scope("a", new_num(23));
  cell* fn = read("(fn (arg1) arg1)");
  new_lexical_scope();
  add_lexical_binding("arg1", new_num(34));
    cell* f = eval(fn);
    new_dynamic_scope("f", f);
  end_lexical_scope();
  cell* call = read("(f a)");
  cell* result = eval(call);
  CHECK_EQ(result, new_num(23));
  rmref(result);
  rmref(call);
  rmref(f);
  rmref(fn);
  end_dynamic_scope("f");
  end_dynamic_scope("a");
}

void test_eval_doesnt_eval_quoted_params() {
  new_dynamic_scope("a", new_num(23));
  cell* fn = read("(fn ('arg1) arg1)");
  new_lexical_scope();
  add_lexical_binding("arg1", new_num(34));
    cell* f = eval(fn);
    new_dynamic_scope("f", f);
  end_lexical_scope();
  cell* call = read("(f a)");
  cell* result = eval(call);
  CHECK_EQ(result, new_sym("a"));
  rmref(result);
  rmref(call);
  rmref(f);
  rmref(fn);
  end_dynamic_scope("f");
  end_dynamic_scope("a");
}

void test_eval_handles_quoted_param_list() {
  new_dynamic_scope("a", new_num(23));
  cell* fn = read("(fn '(arg1) arg1)");
  new_lexical_scope();
  add_lexical_binding("arg1", new_num(34));
    cell* f = eval(fn);
    new_dynamic_scope("f", f);
  end_lexical_scope();
  cell* call = read("(f a)");
  cell* result = eval(call);
  CHECK_EQ(result, new_sym("a"));
  rmref(result);
  rmref(call);
  rmref(f);
  rmref(fn);
  end_dynamic_scope("f");
  end_dynamic_scope("a");
}

void test_eval_handles_multiple_args() {
  cell* fn = read("(fn (a b) b)");
  cell* f = eval(fn);
  new_dynamic_scope("f", f);
  cell* call = read("(f 1 2)");
  cell* result = eval(call);
  CHECK_EQ(result, new_num(2));
  rmref(result);
  rmref(call);
  rmref(f);
  rmref(fn);
  end_dynamic_scope("f");
}

void test_eval_handles_multiple_body_exprs() {
  cell* fn = read("(fn () 1 2)");
  cell* f = eval(fn);
  new_dynamic_scope("f", f);
  cell* call = read("(f)");
  cell* result = eval(call);
  CHECK_EQ(result, new_num(2));
  rmref(result);
  rmref(call);
  rmref(f);
  rmref(fn);
  end_dynamic_scope("f");
}

void test_eval_handles_vararg_param() {
  cell* call = read("((fn args args) 1)");
  cell* result = eval(call);
  CHECK(is_cons(result));
  CHECK_EQ(car(result), new_num(1));
  rmref(result);
  rmref(call);
}

void test_eval_evals_args() {
  cell* call = read("((fn (f) (f)) (fn () 34))");
  cell* result = eval(call);
  CHECK(is_num(result));
  CHECK_EQ(to_int(result), 34);
  rmref(result);
  rmref(call);
}

void test_eval_doesnt_leak_body_evals() {
  cell* call = read("((fn (f) (f) (f)) (fn () 34))");
  cell* result = eval(call);
  CHECK(is_num(result));
  CHECK_EQ(to_int(result), 34);
  rmref(result);
  rmref(call);
}

void test_eval_handles_destructured_params() {
  cell* call = read("((fn ((a b)) b) '(1 2))");
  cell* result = eval(call);
  CHECK(is_num(result));
  CHECK_EQ(to_int(result), 2);
  rmref(result);
  rmref(call);
}

void test_eval_handles_keyword_args_for_fns() {
  cell* fn = read("(fn (a b c) c)");
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

void test_eval_handles_quoted_keyword_args_for_fns() {
  cell* fn = read("(fn (a b 'c) c)");
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

void test_eval_handles_quoted_keyword_args_for_fns2() {
  cell* fn = read("(fn '(a b) b)");
  cell* f = eval(fn);
  new_dynamic_scope("f", f);
  cell* call = read("(f :b 1 2)");
  cell* result = eval(call);
  CHECK_EQ(result, new_num(1));
  rmref(result);
  rmref(call);
  rmref(f);
  rmref(fn);
  end_dynamic_scope("f");
}

void test_eval_handles_rest_keyword_arg_at_end() {
  cell* fn = read("(fn (a ... b) b)");
  cell* f = eval(fn);
  new_dynamic_scope("f", f);
  cell* call = read("(f 2 :b 1 3)");
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

void test_eval_handles_rest_keyword_arg_at_end2() {
  cell* fn = read("(fn (a ... b) b)");
  cell* f = eval(fn);
  new_dynamic_scope("f", f);
  cell* call = read("(f :b 1 2 3)");
  cell* result = eval(call);
  // (1 2 3)
  CHECK_EQ(car(result), new_num(1));
  CHECK_EQ(car(cdr(result)), new_num(2));
  CHECK_EQ(car(cdr(cdr(result))), new_num(3));
  CHECK_EQ(cdr(cdr(cdr(result))), nil);
  rmref(result);
  rmref(call);
  rmref(f);
  rmref(fn);
  end_dynamic_scope("f");
}

void test_eval_handles_args_after_rest_keyword() {
  cell* fn = read("(fn (a|with ... b|over) b)");
  cell* f = eval(fn);
  new_dynamic_scope("f", f);
  cell* call = read("(f :over 1 2 :with 3)");
  cell* result = eval(call);
  // (1 2)
  CHECK_EQ(car(result), new_num(1));
  CHECK_EQ(car(cdr(result)), new_num(2));
  CHECK_EQ(cdr(cdr(result)), nil);
  rmref(result);
  rmref(call);
  rmref(f);
  rmref(fn);
  end_dynamic_scope("f");
}

void test_eval_handles_quoted_rest_keyword_arg() {
  cell* fn = read("(fn (a ... 'b) b)");
  cell* f = eval(fn);
  new_dynamic_scope("f", f);
  cell* call = read("(f :b 1 2 3)");
  cell* result = eval(call);
  // (1 2 3)
  CHECK_EQ(car(result), new_num(1));
  CHECK_EQ(car(cdr(result)), new_num(2));
  CHECK_EQ(car(cdr(cdr(result))), new_num(3));
  CHECK_EQ(cdr(cdr(cdr(result))), nil);
  rmref(result);
  rmref(call);
  rmref(f);
  rmref(fn);
  end_dynamic_scope("f");
}

void test_eval_handles_non_keyword_arg_colon_syms() {
  cell* fn = read("(fn (a b) b)");
  cell* f = eval(fn);
  new_dynamic_scope("f", f);
  cell* call = read("(f :x 1)");
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

void test_eval_handles_keyword_args_inside_splice() {
  cell* fn = read("(fn (a b) b)");
  cell* f = eval(fn);
  new_dynamic_scope("f", f);
  cell* call = read("(f @'(3 :a 4))");
  cell* result = eval(call);
  CHECK_EQ(result, new_num(3));
  rmref(result);
  rmref(call);
  rmref(f);
  rmref(fn);
  end_dynamic_scope("f");
}

void test_eval_handles_keyword_args_inside_destructured_params() {
  cell* fn = read("(fn ((a b)) b)");
  cell* f = eval(fn);
  new_dynamic_scope("f", f);
  cell* call = read("(f '(3 :a 4))");
  cell* result = eval(call);
  CHECK_EQ(result, new_num(3));
  rmref(result);
  rmref(call);
  rmref(f);
  rmref(fn);
  end_dynamic_scope("f");
}

void test_eval_handles_unknown_call() {
  cell* expr = read("(f 3)");
  cell* attempt1 = eval(expr);
  CHECK(is_incomplete_eval(attempt1));
  cell* fn = read("(fn (a) a)");
  cell* f = eval(fn);
  new_dynamic_scope("f", f);
  cell* attempt2 = eval(attempt1);
  CHECK_EQ(attempt2, new_num(3));
  rmref(attempt2);
  end_dynamic_scope("f");
  rmref(f);
  rmref(fn);
  rmref(attempt1);
  rmref(expr);
}

void test_eval_handles_unknown_arg() {
  cell* fn = read("(fn (a) a)");
  cell* f = eval(fn);
  new_dynamic_scope("f", f);
  cell* call = read("(f a)");
  cell* attempt1 = eval(call);
  // `(object incomplete_eval (,f a))
  CHECK(is_incomplete_eval(attempt1));
  CHECK(is_cons(rep(attempt1)));
  CHECK_EQ(car(rep(attempt1)), f);
  CHECK_EQ(car(cdr(rep(attempt1))), new_sym("a"));
  CHECK_EQ(cdr(cdr(rep(attempt1))), nil);
  rmref(attempt1);
  rmref(call);
  end_dynamic_scope("f");
  rmref(f);
  rmref(fn);
}

void test_eval_handles_known_and_unknown_args() {
  cell* fn = read("(fn (a b) b)");
  cell* f = eval(fn);
  new_dynamic_scope("f", f);
  new_dynamic_scope("a", new_num(3));
  cell* call = read("(f a b)");
  cell* attempt1 = eval(call);
  // `(object incomplete_eval (,f ''3 b))
  CHECK(is_incomplete_eval(attempt1));
  CHECK(is_cons(rep(attempt1)));
  CHECK_EQ(car(rep(attempt1)), f);
  CHECK(is_cons(car(cdr(rep(attempt1)))));
  CHECK_EQ(car(car(cdr(rep(attempt1)))), sym_already_evald);
  CHECK_EQ(cdr(car(cdr(rep(attempt1)))), new_num(3));
  CHECK_EQ(car(cdr(cdr(rep(attempt1)))), new_sym("b"));
  CHECK_EQ(cdr(cdr(cdr(rep(attempt1)))), nil);
  rmref(attempt1);
  rmref(call);
  end_dynamic_scope("a");
  end_dynamic_scope("f");
  rmref(f);
  rmref(fn);
}

void test_eval_handles_quoted_and_unknown_args() {
  cell* fn = read("(fn ('a b) b)");
  cell* f = eval(fn);
  new_dynamic_scope("f", f);
  cell* call = read("(f a b)");
  cell* attempt1 = eval(call);
  // `(object incomplete_eval (,f ''a b))
  CHECK(is_incomplete_eval(attempt1));
  CHECK(is_cons(rep(attempt1)));
  CHECK_EQ(car(rep(attempt1)), f);
  CHECK(is_cons(car(cdr(rep(attempt1)))));
  CHECK_EQ(car(car(cdr(rep(attempt1)))), sym_already_evald);
  CHECK_EQ(cdr(car(cdr(rep(attempt1)))), new_sym("a"));
  CHECK_EQ(car(cdr(cdr(rep(attempt1)))), new_sym("b"));
  CHECK_EQ(cdr(cdr(cdr(rep(attempt1)))), nil);
  rmref(attempt1);
  rmref(call);
  end_dynamic_scope("f");
  rmref(f);
  rmref(fn);
}

void test_eval_handles_unknown_destructured_args() {
  cell* fn = read("(fn ((a b)) b)");
  cell* f = eval(fn);
  new_dynamic_scope("f", f);
  cell* call = read("(f args)");
  cell* attempt1 = eval(call);
  // `(object incomplete_eval (,f args))
  CHECK(is_incomplete_eval(attempt1));
  CHECK(is_cons(rep(attempt1)));
  CHECK_EQ(car(rep(attempt1)), f);
  CHECK_EQ(car(cdr(rep(attempt1))), new_sym("args"));
  CHECK_EQ(cdr(cdr(rep(attempt1))), nil);
  rmref(attempt1);
  rmref(call);
  end_dynamic_scope("f");
  rmref(f);
  rmref(fn);
}

void test_eval_handles_unknown_spliced_args() {
  cell* fn = read("(fn ((a b)) b)");
  cell* f = eval(fn);
  new_dynamic_scope("f", f);
  cell* call = read("(f @args)");
  cell* attempt1 = eval(call);
  // `(object incomplete_eval (,f @args))
  CHECK(is_incomplete_eval(attempt1));
  CHECK(is_cons(rep(attempt1)));
  CHECK_EQ(car(rep(attempt1)), f);
  cell* args = car(cdr(rep(attempt1)));
  CHECK_EQ(car(args), sym_splice);
  CHECK_EQ(cdr(args), new_sym("args"));
  rmref(attempt1);
  rmref(call);
  end_dynamic_scope("f");
  rmref(f);
  rmref(fn);
}

void test_eval_handles_literal_incomplete_args() {
  cell* fn = read("(fn (x) x)");
  cell* f = eval(fn);
  new_dynamic_scope("f", f);
  cell* call = read("(f (object incomplete_eval 34))");
  cell* attempt1 = eval(call);
  CHECK_EQ(attempt1, new_num(34));
  rmref(attempt1);
  rmref(call);
  end_dynamic_scope("f");
  rmref(f);
  rmref(fn);
}

void test_eval_handles_incomplete_args_for_aliased_params() {
  cell* fn = read("(fn (x|y) x)");
  cell* f = eval(fn);
  new_dynamic_scope("f", f);
  cell* call = read("(f a)");
  cell* attempt1 = eval(call);
  // `(object incomplete_eval (,f a))
  CHECK(is_incomplete_eval(attempt1));
  CHECK(is_cons(rep(attempt1)));
  CHECK_EQ(car(rep(attempt1)), f);
  CHECK_EQ(car(cdr(rep(attempt1))), new_sym("a"));
  CHECK_EQ(cdr(cdr(rep(attempt1))), nil);
  rmref(attempt1);
  rmref(call);
  end_dynamic_scope("f");
  rmref(f);
  rmref(fn);
}
