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
