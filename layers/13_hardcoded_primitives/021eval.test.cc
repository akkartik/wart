bool contains(const list<cell*>& v, string sym) {
  return find(v.begin(), v.end(), new_sym(sym)) != v.end();
}

void test_eval_bind_all_handles_unquoted_param() {
  cell* params = read("(x)");
  cell* args = read("(a)");
  new_dynamic_scope("a", new_num(3));
  list<cell*> vars_bound;
  eval_bind_all(params, args, vars_bound);
  CHECK_EQ(vars_bound.size(), 1);
  CHECK(contains(vars_bound, "x"));
  CHECK_EQ(lookup("x"), new_num(3));
  end_dynamic_scope("x");
  end_dynamic_scope("a");
  rmref(args);
  rmref(params);
}

void test_eval_bind_all_binds_missing_params() {
  cell* params = read("(x y)");
  cell* args = read("(a)");
  new_dynamic_scope("a", new_num(3));
  list<cell*> vars_bound;
  eval_bind_all(params, args, vars_bound);
  CHECK_EQ(lookup("x"), new_num(3));
  CHECK_EQ(lookup("y"), nil);
  CHECK_EQ(vars_bound.size(), 2);
  CHECK(contains(vars_bound, "x"));
  CHECK(contains(vars_bound, "y"));
  end_dynamic_scope("x");
  end_dynamic_scope("y");
  end_dynamic_scope("a");
  rmref(args);
  rmref(params);
}

void test_eval_bind_all_handles_quoted_param() {
  cell* params = read("('x)");
  cell* args = read("(a)");
  list<cell*> vars_bound;
  eval_bind_all(params, args, vars_bound);
  CHECK_EQ(vars_bound.size(), 1);
  CHECK(contains(vars_bound, "x"));
  CHECK_EQ(lookup("x"), new_sym("a"));
  end_dynamic_scope("x");
  rmref(args);
  rmref(params);
}

void test_eval_bind_all_handles_varargs_param() {
  cell* params = read("x");
  cell* args = read("(a b)");
  new_dynamic_scope("a", new_num(3));
  new_dynamic_scope("b", new_num(4));
  list<cell*> vars_bound;
  eval_bind_all(params, args, vars_bound);
  CHECK_EQ(vars_bound.size(), 1);
  CHECK(contains(vars_bound, "x"));
  // {x: (3 4)}
  CHECK_EQ(car(lookup("x")), new_num(3));
  CHECK_EQ(car(cdr(lookup("x"))), new_num(4));
  CHECK_EQ(cdr(cdr(lookup("x"))), nil);
  end_dynamic_scope("x");
  end_dynamic_scope("a");
  end_dynamic_scope("b");
  rmref(args);
  rmref(params);
}

void test_eval_bind_all_handles_quoted_varargs_param() {
  cell* params = read("'x");
  cell* args = read("(a b)");
  new_dynamic_scope("a", new_num(3));
  new_dynamic_scope("b", new_num(4));
  list<cell*> vars_bound;
  eval_bind_all(params, args, vars_bound);
  CHECK_EQ(vars_bound.size(), 1);
  CHECK(contains(vars_bound, "x"));
  // {x: (a b)}
  CHECK_EQ(car(lookup("x")), new_sym("a"));
  CHECK_EQ(car(cdr(lookup("x"))), new_sym("b"));
  CHECK_EQ(cdr(cdr(lookup("x"))), nil);
  end_dynamic_scope("x");
  end_dynamic_scope("a");
  end_dynamic_scope("b");
  rmref(args);
  rmref(params);
}

void test_eval_bind_all_handles_rest_param() {
  cell* params = read("(x ... y)");
  cell* args = read("(a b)");
  new_dynamic_scope("a", new_num(3));
  new_dynamic_scope("b", new_num(4));
  list<cell*> vars_bound;
  eval_bind_all(params, args, vars_bound);
  // {x: 3, y: (4)}
  CHECK_EQ(lookup("x"), new_num(3));
  CHECK_EQ(car(lookup("y")), new_num(4));
  CHECK_EQ(cdr(lookup("y")), nil);
  end_dynamic_scope("x");
  end_dynamic_scope("y");
  end_dynamic_scope("a");
  end_dynamic_scope("b");
  rmref(args);
  rmref(params);
}

void test_eval_bind_all_handles_quoted_rest_param() {
  cell* params = read("(x ... 'y)");
  cell* args = read("(a b)");
  new_dynamic_scope("a", new_num(3));
  new_dynamic_scope("b", new_num(4));
  list<cell*> vars_bound;
  eval_bind_all(params, args, vars_bound);
  // {x: 3, y: (b)}
  CHECK_EQ(lookup("x"), new_num(3));
  CHECK_EQ(car(lookup("y")), new_sym("b"));
  CHECK_EQ(cdr(lookup("y")), nil);
  end_dynamic_scope("x");
  end_dynamic_scope("y");
  end_dynamic_scope("a");
  end_dynamic_scope("b");
  rmref(args);
  rmref(params);
}

void test_eval_bind_all_handles_destructured_params() {
  cell* params = read("((a b))");
  cell* args = read("((cons x (cons y)))");
  new_dynamic_scope("x", new_num(3));
  new_dynamic_scope("y", new_num(4));
  list<cell*> vars_bound;
  eval_bind_all(params, args, vars_bound);
  // {a: 3, b: 4}
  CHECK_EQ(lookup("a"), new_num(3));
  CHECK_EQ(lookup("b"), new_num(4));
  end_dynamic_scope("x");
  end_dynamic_scope("y");
  end_dynamic_scope("a");
  end_dynamic_scope("b");
  rmref(args);
  rmref(params);
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

void test_keyword_sym_evals_to_itself() {
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

void test_eval_handles_quoted_param_list() {
  new_dynamic_scope("a", new_num(23));
  cell* fn = read("(fn '(arg1) arg1)");
  cell* f = eval(fn);
  new_dynamic_scope("f", f);
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
