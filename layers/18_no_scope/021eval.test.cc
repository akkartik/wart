void test_eval_bind_all_handles_unquoted_param() {
  cell* params = read("(x)");
  cell* args = read("(a)");
  newBinding("a", new_num(3));
  eval_bind_all(params, args);
  CHECK_EQ(lookup("x"), new_num(3));
  rmref(args);
  rmref(params);
}

void test_eval_bind_all_binds_missing_params() {
  cell* params = read("(x y)");
  cell* args = read("(a)");
  newBinding("a", new_num(3));
  eval_bind_all(params, args);
  CHECK_EQ(lookup("x"), new_num(3));
  CHECK_EQ(lookup("y"), nil);
  rmref(args);
  rmref(params);
}

void test_eval_bind_all_handles_varargs_param() {
  cell* params = read("x");
  cell* args = read("(a b)");
  newBinding("a", new_num(3));
  newBinding("b", new_num(4));
  eval_bind_all(params, args);
  // {x: (3 4)}
  CHECK_EQ(car(lookup("x")), new_num(3));
  CHECK_EQ(car(cdr(lookup("x"))), new_num(4));
  CHECK_EQ(cdr(cdr(lookup("x"))), nil);
  rmref(args);
  rmref(params);
}

void test_eval_bind_all_handles_rest_param() {
  cell* params = read("(x ... y)");
  cell* args = read("(a b)");
  newBinding("a", new_num(3));
  newBinding("b", new_num(4));
  eval_bind_all(params, args);
  // {x: 3, y: (4)}
  CHECK_EQ(lookup("x"), new_num(3));
  CHECK_EQ(car(lookup("y")), new_num(4));
  CHECK_EQ(cdr(lookup("y")), nil);
  rmref(args);
  rmref(params);
}

void test_eval_bind_all_handles_destructured_params() {
  cell* params = read("((a b))");
  cell* args = read("((cons x (cons y)))");
  newBinding("x", new_num(3));
  newBinding("y", new_num(4));
  eval_bind_all(params, args);
  // {a: 3, b: 4}
  CHECK_EQ(lookup("a"), new_num(3));
  CHECK_EQ(lookup("b"), new_num(4));
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

void test_colonsym_evals_to_itself() {
  cell* expr = read(":abc");
  cell* result = eval(expr);
  CHECK_EQ(result, expr);
  rmref(result);
  rmref(expr);
}

void test_colon_evals() {
  cell* expr = read(":");
  newBinding(":", nil);
  cell* result = eval(expr);
  CHECK_EQ(result, nil);
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
  newBinding("a", new_num(34));
  cell* expr = read("a");
  cell* result = eval(expr);
  CHECK_EQ(result, new_num(34));
  rmref(result);
  rmref(expr);
}

void test_sym_evals_to_itself() {
  newBinding("a", new_sym("a"));
  cell* expr = read("a");
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
  newBinding("a", new_num(34));
  cell* result = eval(fn);
  CHECK_EQ(result, new_num(34));
  rmref(result);
  rmref(fn);
}

void test_eval_handles_assigned_fn_calls() {
  cell* fn = read("(fn () 34)");
  cell* f = eval(fn);
  newBinding("f", f);
    cell* call = read("(f)");
    cell* result = eval(call);
    CHECK_EQ(result, new_num(34));
  rmref(result);
  rmref(call);
  rmref(f);
  rmref(fn);
}

void test_eval_handles_multiple_args() {
  cell* fn = read("(fn (a b) b)");
  cell* f = eval(fn);
  newBinding("f", f);
  cell* call = read("(f 1 2)");
  cell* result = eval(call);
  CHECK_EQ(result, new_num(2));
  rmref(result);
  rmref(call);
  rmref(f);
  rmref(fn);
}

void test_eval_handles_multiple_body_exprs() {
  cell* fn = read("(fn () 1 2)");
  cell* f = eval(fn);
  newBinding("f", f);
  cell* call = read("(f)");
  cell* result = eval(call);
  CHECK_EQ(result, new_num(2));
  rmref(result);
  rmref(call);
  rmref(f);
  rmref(fn);
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
