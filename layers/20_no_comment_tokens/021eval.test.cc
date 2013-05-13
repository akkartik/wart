bool contains(const list<Cell*>& v, string sym) {
  return find(v.begin(), v.end(), newSym(sym)) != v.end();
}

void test_evalBindAll_handles_unquoted_param() {
  Cell* params = read("(x)");
  Cell* args = read("(a)");
  newBinding("a", newNum(3));
  evalBindAll(params, args);
  checkEq(lookup("x"), newNum(3));
}

void test_evalBindAll_binds_missing_params() {
  Cell* params = read("(x y)");
  Cell* args = read("(a)");
  newBinding("a", newNum(3));
  evalBindAll(params, args);
  checkEq(lookup("x"), newNum(3));
  checkEq(lookup("y"), nil);
}

void test_evalBindAll_handles_varargs_param() {
  Cell* params = read("x");
  Cell* args = read("(a b)");
  newBinding("a", newNum(3));
  newBinding("b", newNum(4));
  evalBindAll(params, args);
  // {x: (3 4)}
  checkEq(car(lookup("x")), newNum(3));
  checkEq(car(cdr(lookup("x"))), newNum(4));
  checkEq(cdr(cdr(lookup("x"))), nil);
}

void test_evalBindAll_handles_rest_param() {
  Cell* params = read("(x ... y)");
  Cell* args = read("(a b)");
  newBinding("a", newNum(3));
  newBinding("b", newNum(4));
  evalBindAll(params, args);
  // {x: 3, y: (4)}
  checkEq(lookup("x"), newNum(3));
  checkEq(car(lookup("y")), newNum(4));
  checkEq(cdr(lookup("y")), nil);
}

void test_evalBindAll_handles_destructured_params() {
  Cell* params = read("((a b))");
  Cell* args = read("((cons x (cons y)))");
  newBinding("x", newNum(3));
  newBinding("y", newNum(4));
  evalBindAll(params, args);
  // {a: 3, b: 4}
  checkEq(lookup("a"), newNum(3));
  checkEq(lookup("b"), newNum(4));
}



void test_nil_evals_to_itself() {
  Cell* expr = read("()");
  Cell* result = eval(expr);
  checkEq(result, nil);
}

void test_num_evals_to_itself() {
  Cell* expr = read("34");
  Cell* result = eval(expr);
  checkEq(result, expr);
}

void test_colonsym_evals_to_itself() {
  Cell* expr = read(":abc");
  Cell* result = eval(expr);
  checkEq(result, expr);
}

void test_colon_evals() {
  Cell* expr = read(":");
  newBinding(":", nil);
  Cell* result = eval(expr);
  checkEq(result, nil);
}

void test_string_evals_to_itself() {
  Cell* expr = read("\"ac bd\"");
  Cell* result = eval(expr);
  checkEq(result, expr);
}

void test_sym_evals_to_value() {
  newBinding("a", newNum(34));
  Cell* expr = read("a");
  Cell* result = eval(expr);
  checkEq(result, newNum(34));
}

void test_sym_evals_to_itself() {
  newBinding("a", newSym("a"));
  Cell* expr = read("a");
  Cell* result = eval(expr);
  checkEq(result, expr);
}

void test_eval_handles_quoted_atoms() {
  Cell* expr = read("'a");
  Cell* result = eval(expr);
  checkEq(result, newSym("a"));

  expr = read("'34");
  result = eval(expr);
  checkEq(result, newNum(34));
}

void test_eval_handles_quoted_lists() {
  Cell* expr = read("'(a b)");
  Cell* result = eval(expr);
  // (a b)
  checkEq(car(result), newSym("a"));
  checkEq(car(cdr(result)), newSym("b"));
  checkEq(cdr(cdr(result)), nil);
}

void test_eval_handles_rest_params() {
  Cell* call = read("((fn (a b ... c) c) 1 2 3 4 5)");
  Cell* result = eval(call);
  check(isCons(result));
  check(isNum(car(result)));
  // (3 4 5)
  checkEq(toInt(car(result)), 3);
  check(isNum(car(cdr(result))));
  checkEq(toInt(car(cdr(result))), 4);
  checkEq(toInt(car(cdr(cdr(result)))), 5);
  checkEq(cdr(cdr(cdr(result))), nil);
}

void test_eval_handles_fn_calls() {
  Cell* call = read("((fn () 34))");
  Cell* result = eval(call);
  checkEq(result, newNum(34));
}

void test_eval_expands_syms_in_fn_bodies() {
  Cell* fn = read("((fn () a))");
  newBinding("a", newNum(34));
  Cell* result = eval(fn);
  checkEq(result, newNum(34));
}

void test_eval_handles_assigned_fn_calls() {
  Cell* fn = read("(fn () 34)");
  Cell* f = eval(fn);
  newBinding("f", f);
    Cell* call = read("(f)");
    Cell* result = eval(call);
    checkEq(result, newNum(34));
}

void test_eval_handles_multiple_args() {
  Cell* fn = read("(fn (a b) b)");
  Cell* f = eval(fn);
  newBinding("f", f);
  Cell* call = read("(f 1 2)");
  Cell* result = eval(call);
  checkEq(result, newNum(2));
}

void test_eval_handles_multiple_body_exprs() {
  Cell* fn = read("(fn () 1 2)");
  Cell* f = eval(fn);
  newBinding("f", f);
  Cell* call = read("(f)");
  Cell* result = eval(call);
  checkEq(result, newNum(2));
}

void test_eval_handles_vararg_param() {
  Cell* call = read("((fn args args) 1)");
  Cell* result = eval(call);
  check(isCons(result));
  checkEq(car(result), newNum(1));
}

void test_eval_evals_args() {
  Cell* call = read("((fn (f) (f)) (fn () 34))");
  Cell* result = eval(call);
  check(isNum(result));
  checkEq(toInt(result), 34);
}

void test_eval_doesnt_leak_body_evals() {
  Cell* call = read("((fn (f) (f) (f)) (fn () 34))");
  Cell* result = eval(call);
  check(isNum(result));
  checkEq(toInt(result), 34);
}

void test_eval_handles_destructured_params() {
  Cell* call = read("((fn ((a b)) b) '(1 2))");
  Cell* result = eval(call);
  check(isNum(result));
  checkEq(toInt(result), 2);
}
