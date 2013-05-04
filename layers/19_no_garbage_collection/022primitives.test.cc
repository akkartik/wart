void test_fn_works() {
  Cell* fn = read("(fn(x) x)");
  Cell* result = eval(fn);
  // {sig: (x), body: (x)}
  checkEq(car(get(result, sym_sig)), newSym("x"));
  checkEq(cdr(get(result, sym_sig)), nil);
  checkEq(car(get(result, sym_body)), newSym("x"));
  checkEq(cdr(get(result, sym_body)), nil);
}



void test_if_sees_args_in_then_and_else() {
  Cell* fn = read("(fn(x) (if 34 x))");
  Cell* f = eval(fn);
  newBinding("f", f);
  Cell* call = read("(f 35)");
  Cell* result = eval(call);
  checkEq(result, newNum(35));
}

void test_not_works() {
  Cell* call = read("(not 35)");
  Cell* result = eval(call);
  checkEq(result, nil);
}

void test_not_works2() {
  Cell* call = read("(not nil)");
  Cell* result = eval(call);
  checkEq(result, newNum(1));
}

void test_cons_works() {
  Cell* call = read("(cons 1 2)");
  Cell* result = eval(call);
  // (1 ... 2)
  checkEq(car(result), newNum(1));
  checkEq(cdr(result), newNum(2));
}

void test_assign_to_non_sym_warns() {
  Cell* expr = read("(<- 3 nil)");
  eval(expr);
  checkEq(raiseCount, 1);   raiseCount=0;
}

void test_assign_lexical_var() {
  Cell* fn = read("((fn(x) (<- x 34) x))");
  Cell* call = eval(fn);
  checkEq(call, newNum(34));
}

void test_assign_overrides_dynamic_vars() {
  Cell* init1 = read("(<- x 3)");
  eval(init1);
  Cell* init2 = read("(<- x 5)");
  eval(init2);
  checkEq(lookup("x"), newNum(5));
}

void test_assign_overrides_within_lexical_scope() {
  Cell* init1 = read("(<- x 3)");
  eval(init1);
  Cell* init2 = read("((fn() (<- x 5)))");
  eval(init2);
  checkEq(lookup("x"), newNum(5));
}

void test_assign_never_overrides_lexical_vars_in_caller_scope() {
  Cell* fn = read("((fn(x) (<- y x)) 34)");
  eval(fn);
  checkEq(lookup("y"), newNum(34));
}

void test_assign_overrides_lexical_var() {
  Cell* fn = read("((fn(x) (<- x 35) (<- x 36) x) 34)");
  Cell* call = eval(fn);
  checkEq(call, newNum(36));
}

void test_equal_handles_nil() {
  Cell* call = read("(= nil nil)");
  Cell* result = eval(call);
  check(result);
  check(result != nil);
}

void test_equal_handles_floats() {
  Cell* call = read("(= (/ 3.0 2) 1.5)");
  Cell* result = eval(call);
  check(result);
  check(result != nil);
}

void test_equal_handles_float_vs_nil() {
  Cell* call = read("(= nil 1.5)");
  eval(call);
  checkEq(raiseCount, 0);
}
