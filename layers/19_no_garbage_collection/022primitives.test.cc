void test_fn_works() {
  cell* fn = read("(fn(x) x)");
  cell* result = eval(fn);
  // {sig: (x), body: (x)}
  CHECK_EQ(car(get(result, sym_sig)), new_sym("x"));
  CHECK_EQ(cdr(get(result, sym_sig)), nil);
  CHECK_EQ(car(get(result, sym_body)), new_sym("x"));
  CHECK_EQ(cdr(get(result, sym_body)), nil);
}



void test_if_sees_args_in_then_and_else() {
  cell* fn = read("(fn(x) (if 34 x))");
  cell* f = eval(fn);
  new_binding("f", f);
  cell* call = read("(f 35)");
  cell* result = eval(call);
  CHECK_EQ(result, new_num(35));
}

void test_not_works() {
  cell* call = read("(not 35)");
  cell* result = eval(call);
  CHECK_EQ(result, nil);
}

void test_not_works2() {
  cell* call = read("(not nil)");
  cell* result = eval(call);
  CHECK_EQ(result, new_num(1));
}

void test_cons_works() {
  cell* call = read("(cons 1 2)");
  cell* result = eval(call);
  // (1 ... 2)
  CHECK_EQ(car(result), new_num(1));
  CHECK_EQ(cdr(result), new_num(2));
}

void test_assign_to_non_sym_warns() {
  cell* expr = read("(<- 3 nil)");
  eval(expr);
  CHECK_EQ(Raise_count, 1);   Raise_count=0;
}

void test_assign_lexical_var() {
  cell* fn = read("((fn(x) (<- x 34) x))");
  cell* call = eval(fn);
  CHECK_EQ(call, new_num(34));
}

void test_assign_overrides_dynamic_vars() {
  cell* init1 = read("(<- x 3)");
  eval(init1);
  cell* init2 = read("(<- x 5)");
  eval(init2);
  CHECK_EQ(lookup("x"), new_num(5));
}

void test_assign_overrides_within_lexical_scope() {
  cell* init1 = read("(<- x 3)");
  eval(init1);
  cell* init2 = read("((fn() (<- x 5)))");
  eval(init2);
  CHECK_EQ(lookup("x"), new_num(5));
}

void test_assign_never_overrides_lexical_vars_in_caller_scope() {
  cell* fn = read("((fn(x) (<- y x)) 34)");
  eval(fn);
  CHECK_EQ(lookup("y"), new_num(34));
}

void test_assign_overrides_lexical_var() {
  cell* fn = read("((fn(x) (<- x 35) (<- x 36) x) 34)");
  cell* call = eval(fn);
  CHECK_EQ(call, new_num(36));
}

void test_equal_handles_nil() {
  cell* call = read("(= nil nil)");
  cell* result = eval(call);
  CHECK(result);
  CHECK(result != nil);
}

void test_equal_handles_floats() {
  cell* call = read("(= (/ 3.0 2) 1.5)");
  cell* result = eval(call);
  CHECK(result);
  CHECK(result != nil);
}

void test_equal_handles_float_vs_nil() {
  cell* call = read("(= nil 1.5)");
  eval(call);
  CHECK_EQ(Raise_count, 0);
}
