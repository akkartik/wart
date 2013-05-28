void test_fn_works() {
  cell* fn = read("(fn(x) x)");
  cell* result = eval(fn);
  // (object function {sig: (x), body: (x)})
  CHECK(is_object(result));
  CHECK_EQ(type(result), sym_function);
  cell* t = rep(result);
  CHECK_EQ(car(get(t, sym_sig)), new_sym("x"));
  CHECK_EQ(cdr(get(t, sym_sig)), nil);
  CHECK_EQ(car(get(t, sym_body)), new_sym("x"));
  CHECK_EQ(cdr(get(t, sym_body)), nil);
  rmref(result);
  rmref(fn);
}



void test_if_sees_args_in_then_and_else() {
  cell* fn = read("(fn(x) (if 34 x))");
  cell* f = eval(fn);
  new_dynamic_scope("f", f);
  cell* call = read("(f 35)");
  cell* result = eval(call);
  CHECK_EQ(result, new_num(35));
  rmref(result);
  rmref(call);
  end_dynamic_scope("f");
  rmref(f);
  rmref(fn);
}

void test_cons_works() {
  cell* call = read("(cons 1 2)");
  cell* result = eval(call);
  // (1 ... 2)
  CHECK_EQ(car(result), new_num(1));
  CHECK_EQ(cdr(result), new_num(2));
  rmref(result);
  rmref(call);
}

void test_assign_to_non_sym_warns() {
  cell* expr = read("(<- 3 nil)");
  cell* result = eval(expr);
  CHECK_EQ(Raise_count, 1);   Raise_count=0;
  rmref(result);
  rmref(expr);
}

void test_assign_lexical_var() {
  cell* fn = read("((fn(x) (<- x 34) x))");
  cell* call = eval(fn);
  CHECK_EQ(call, new_num(34));
  rmref(call);
  rmref(fn);
}

void test_assign_overrides_dynamic_vars() {
  cell* init1 = read("(<- x 3)");
  cell* call1 = eval(init1);
  cell* init2 = read("(<- x 5)");
  cell* call2 = eval(init2);
  CHECK_EQ(lookup("x"), new_num(5));
  end_dynamic_scope("x");
  rmref(call2);
  rmref(init2);
  rmref(call1);
  rmref(init1);
}

void test_assign_overrides_within_lexical_scope() {
  cell* init1 = read("(<- x 3)");
  cell* call1 = eval(init1);
  cell* init2 = read("((fn() (<- x 5)))");
  cell* call2 = eval(init2);
  CHECK_EQ(lookup("x"), new_num(5));
  end_dynamic_scope("x");
  rmref(call2);
  rmref(init2);
  rmref(call1);
  rmref(init1);
}

void test_assign_never_overrides_lexical_vars_in_caller_scope() {
  cell* fn = read("((fn(x) (<- y x)) 34)");
  cell* def = eval(fn);
  CHECK_EQ(lookup("y"), new_num(34));
  end_dynamic_scope("y");
  rmref(def);
  rmref(fn);
}

void test_assign_overrides_lexical_var() {
  cell* fn = read("((fn(x) (<- x 35) (<- x 36) x) 34)");
  cell* call = eval(fn);
  CHECK_EQ(call, new_num(36));
  rmref(call);
  rmref(fn);
}

void test_unbind_works() {
  new_dynamic_scope("x", new_num(3));
  CHECK_EQ(Dynamics[new_sym("x")].size(), 1);
  cell* expr = read("(unbind x)");
  eval(expr);   // always returns nil
  CHECK_EQ(Dynamics[new_sym("x")].size(), 0);
  rmref(expr);
}

void test_unbind_handles_unbound_vars() {
  CHECK_EQ(Dynamics[new_sym("x")].size(), 0);
  cell* expr = read("(unbind x)");
  eval(expr);   // always returns nil
  CHECK_EQ(Dynamics[new_sym("x")].size(), 0);
  rmref(expr);
}

void test_bound_works() {
  cell* call = read("(bound? 'a)");
  cell* result1 = eval(call);
  CHECK_EQ(result1, nil);
  new_dynamic_scope("a", new_num(3));
  cell* result2 = eval(call);
  CHECK_EQ(result2, new_sym("a"));
  rmref(result2);
  end_dynamic_scope("a");
  rmref(result1);
  rmref(call);
}

void test_bound_checks_only_dynamic_scope_on_nil() {
  cell* call = read("(bound? 'a nil)");
  cell* result1 = eval(call);
  CHECK_EQ(result1, nil);
  new_lexical_scope();
  add_lexical_binding("a", new_num(3));
  cell* result2 = eval(call);
  CHECK_EQ(result2, nil);
  rmref(result2);
  end_lexical_scope();
  rmref(result1);
  rmref(call);
}

void test_equal_handles_nil() {
  cell* call = read("(= nil nil)");
  cell* result = eval(call);
  CHECK(result);
  CHECK(result != nil);
  rmref(result);
  rmref(call);
}

void test_equal_handles_floats() {
  cell* call = read("(= (/ 3.0 2) 1.5)");
  cell* result = eval(call);
  CHECK(result);
  CHECK(result != nil);
  rmref(result);
  rmref(call);
}

void test_equal_handles_float_vs_nil() {
  cell* call = read("(= nil 1.5)");
  eval(call);
  CHECK_EQ(Raise_count, 0);
  rmref(call);
}



void test_eval_handles_eval() {
  new_dynamic_scope("a", new_num(34));
  new_dynamic_scope("x", new_sym("a"));
  cell* call = read("(eval x)");
  cell* result = eval(call);
  CHECK_EQ(result, new_num(34));
  rmref(result);
  rmref(call);
  end_dynamic_scope("x");
  end_dynamic_scope("a");
}

void test_eval_handles_nil_scope() {
  new_lexical_scope();
  add_lexical_binding("x", new_num(34));
  add_lexical_binding("caller_scope", nil);
  cell* call = read("(eval 'x caller_scope)");
  cell* result = eval(call);
  CHECK_EQ(Raise_count, 1);   Raise_count=0;
  CHECK_EQ(result, nil);
  rmref(call);
  end_lexical_scope();
}
