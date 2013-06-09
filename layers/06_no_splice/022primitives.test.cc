cell* eval(string s) {
  TEMP(form, read(s));
  return eval(form);
}

void test_fn_works() {
  TEMP(result, eval("(fn(x) x)"));
  // (object function {sig: (x), body: (x)})
  CHECK(is_object(result));
  CHECK_EQ(type(result), sym_function);
  CHECK_EQ(car(get(rep(result), sym_sig)), new_sym("x"));
  CHECK_EQ(cdr(get(rep(result), sym_sig)), nil);
  CHECK_EQ(car(get(rep(result), sym_body)), new_sym("x"));
  CHECK_EQ(cdr(get(rep(result), sym_body)), nil);
}

void test_eval_on_fn_is_idempotent() {
  TEMP(a, eval("(fn(x) x)"));
  TEMP(result, eval(a));
  // (object function {sig: (x), body: (x)})
  CHECK(is_object(result));
  CHECK_EQ(type(result), sym_function);
  CHECK_EQ(car(get(rep(result), sym_sig)), new_sym("x"));
  CHECK_EQ(cdr(get(rep(result), sym_sig)), nil);
  CHECK_EQ(car(get(rep(result), sym_body)), new_sym("x"));
  CHECK_EQ(cdr(get(rep(result), sym_body)), nil);
}



void test_if_sees_args_in_then_and_else() {
  run("(<- f (fn(x) (if 34 x)))");
  CLEAR_TRACE;
  run("(f 35)");
  CHECK_TRACE_TOP("eval", "=> 35");
  end_dynamic_scope("f");
}

void test_not_works() {
  run("(not 35)");
  CHECK_TRACE_TOP("eval", "compiled fn=> nil");
}

void test_not_works2() {
  run("(not nil)");
  CHECK_TRACE_TOP("eval", "compiled fn=> 1");
}

void test_cons_works() {
  run("(cons 1 2)");
  CHECK_TRACE_TOP("eval", "compiled fn=> (1 ... 2)");
}

void test_assign_to_non_sym_warns() {
  Do_raise = false;
  run("(<- 3 nil)");
  CHECK_EQ(Raise_count, 1);   Raise_count=0;
}

void test_assign_lexical_var() {
  run("((fn (x) (x <- 34) x))");
  CHECK_TRACE_TOP("eval", "=> 34");
}

void test_assign_overrides_dynamic_vars() {
  run("(<- x 3)");
  run("(x <- 5)");  // infix for variety
  CLEAR_TRACE;
  run("x");
  CHECK_TRACE_TOP("eval", "sym=> 5");
  end_dynamic_scope("x");
}

void test_assign_overrides_within_lexical_scope() {
  run("(<- x 3)");
  run("((fn () (<- x 5)))");
  CLEAR_TRACE;
  run("x");
  CHECK_TRACE_TOP("eval", "sym=> 5");
  end_dynamic_scope("x");
}

void test_assign_never_overrides_lexical_vars_in_caller_scope() {
  run("((fn (x) (<- y x)) 34)");  // prefix <- for variety
  CLEAR_TRACE;
  run("y");
  CHECK_TRACE_CONTENTS("eval", "sym=> 34");
  end_dynamic_scope("y");
}

void test_assign_overrides_lexical_var() {
  run("((fn (x) (x <- 35) (x <- 36) x) 34)");
  CHECK_TRACE_TOP("eval", "=> 36");
}

void test_unbind_works() {
  new_dynamic_scope("x", new_num(3));
  run("(unbind x)");
  CHECK_TRACE_CONTENTS("unbind", "x");
}

void test_unbind_handles_unbound_vars() {
  run("(unbind x)");
  CHECK_TRACE_DOESNT_CONTAIN("unbind", "x");
}

void test_bound_works() {
  run("(bound? 'a)");
  CHECK_TRACE_TOP("eval", "=> nil");
  CLEAR_TRACE;
  new_dynamic_scope("a", new_num(3));
  run("(bound? 'a)");
  CHECK_TRACE_TOP("eval", "=> a");
  end_dynamic_scope("a");
}

void test_bound_checks_only_dynamic_scope_on_nil() {
  TEMP(call, read("(bound? 'a nil)"));
  TEMP(result1, eval(call));
  CHECK_EQ(result1, nil);
  new_lexical_scope();
  add_lexical_binding("a", new_num(3));
    TEMP(result2, eval(call));
    CHECK_EQ(result2, nil);
  end_lexical_scope();
}

void test_equal_handles_nil() {
  run("(nil = nil)");
  CHECK_TRACE_DOESNT_CONTAIN("eval", 1, "=> nil");
}

void test_equal_handles_floats() {
  run("((/ 3.0 2) = 1.5)");
  CHECK_TRACE_DOESNT_CONTAIN("eval", 1, "=> nil");
}

void test_equal_handles_float_vs_nil() {
  Do_raise = false;
  run("(nil = 1.5)");
  CHECK_TRACE_TOP("eval", "=> nil");
  CHECK_EQ(Raise_count, 0);
}

void test_eval_handles_eval() {
  new_dynamic_scope("a", new_num(34));
  new_dynamic_scope("x", new_sym("a"));
  run("(eval x)");
  CHECK_TRACE_TOP("eval", "=> 34");
  end_dynamic_scope("x");
  end_dynamic_scope("a");
}

void test_eval_warns_on_missing_binding() {
  Do_raise = false;
  run("(eval 'x nil)");
  CHECK_EQ(Raise_count, 1);   Raise_count=0;
  CHECK_TRACE_TOP("eval", "=> nil");
}
