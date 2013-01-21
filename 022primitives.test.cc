void test_if_sees_args_in_then_and_else() {
  Cell* fn = read("(fn(x) (if 34 x))");
  Cell* f = eval(fn);
  newDynamicScope("f", f);
  Cell* call = read("(f 35)");
  Cell* result = eval(call);
  checkEq(result, newNum(35));
  rmref(result);
  rmref(call);
  endDynamicScope("f");
  rmref(f);
  rmref(fn);
}

void test_cons_works() {
  Cell* call = read("(cons 1 2)");
  Cell* result = eval(call);
  checkEq(car(result), newNum(1));
  checkEq(cdr(result), newNum(2));
  rmref(result);
  rmref(call);
}

void test_assign_to_fn() {
  Cell* fn = read("(<- foo (fn() 34))");
  Cell* def = eval(fn);
  Cell* scope = env(lookup("foo"));
  checkEq(scope, nil);
  endDynamicScope("foo");
  rmref(def);
  rmref(fn);
}

void test_assign_to_non_sym_warns() {
  Cell* expr = read("(<- 3 nil)");
  Cell* result = eval(expr);
  checkEq(raiseCount, 1);   raiseCount=0;
  rmref(result);
  rmref(expr);
}

void test_assign_lexical_var() {
  Cell* fn = read("((fn(x) (x <- 34) x))");
  Cell* call = eval(fn);
  checkEq(call, newNum(34));
  rmref(call);
  rmref(fn);
}

void test_assign_overrides_dynamic_vars() {
  Cell* init1 = read("(<- x 3)");
  Cell* call1 = eval(init1);
  Cell* init2 = read("(x <- 5)");   // infix <- for variety
  Cell* call2 = eval(init2);
  checkEq(lookup("x"), newNum(5));
  endDynamicScope("x");
  rmref(call2);
  rmref(init2);
  rmref(call1);
  rmref(init1);
}

void test_assign_overrides_within_lexical_scope() {
  Cell* init1 = read("(<- x 3)");
  Cell* call1 = eval(init1);
  Cell* init2 = read("((fn() (x <- 5)))");
  Cell* call2 = eval(init2);
  checkEq(lookup("x"), newNum(5));
  endDynamicScope("x");
  rmref(call2);
  rmref(init2);
  rmref(call1);
  rmref(init1);
}

void test_assign_never_overrides_lexical_vars_in_caller_scope() {
  Cell* fn = read("((fn(x) (<- y x)) 34)");  // prefix <- for variety
  Cell* def = eval(fn);
  checkEq(lookup("y"), newNum(34));
  endDynamicScope("y");
  rmref(def);
  rmref(fn);
}

void test_assign_overrides_lexical_var() {
  Cell* fn = read("((fn(x) (x <- 35) (x <- 36) x) 34)");
  Cell* call = eval(fn);
  checkEq(call, newNum(36));
  rmref(call);
  rmref(fn);
}

void test_bound_works() {
  Cell* call = read("(bound? 'a)");
  Cell* result1 = eval(call);
  checkEq(result1, nil);
  newDynamicScope("a", newNum(3));
  Cell* result2 = eval(call);
  checkEq(result2, newSym("a"));
  rmref(result2);
  endDynamicScope("a");
  rmref(result1);
  rmref(call);
}

void test_equal_handles_nil() {
  Cell* call = read("(nil = nil)");
  Cell* result = eval(call);
  check(result);
  check(result != nil);
  rmref(result);
  rmref(call);
}

void test_equal_handles_floats() {
  Cell* call = read("((/ 3.0 2) = 1.5)");
  Cell* result = eval(call);
  check(result);
  check(result != nil);
  rmref(result);
  rmref(call);
}

void test_equal_handles_float_vs_nil() {
  Cell* call = read("(nil = 1.5)");
  eval(call);
  checkEq(raiseCount, 0);
  rmref(call);
}



void test_eval_handles_eval() {
  newDynamicScope("a", newNum(34));
  newDynamicScope("x", newSym("a"));
  Cell* call = read("(eval x)");
  Cell* result = eval(call);
  checkEq(result, newNum(34));
  rmref(result);
  rmref(call);
  endDynamicScope("x");
  endDynamicScope("a");
}

void test_eval_handles_nil_scope() {
  newLexicalScope();
  addLexicalBinding("x", newNum(34));
  addLexicalBinding("caller_scope", nil);
  Cell* call = read("(eval 'x caller_scope)");
  Cell* result = eval(call);
  checkEq(result, nil);
  checkEq(raiseCount, 1);   raiseCount=0;
  rmref(result);
  rmref(call);
  endLexicalScope();
}
