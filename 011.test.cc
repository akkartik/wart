void test_eval_handles_eval() {
  newDynamicScope("a", newNum(34));
  newDynamicScope("x", newSym("a"));
  newDynamicScope("caller-scope", nil);
  Cell* call = read(stream("(eval x)"));
  Cell* result = eval(call);
  checkEq(result, newNum(34));
  rmref(result);
  rmref(call);
  endDynamicScope("caller-scope");
  endDynamicScope("x");
  endDynamicScope("a");
}

void test_if_sees_args_in_then_and_else() {
  Cell* fn = read(stream("(fn(x) (if 34 x))"));
  Cell* f = eval(fn);
  newDynamicScope("f", f);
  Cell* call = read(stream("(f 35)"));
  Cell* result = eval(call);
  checkEq(result, newNum(35));
  rmref(result);
  rmref(call);
  endDynamicScope("f");
  rmref(f);
  rmref(fn);
}

void test_cons_works() {
  Cell* call = read(stream("cons 1 2"));
  Cell* result = eval(call);
  checkEq(car(result), newNum(1));
  checkEq(cdr(result), newNum(2));
  rmref(result);
  rmref(call);
}

void test_assign_to_fn() {
  Cell* fn = read(stream("= foo (fn() 34)"));
  Cell* def = eval(fn);
  Cell* scope = calleeEnv(lookup("foo"));
  checkEq(scope, nil);
  endDynamicScope("foo");
  rmref(def);
  rmref(fn);
}

void test_assign_lexical_var() {
  Cell* fn = read(stream("((fn(x) (= x 34) x))"));
  Cell* call = eval(fn);
  checkEq(call, newNum(34));
  rmref(call);
  rmref(fn);
}

void test_assign_overrides_dynamic_vars() {
  Cell* init1 = read(stream("= x 3"));
  Cell* call1 = eval(init1);
  Cell* init2 = read(stream("= x 5"));
  Cell* call2 = eval(init2);
  checkEq(lookup("x"), newNum(5));
  endDynamicScope("x");
  rmref(call2);
  rmref(init2);
  rmref(call1);
  rmref(init1);
}

void test_assign_overrides_within_lexical_scope() {
  Cell* init1 = read(stream("= x 3"));
  Cell* call1 = eval(init1);
  Cell* init2 = read(stream("((fn() (= x 5)))"));
  Cell* call2 = eval(init2);
  checkEq(lookup("x"), newNum(5));
  endDynamicScope("x");
  rmref(call2);
  rmref(init2);
  rmref(call1);
  rmref(init1);
}

void test_assign_never_overrides_lexical_vars_in_caller_scope() {
  Cell* fn = read(stream("((fn(x) (= y x)) 34)"));
  Cell* def = eval(fn);
  checkEq(lookup("y"), newNum(34));
  endDynamicScope("y");
  rmref(def);
  rmref(fn);
}

void test_assign_overrides_lexical_var() {
  Cell* fn = read(stream("((fn(x) (= x 34) (= x 35) x) 34)"));
  Cell* call = eval(fn);
  checkEq(call, newNum(35));
  rmref(call);
  rmref(fn);
}

void test_bound_works() {
  Cell* call = read(stream("bound?!a"));
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

void test_iso_nil() {
  Cell* call = read(stream("iso nil nil"));
  Cell* result = eval(call);
  check(result);
  check(result != nil);
  rmref(result);
  rmref(call);
}
