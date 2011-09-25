void test_eval_handles_eval() {
  newDynamicScope(L"a", newNum(34));
  newDynamicScope(L"x", newSym(L"a"));
  Cell* call = wartRead(stream(L"(eval x)")).front();
  Cell* result = eval(call);
  checkEq(result, newNum(34));
  rmref(result);
  rmref(call);
  endDynamicScope(L"x");
  endDynamicScope(L"a");
}

void test_inline_sees_caller_scope_before_dynamic_vars() {
  newDynamicScope(L"x", newNum(34));
  Cell* def = wartRead(stream(L"(inline (fn() x))")).front();
  Cell* fn = eval(def);
  newDynamicScope(L"f", fn);
  newLexicalScope();
    addLexicalBinding(L"x", newNum(3));
    Cell* call = wartRead(stream(L"(f)")).front();
    Cell* result = eval(call);
    checkEq(result, newNum(3));
    rmref(result);
    rmref(call);
  endLexicalScope();
  endDynamicScope(L"f");
  rmref(fn);
  rmref(def);
  endDynamicScope(L"x");
}

void test_cons_works() {
  Cell* call = wartRead(stream(L"cons 1 2")).front();
  Cell* result = eval(call);
  checkEq(car(result), newNum(1));
  checkEq(cdr(result), newNum(2));
  rmref(result);
  rmref(call);
}

void test_assign_to_fn() {
  Cell* fn = wartRead(stream(L"assign foo (fn() 34)")).front();
  Cell* def = eval(fn);
  Cell* scope = calleeEnv(lookup(L"foo"));
  checkEq(scope, newSym(L"dynamicScope"));
  endDynamicScope(L"foo");
  rmref(def);
  rmref(fn);
}

void test_assign_lexical_var() {
  Cell* fn = wartRead(stream(L"((fn(x) (assign x 34) x))")).front();
  Cell* call = eval(fn);
  checkEq(call, newNum(34));
  rmref(call);
  rmref(fn);
}

void test_assign_overrides_dynamic_vars() {
  Cell* init1 = wartRead(stream(L"assign x 3")).front();
  Cell* call1 = eval(init1);
  Cell* init2 = wartRead(stream(L"assign x 5")).front();
  Cell* call2 = eval(init2);
  checkEq(lookup(L"x"), newNum(5));
  endDynamicScope(L"x");
  rmref(call2);
  rmref(init2);
  rmref(call1);
  rmref(init1);
}

void test_assign_overrides_within_lexical_scope() {
  Cell* init1 = wartRead(stream(L"assign x 3")).front();
  Cell* call1 = eval(init1);
  Cell* init2 = wartRead(stream(L"((fn() (assign x 5)))")).front();
  Cell* call2 = eval(init2);
  checkEq(lookup(L"x"), newNum(5));
  endDynamicScope(L"x");
  rmref(call2);
  rmref(init2);
  rmref(call1);
  rmref(init1);
}

void test_assign_never_overrides_lexical_vars_in_caller_scope() {
  Cell* fn = wartRead(stream(L"((fn(x) (assign y x)) 34)")).front();
  Cell* def = eval(fn);
  checkEq(lookup(L"y"), newNum(34));
  endDynamicScope(L"y");
  rmref(def);
  rmref(fn);
}

void test_assign_overrides_lexical_var() {
  Cell* fn = wartRead(stream(L"((fn(x) (assign x 34) (assign x 35) x) 34)")).front();
  Cell* call = eval(fn);
  checkEq(call, newNum(35));
  rmref(call);
  rmref(fn);
}

void test_if_sees_args_in_then_and_else() {
  Cell* fn = wartRead(stream(L"(fn(x) (if 34 x))")).front();
  Cell* f = eval(fn);
  newDynamicScope(L"f", f);
  Cell* call = wartRead(stream(L"(f 35)")).front();
  Cell* result = eval(call);
  checkEq(result, newNum(35));
  rmref(result);
  rmref(call);
  endDynamicScope(L"f");
  rmref(f);
  rmref(fn);
}

void test_sym_works_with_one_arg() {
  Cell* call = wartRead(stream(L"(sym \"abc\")")).front();
  Cell* result = eval(call);
  checkEq(result, newSym(L"abc"));
  rmref(result);
  rmref(call);
}

void test_sym_works_with_multiple_args() {
  Cell* call = wartRead(stream(L"(sym \"abc\" 42 'def)")).front();
  Cell* result = eval(call);
  checkEq(result, newSym(L"abc42def"));
  rmref(result);
  rmref(call);
}

void test_str_works_with_multiple_args() {
  Cell* call = wartRead(stream(L"(str \"abc\" 42 'def)")).front();
  Cell* result = eval(call);
  check(isString(result));
  checkEq(toString(result), L"abc42def");
  rmref(result);
  rmref(call);
}



void test_add_works() {
  Cell* call = wartRead(stream(L"+ 1 2")).front();
  Cell* result = eval(call);
  checkEq(toNum(result), 3);
  rmref(result);
  rmref(call);
}
