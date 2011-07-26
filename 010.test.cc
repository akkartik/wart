void test_cons_works() {
  Cell* call = buildCells(parse(parenthesize(tokenize(teststream(L"cons 1 2"))))).front();
  Cell* result = eval(call);
  check_eq(car(result), newNum(1));
  check_eq(cdr(result), newNum(2));
  rmref(result);
  rmref(call);
  checkState();
}

void test_assign_to_lambda() {
  Cell* lambda = buildCells(parse(parenthesize(tokenize(teststream(L"assign foo (lambda() 34)"))))).front();
  Cell* def = eval(lambda);
  check_eq(callee_env(lookup(L"foo")), nil);
  endDynamicScope(L"foo");
  rmref(def);
  rmref(lambda);
  checkState();
}

void test_if_sees_args_in_then_and_else() {
  Cell* lambda = buildCells(parse(parenthesize(tokenize(teststream(L"(lambda(x) (_if 34 x))"))))).front();
  Cell* f = eval(lambda);
  newDynamicScope(L"f", f);
  Cell* call = buildCells(parse(parenthesize(tokenize(teststream(L"(f 35)"))))).front();
  Cell* result = eval(call);
  check_eq(result, newNum(35));
  rmref(result);
  rmref(call);
  endDynamicScope(L"f");
  rmref(f);
  rmref(lambda);
  checkState();
}
