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

void test_assign_lexical_var() {
  Cell* lambda = buildCells(parse(parenthesize(tokenize(teststream(L"((lambda(x) (assign x 34) x))"))))).front();
  Cell* call = eval(lambda);
  check_eq(call, newNum(34));
  rmref(call);
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

void test_sym_works_with_one_arg() {
  Cell* call = buildCells(parse(parenthesize(tokenize(teststream(L"(sym \"abc\")"))))).front();
  Cell* result = eval(call);
  check_eq(result, newSym(L"abc"));
  rmref(result);
  rmref(call);
  checkState();
}

void test_sym_works_with_multiple_args() {
  Cell* call = buildCells(parse(parenthesize(tokenize(teststream(L"(sym \"abc\" 42 'def)"))))).front();
  Cell* result = eval(call);
  check_eq(result, newSym(L"abc42def"));
  rmref(result);
  rmref(call);
  checkState();
}
