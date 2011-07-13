void test_assign_to_lambda() {
  Cell* lambda = buildCells(parse(parenthesize(tokenize(teststream(L"assign foo (lambda() 34)"))))).front();
  Cell* def = eval(lambda);
  check_eq(callee_env(lookup(L"foo")), nil);
  endDynamicScope(L"foo");
  rmref(def);
  rmref(lambda);
  checkState();
}
