void test_prn_returns_arg() {
  Cell* lambda = buildCells(parse(parenthesize(tokenize(teststream(L"assign foo (lambda() 34)"))))).front();
  Cell* def = eval(lambda);
  Cell* prn = buildCells(parse(parenthesize(tokenize(teststream(L"_prn foo"))))).front();
  Cell* evalprn = eval(prn);
  check_eq(evalprn, def);
  rmref(evalprn);
  rmref(prn);
  endDynamicScope(L"foo");
  rmref(def);
  rmref(lambda);
  checkState();
}

