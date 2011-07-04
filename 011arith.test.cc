void test_plus_works() {
  Cell* call = buildCells(parse(parenthesize(tokenize(teststream(L"plus 1 2"))))).front();
  Cell* result = eval(call);
  check_eq(toNum(result), 3);
  rmref(result);
  rmref(call);
  checkState();
}
