void test_transform_handles_dollar_vars() {
  list<Cell*> cells = transform(buildCells(parse(parenthesize(tokenize(teststream(L"$x"))))));
  check_eq(cells.size(), 1);
  check_eq(newSym(L"sym1"), cells.front());
  rmref(cells.front());
}
