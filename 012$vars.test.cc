void test_transform_handles_dollar_vars() {
  list<Cell*> cells = transform(buildCells(parse(parenthesize(tokenize(stream(L"$x"))))));
  check_eq(cells.size(), 1);
  check(isSym(cells.front()));
  check_eq(toString(cells.front()).substr(0, 3), L"sym");
  rmref(cells.front());
}
