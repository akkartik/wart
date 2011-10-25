void test_transform_handles_dollar_vars() {
  list<Cell*> cells = wartRead(stream(L"$x"));
  checkEq(cells.size(), 1);
  check(isSym(cells.front()));
  checkEq(toString(cells.front()).substr(0, 1), L"x");
  rmref(cells.front());
}
