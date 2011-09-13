void test_transform_handles_dollar_vars() {
  list<Cell*> cells = wartRead(stream(L"$x"));
  check_eq(cells.size(), 1);
  check(isSym(cells.front()));
  check_eq(toString(cells.front()).substr(0, 1), L"x");
  rmref(cells.front());
}
