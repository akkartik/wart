void test_transform_handles_dollar_vars() {
  Cell* expr = read(stream(L"$x"));
  check(isSym(expr));
  checkEq(toString(expr).substr(0, 1), L"x");
  rmref(expr);
}
