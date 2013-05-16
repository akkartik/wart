void test_transform_handles_dollar_vars() {
  Cell* expr = read("$x");
  CHECK(isSym(expr));
  CHECK_EQ(toString(expr).substr(0, 1), "x");
  rmref(expr);
}
