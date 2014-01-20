void test_transform_handles_dollar_vars() {
  cell* expr = read("$x");
  CHECK(is_sym(expr));
  CHECK_EQ(to_string(expr).substr(0, 1), "x");
  rmref(expr);
}
