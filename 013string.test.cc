void test_string_range() {
  Cell* expr = read(stream("string_range \"abc\" 0 2"));
  Cell* result = eval(expr);
  checkEq(toString(result), "ab");
  rmref(result);
  rmref(expr);
}
