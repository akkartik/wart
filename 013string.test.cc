void test_string_get_takes_range() {
  Cell* expr = read(stream("string_get \"abc\" 0 2"));
  Cell* result = eval(expr);
  checkEq(toString(result), "ab");
  rmref(result);
  rmref(expr);
}
