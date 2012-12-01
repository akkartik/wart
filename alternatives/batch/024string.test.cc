void test_string_range() {
  Cell* expr = read("(string_range \"abc\" 0 2)");
  Cell* result = eval(expr);
  checkEq(toString(result), "ab");
  rmref(result);
  rmref(expr);
}

void test_string_compare() {
  Cell* expr = read("(string_lesser \"abc\" \"abd\")");
  Cell* result = eval(expr);
  checkEq(result, newNum(1))
  rmref(result);
  rmref(expr);
}
