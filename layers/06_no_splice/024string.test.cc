void test_string_range() {
  cell* expr = read("(string_range \"abc\" 0 2)");
  cell* result = eval(expr);
  CHECK_EQ(to_string(result), "ab");
  rmref(result);
  rmref(expr);
}

void test_string_compare() {
  cell* expr = read("(string_lesser \"abc\" \"abd\")");
  cell* result = eval(expr);
  CHECK_EQ(result, new_num(1))
  rmref(result);
  rmref(expr);
}

void test_string_split() {
  cell* expr = read("(split \"abc abd\")");
  cell* result = eval(expr);
  CHECK_EQ(to_string(car(result)), "abc");
  CHECK_EQ(to_string(car(cdr(result))), "abd");
  CHECK_EQ(cdr(cdr(result)), nil);
  rmref(result);
  rmref(expr);
}
