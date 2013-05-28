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

void test_upcase() {
  cell* expr = read("(upcase \"Abc aBd?\")");
  cell* result = eval(expr);
  CHECK_EQ(to_string(result), "ABC ABD?");
  rmref(result);
  rmref(expr);
}

void test_downcase() {
  cell* expr = read("(downcase \"Abc aBd?\")");
  cell* result = eval(expr);
  CHECK_EQ(to_string(result), "abc abd?");
  rmref(result);
  rmref(expr);
}
