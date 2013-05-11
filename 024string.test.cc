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

void test_string_split() {
  Cell* expr = read("(split \"abc abd\")");
  Cell* result = eval(expr);
  checkEq(toString(car(result)), "abc");
  checkEq(toString(car(cdr(result))), "abd");
  checkEq(cdr(cdr(result)), nil);
  rmref(result);
  rmref(expr);
}

void test_upcase() {
  Cell* expr = read("(upcase \"Abc aBd?\")");
  Cell* result = eval(expr);
  checkEq(toString(result), "ABC ABD?");
  rmref(result);
  rmref(expr);
}

void test_downcase() {
  Cell* expr = read("(downcase \"Abc aBd?\")");
  Cell* result = eval(expr);
  checkEq(toString(result), "abc abd?");
  rmref(result);
  rmref(expr);
}
