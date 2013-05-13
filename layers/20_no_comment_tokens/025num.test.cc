void test_add_works() {
  Cell* call = read("(+ 1 2)");
  Cell* result = eval(call);
  checkEq(toInt(result), 3);
}

void test_add_works_for_floats() {
  Cell* call = read("(+ 1.0 2.0)");
  Cell* result = eval(call);
  checkEq(toFloat(result), 3.0);
}

void test_division_always_returns_floats() {
  Cell* call = read("(/ 4 2)");
  Cell* result = eval(call);
  checkEq(result->type, FLOAT);
  checkEq(toFloat(result), 2.0);
}

void test_integer_drops_decimals() {
  Cell* call = read("(int -2.7)");
  Cell* result = eval(call);
  checkEq(result->type, INTEGER);
  checkEq(toInt(result), -2.0);
}

void test_lesser_always_passes_nil() {
  Cell* call = read("(< 3 nil)");
  checkEq(eval(call), nil);
}
