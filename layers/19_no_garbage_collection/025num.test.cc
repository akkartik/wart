void test_add_works() {
  cell* call = read("(+ 1 2)");
  cell* result = eval(call);
  CHECK_EQ(to_int(result), 3);
}

void test_add_works_for_floats() {
  cell* call = read("(+ 1.0 2.0)");
  cell* result = eval(call);
  CHECK_EQ(to_float(result), 3.0);
}

void test_division_always_returns_floats() {
  cell* call = read("(/ 4 2)");
  cell* result = eval(call);
  CHECK_EQ(result->type, FLOAT);
  CHECK_EQ(to_float(result), 2.0);
}

void test_integer_drops_decimals() {
  cell* call = read("(int -2.7)");
  cell* result = eval(call);
  CHECK_EQ(result->type, INTEGER);
  CHECK_EQ(to_int(result), -2.0);
}

void test_lesser_always_passes_nil() {
  cell* call = read("(< 3 nil)");
  CHECK_EQ(eval(call), nil);
}
