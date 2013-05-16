void test_add_works() {
  Cell* call = read("(+ 1 2)");
  Cell* result = eval(call);
  CHECK_EQ(toInt(result), 3);
  rmref(result);
  rmref(call);
}

void test_add_works_for_floats() {
  Cell* call = read("(+ 1.0 2.0)");
  Cell* result = eval(call);
  CHECK_EQ(toFloat(result), 3.0);
  rmref(result);
  rmref(call);
}

void test_division_always_returns_floats() {
  Cell* call = read("(/ 4 2)");
  Cell* result = eval(call);
  CHECK_EQ(result->type, FLOAT);
  CHECK_EQ(toFloat(result), 2.0);
  rmref(result);
  rmref(call);
}

void test_integer_drops_decimals() {
  Cell* call = read("(int -2.7)");
  Cell* result = eval(call);
  CHECK_EQ(result->type, INTEGER);
  CHECK_EQ(toInt(result), -2.0);
  rmref(result);
  rmref(call);
}

void test_lesser_always_passes_nil() {
  Cell* call = read("(< 3 nil)");
  CHECK_EQ(eval(call), nil);
  rmref(call);
}

void test_num_converts_from_string() {
  Cell* call = read("(num \"3\")");
  Cell* result = eval(call);
  CHECK_EQ(toInt(result), 3);
  rmref(result);
  rmref(call);
}
