void test_add_works() {
  Cell* call = read(stream("+ 1 2"));
  Cell* result = eval(call);
  checkEq(toInt(result), 3);
  rmref(result);
  rmref(call);
}

void test_add_works_for_floats() {
  Cell* call = read(stream("+ 1.0 2.0"));
  Cell* result = eval(call);
  checkEq(toFloat(result), 3.0);
  rmref(result);
  rmref(call);
}

void test_division_always_returns_floats() {
  Cell* call = read(stream("/ 4 2"));
  Cell* result = eval(call);
  checkEq(result->type, FLOAT);
  checkEq(toFloat(result), 2.0);
  rmref(result);
  rmref(call);
}

void test_integer_drops_decimals() {
  Cell* call = read(stream("int -2.7"));
  Cell* result = eval(call);
  checkEq(result->type, INTEGER);
  checkEq(toInt(result), -2.0);
  rmref(result);
  rmref(call);
}
