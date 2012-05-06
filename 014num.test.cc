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
