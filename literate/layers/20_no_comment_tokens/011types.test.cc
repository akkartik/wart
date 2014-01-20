void test_to_float_works() {
  cell* num1 = new_num(3);
  CHECK(equal_floats(to_float(num1), 3.0));
  cell* num2 = new_num(1.5);
  CHECK(equal_floats(to_float(num2), 1.5));
}
