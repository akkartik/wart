void test_toFloat_works() {
  Cell* num1 = newNum(3);
  check(equalFloats(toFloat(num1), 3.0));
  Cell* num2 = newNum(1.5);
  check(equalFloats(toFloat(num2), 1.5));
}
