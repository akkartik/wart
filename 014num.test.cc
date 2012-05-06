void test_add_works() {
  Cell* call = read(stream("+ 1 2"));
  Cell* result = eval(call);
  checkEq(toInt(result), 3);
  rmref(result);
  rmref(call);
}
