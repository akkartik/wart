void test_add_works() {
  Cell* call = read(stream(L"+ 1 2"));
  Cell* result = eval(call);
  checkEq(toNum(result), 3);
  rmref(result);
  rmref(call);
}
