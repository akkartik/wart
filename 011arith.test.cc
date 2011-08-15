void test_add_works() {
  Cell* call = wartRead(teststream(L"add 1 2")).front();
  Cell* result = eval(call);
  check_eq(toNum(result), 3);
  rmref(result);
  rmref(call);
  checkState();
}
