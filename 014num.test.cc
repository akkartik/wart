void test_add_works() {
  Cell* call = wartRead(stream(L"+ 1 2")).front();
  Cell* result = eval(call);
  checkEq(toNum(result), 3);
  rmref(result);
  rmref(call);
}