void test_sym_works_with_one_arg() {
  Cell* call = wartRead(stream(L"(sym \"abc\")")).front();
  Cell* result = eval(call);
  checkEq(result, newSym(L"abc"));
  rmref(result);
  rmref(call);
}

void test_sym_works_with_multiple_args() {
  Cell* call = wartRead(stream(L"(sym \"abc\" 42 'def)")).front();
  Cell* result = eval(call);
  checkEq(result, newSym(L"abc42def"));
  rmref(result);
  rmref(call);
}
