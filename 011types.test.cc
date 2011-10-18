void test_list_set_works_with_first_index() {
  newDynamicScope(L"a", newCons(newNum(3), newCons(newNum(4), nil)));
  Cell* expr = wartRead(stream(L"list_set a 0 9")).front();
  Cell* call = eval(expr);
  checkEq(car(lookup(L"a")), newNum(9));
  rmref(call);
  rmref(expr);
  endDynamicScope(L"a");
}
