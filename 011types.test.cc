void test_list_splice_replaces_index() {
  newDynamicScope(L"a", newCons(newNum(3), newCons(newNum(4), nil)));
  Cell* expr = wartRead(stream(L"list_splice a 1 2 '(5)")).front();
  Cell* call = eval(expr);
  checkEq(car(lookup(L"a")), newNum(3));
  checkEq(car(cdr(lookup(L"a"))), newNum(5));
  rmref(call);
  rmref(expr);
  endDynamicScope(L"a");
}

void test_list_splice_replaces_first_index() {
  newDynamicScope(L"a", newCons(newNum(3), newCons(newNum(4), nil)));
  Cell* expr = wartRead(stream(L"list_splice a 0 1 '(9)")).front();
  Cell* call = eval(expr);
  checkEq(car(lookup(L"a")), newNum(9));
  rmref(call);
  rmref(expr);
  endDynamicScope(L"a");
}
