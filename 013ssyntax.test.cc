void test_ssyntax() {
  SsyntaxTemplate s = {L'.', SsyntaxTemplate::IN_BETWEEN, newSym(L"foo")};
  ssyntaxTemplates.push_back(s);
  Cell* cons = wartRead(stream(L"a.b")).front();
  check_eq(car(cons), newSym(L"foo"));
  check_eq(car(cdr(cons)), newSym(L"a"));
  check_eq(car(cdr(cdr(cons))), newSym(L"b"));
  check_eq(cdr(cdr(cdr(cons))), nil);
  rmref(cons);
}
