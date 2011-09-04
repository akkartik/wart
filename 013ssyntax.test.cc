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

void test_ssyntax_skips_floats() {
  SsyntaxTemplate s = {L'.', SsyntaxTemplate::IN_BETWEEN, newSym(L"foo")};
  ssyntaxTemplates.push_back(s);
  Cell* val = wartRead(stream(L"2.4")).front();
  check(isSym(val)); // fix when we support floats
  check_eq(toString(val), L"2.4");
  rmref(val);
}
