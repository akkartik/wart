void test_ssyntax() {
  SsyntaxTemplate s = {L'.', SsyntaxTemplate::IN_BETWEEN, newSym(L"op")};
  ssyntaxTemplates.push_back(s);
  Cell* cons = wartRead(stream(L"a.b")).front();
  check_eq(car(cons), newSym(L"op"));
  check_eq(car(cdr(cons)), newSym(L"a"));
  check_eq(car(cdr(cdr(cons))), newSym(L"b"));
  check_eq(cdr(cdr(cdr(cons))), nil);
  rmref(cons);
  ssyntaxTemplates.clear();
}

void test_ssyntax_skips_floats() {
  SsyntaxTemplate s = {L'.', SsyntaxTemplate::IN_BETWEEN, newSym(L"op")};
  ssyntaxTemplates.push_back(s);
  Cell* val = wartRead(stream(L"2.4")).front();
  check(isSym(val)); // fix when we support floats
  check_eq(toString(val), L"2.4");
  rmref(val);
  ssyntaxTemplates.clear();
}

void test_unary_ssyntax() {
  SsyntaxTemplate s = {L'.', SsyntaxTemplate::BEGINNING, newSym(L"op")};
  ssyntaxTemplates.push_back(s);
  Cell* cons = wartRead(stream(L".b")).front();
  check_eq(car(cons), newSym(L"op"));
  check_eq(car(cdr(cons)), newSym(L"b"));
  check_eq(cdr(cdr(cons)), nil);
  rmref(cons);
  ssyntaxTemplates.clear();
}

void test_ssyntax_setup() {
  Cell* def = wartRead(stream(L"ssyntax _._ (op _ _)")).front();
  eval(def); // needn't rmref; returns nil
  Cell* expr = wartRead(stream(L"a.b")).front();
  check_eq(car(expr), newSym(L"op"));
  check_eq(car(cdr(expr)), newSym(L"a"));
  check_eq(car(cdr(cdr(expr))), newSym(L"b"));
  check_eq(cdr(cdr(cdr(expr))), nil);
  rmref(expr);
  ssyntaxTemplates.clear();
  rmref(def);
}
