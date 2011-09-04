void test_ssyntax() {
  SsyntaxTemplate s = {L'.', SsyntaxTemplate::MULTIARY, newSym(L"op")};
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
  SsyntaxTemplate s = {L'.', SsyntaxTemplate::MULTIARY, newSym(L"op")};
  ssyntaxTemplates.push_back(s);
  Cell* val = wartRead(stream(L"2.4")).front();
  check_eq(val, newSym(L"2.4")); // fix when we support floats
  rmref(val);
  ssyntaxTemplates.clear();
}

void test_unary_ssyntax() {
  SsyntaxTemplate s = {L'.', SsyntaxTemplate::UNARY, newSym(L"op")};
  ssyntaxTemplates.push_back(s);
  Cell* sym = wartRead(stream(L"a.b")).front();
  check_eq(sym, newSym(L"a.b")); // doesn't trigger for binary ops
  rmref(sym);

  Cell* cons = wartRead(stream(L".b")).front();
  check_eq(car(cons), newSym(L"op"));
  check_eq(car(cdr(cons)), newSym(L"b"));
  check_eq(cdr(cdr(cons)), nil);
  rmref(cons);
  ssyntaxTemplates.clear();
}

void test_ssyntax_adds_trailing_nil() {
  SsyntaxTemplate s = {L'.', SsyntaxTemplate::MULTIARY, newSym(L"op")};
  ssyntaxTemplates.push_back(s);
  Cell* cons = wartRead(stream(L"a.")).front();
  check_eq(car(cons), newSym(L"op"));
  check_eq(car(cdr(cons)), newSym(L"a"));
  check(isCons(cdr(cdr(cons))));
  check_eq(car(cdr(cdr(cons))), nil);
  check_eq(cdr(cdr(cdr(cons))), nil);
  rmref(cons);
  ssyntaxTemplates.clear();
}

void test_multiary_ssyntax() {
  SsyntaxTemplate s = {L'.', SsyntaxTemplate::MULTIARY, newSym(L"op")};
  ssyntaxTemplates.push_back(s);
  Cell* cons = wartRead(stream(L"a.b.c")).front();
  check_eq(car(cons), newSym(L"op"));
  check_eq(car(cdr(cons)), newSym(L"a"));
  check_eq(car(cdr(cdr(cons))), newSym(L"b"));
  check_eq(car(cdr(cdr(cdr(cons)))), newSym(L"c"));
  check_eq(cdr(cdr(cdr(cdr(cons)))), nil);
  rmref(cons);
  ssyntaxTemplates.clear();
}

void test_left_associative_ssyntax() {
  SsyntaxTemplate s = {L'.', SsyntaxTemplate::LEFT_ASSOCIATIVE, newSym(L"op")};
  ssyntaxTemplates.push_back(s);
  Cell* cons = wartRead(stream(L"a.b.c")).front();
  check_eq(car(cons), newSym(L"op"));

  Cell* lhs = car(cdr(cons));
  check(isCons(lhs));
  check_eq(car(lhs), newSym(L"op"));
  check_eq(car(cdr(lhs)), newSym(L"a"));
  check_eq(car(cdr(cdr(lhs))), newSym(L"b"));
  check_eq(cdr(cdr(cdr(lhs))), nil);

  Cell* rhs = car(cdr(cdr(cons)));
  check_eq(rhs, newSym(L"c"));

  check_eq(cdr(cdr(cdr(cons))), nil);
  rmref(cons);
  ssyntaxTemplates.clear();
}

void test_right_associative_ssyntax() {
  SsyntaxTemplate s = {L'.', SsyntaxTemplate::RIGHT_ASSOCIATIVE, newSym(L"op")};
  ssyntaxTemplates.push_back(s);
  Cell* cons = wartRead(stream(L"a.b.c")).front();
  check_eq(car(cons), newSym(L"op"));

  Cell* lhs = car(cdr(cons));
  check_eq(lhs, newSym(L"a"));

  Cell* rhs = car(cdr(cdr(cons)));
  check(isCons(rhs));
  check_eq(car(rhs), newSym(L"op"));
  check_eq(car(cdr(rhs)), newSym(L"b"));
  check_eq(car(cdr(cdr(rhs))), newSym(L"c"));

  check_eq(cdr(cdr(cdr(cons))), nil);
  rmref(cons);
  ssyntaxTemplates.clear();
}

void test_multiary_ssyntax_setup() {
  Cell* def = wartRead(stream(L"ssyntax _._ (op _ _)")).front();
  eval(def); // needn't rmref; returns nil
  Cell* expr = wartRead(stream(L"a.b.c")).front();
  check_eq(car(expr), newSym(L"op"));
  check_eq(car(cdr(expr)), newSym(L"a"));
  check_eq(car(cdr(cdr(expr))), newSym(L"b"));
  check_eq(car(cdr(cdr(cdr(expr)))), newSym(L"c"));
  check_eq(cdr(cdr(cdr(cdr(expr)))), nil);
  rmref(expr);
  ssyntaxTemplates.clear();
  rmref(def);
}

void test_left_associative_ssyntax_setup() {
  Cell* def = wartRead(stream(L"ssyntax _._._ (op (op _ _) _)")).front();
  eval(def); // needn't rmref; returns nil
  Cell* cons = wartRead(stream(L"a.b.c")).front();
  check_eq(car(cons), newSym(L"op"));

  Cell* lhs = car(cdr(cons));
  check(isCons(lhs));
  check_eq(car(lhs), newSym(L"op"));
  check_eq(car(cdr(lhs)), newSym(L"a"));
  check_eq(car(cdr(cdr(lhs))), newSym(L"b"));
  check_eq(cdr(cdr(cdr(lhs))), nil);

  Cell* rhs = car(cdr(cdr(cons)));
  check_eq(rhs, newSym(L"c"));

  check_eq(cdr(cdr(cdr(cons))), nil);
  rmref(cons);
  ssyntaxTemplates.clear();
  rmref(def);
}

void test_multiary_ssyntax_handles_trailing_ssyntax() {
  Cell* def = wartRead(stream(L"ssyntax _._ (op _ _)")).front();
  eval(def); // needn't rmref; returns nil
  Cell* cons = wartRead(stream(L"a.b.")).front();
  check_eq(car(cons), newSym(L"op"));
  check_eq(car(cdr(cons)), newSym(L"a"));
  check_eq(car(cdr(cdr(cons))), newSym(L"b"));
  check(isCons(cdr(cdr(cdr(cons)))));
  check_eq(car(cdr(cdr(cdr(cons)))), nil);
  check_eq(cdr(cdr(cdr(cdr(cons)))), nil);
  rmref(cons);
  ssyntaxTemplates.clear();
  rmref(def);
}
