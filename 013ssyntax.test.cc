void test_leading_bang_is_not() {
  Cell* cons = wartRead(stream(L"!b")).front();
  check_eq(car(cons), newSym(L"not"));
  check_eq(car(cdr(cons)), newSym(L"b"));
  check_eq(cdr(cdr(cons)), nil);
  rmref(cons);
}

void test_colon_is_compose() {
  Cell* cons = wartRead(stream(L"a:b")).front();
  check_eq(car(cons), newSym(L"compose"));
  check_eq(car(cdr(cons)), newSym(L"a"));
  check_eq(car(cdr(cdr(cons))), newSym(L"b"));
  check_eq(cdr(cdr(cdr(cons))), nil);
  rmref(cons);
}

void test_leading_colon() {
  Cell* sym = wartRead(stream(L":b")).front();
  check_eq(sym, newSym(L":b")); // just a keyword arg
  rmref(sym);
}

void test_dot_is_call() {
  Cell* cons = wartRead(stream(L"a.b")).front();
  check_eq(car(cons), newSym(L"a"));
  check_eq(car(cdr(cons)), newSym(L"b"));
  check_eq(cdr(cdr(cons)), nil);
  rmref(cons);
}

void test_dot_skips_floats() {
  Cell* val = wartRead(stream(L"2.4")).front();
  check_eq(val, newSym(L"2.4")); // fix when we support floats
  rmref(val);
}

void test_trailing_dot() {
  Cell* cons = wartRead(stream(L"a.")).front();
  check(isCons(cons));
  check_eq(car(cons), newSym(L"a"));
  check_eq(car(cdr(cons)), nil);
  check_eq(cdr(cdr(cons)), nil);
  rmref(cons);
}

void test_bang_is_call() {
  Cell* cons = wartRead(stream(L"a.b")).front();
  check_eq(car(cons), newSym(L"a"));
  check_eq(car(cdr(cons)), newSym(L"b"));
  check_eq(cdr(cdr(cons)), nil);
  rmref(cons);
}

void test_call_is_left_associative() {
  Cell* cons = wartRead(stream(L"a.b!c")).front();
  check(isCons(cons));

  Cell* lhs = car(cons);
  check(isCons(lhs));
  check_eq(car(lhs), newSym(L"a"));
  check_eq(car(cdr(lhs)), newSym(L"b"));

  Cell* rhs = car(cdr(cons));
  check_eq(car(rhs), newSym(L"'"));
  check_eq(cdr(rhs), newSym(L"c"));

  check_eq(cdr(cdr(cons)), nil);
  rmref(cons);
}

void test_complement_is_unary() {
  Cell* sym = wartRead(stream(L"a~b")).front();
  check_eq(sym, newSym(L"a~b")); // doesn't trigger for binary ops
  rmref(sym);

  Cell* cons = wartRead(stream(L"~b")).front();
  check_eq(car(cons), newSym(L"complement"));
  check_eq(car(cdr(cons)), newSym(L"b"));
  check_eq(cdr(cdr(cons)), nil);
  rmref(cons);
}
