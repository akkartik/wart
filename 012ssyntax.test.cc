void test_leading_bang_is_not() {
  Cell* cons = wartRead(stream(L"!b")).front();
  checkEq(car(cons), newSym(L"not"));
  checkEq(car(cdr(cons)), newSym(L"b"));
  checkEq(cdr(cdr(cons)), nil);
  rmref(cons);
}

void test_colon_is_compose() {
  Cell* cons = wartRead(stream(L"a:b")).front();
  checkEq(car(cons), newSym(L"compose"));
  checkEq(car(cdr(cons)), newSym(L"a"));
  checkEq(car(cdr(cdr(cons))), newSym(L"b"));
  checkEq(cdr(cdr(cdr(cons))), nil);
  rmref(cons);
}

void test_leading_colon() {
  Cell* sym = wartRead(stream(L":b")).front();
  checkEq(sym, newSym(L":b")); // just a keyword arg
  rmref(sym);
}

void test_dot_is_call() {
  Cell* cons = wartRead(stream(L"a.b")).front();
  checkEq(car(cons), newSym(L"a"));
  checkEq(car(cdr(cons)), newSym(L"b"));
  checkEq(cdr(cdr(cons)), nil);
  rmref(cons);
}

void test_dot_skips_floats() {
  Cell* val = wartRead(stream(L"2.4")).front();
  checkEq(val, newSym(L"2.4")); // fix when we support floats
  rmref(val);
}

void test_trailing_dot() {
  Cell* cons = wartRead(stream(L"a.")).front();
  check(isCons(cons));
  checkEq(car(cons), newSym(L"a"));
  checkEq(car(cdr(cons)), nil);
  checkEq(cdr(cdr(cons)), nil);
  rmref(cons);
}

void test_bang_is_call() {
  Cell* cons = wartRead(stream(L"a!b")).front();
  checkEq(car(cons), newSym(L"a"));
  check(isCons(car(cdr(cons))));
  checkEq(car(car(cdr(cons))), newSym(L"'"));
  checkEq(cdr(car(cdr(cons))), newSym(L"b"));
  checkEq(cdr(cdr(cons)), nil);
  rmref(cons);
}

void test_leave_final_bang_alone() {
  Cell* var = wartRead(stream(L"a!")).front();
  checkEq(var, newSym(L"a!"));
  rmref(var);
}

void test_call_is_left_associative() {
  Cell* cons = wartRead(stream(L"a.b!c")).front();
  check(isCons(cons));

  Cell* lhs = car(cons);
  check(isCons(lhs));
  checkEq(car(lhs), newSym(L"a"));
  checkEq(car(cdr(lhs)), newSym(L"b"));

  Cell* rhs = car(cdr(cons));
  checkEq(car(rhs), newSym(L"'"));
  checkEq(cdr(rhs), newSym(L"c"));

  checkEq(cdr(cdr(cons)), nil);
  rmref(cons);
}

void test_complement_is_unary() {
  Cell* sym = wartRead(stream(L"a~b")).front();
  checkEq(sym, newSym(L"a~b")); // doesn't trigger for binary ops
  rmref(sym);

  Cell* cons = wartRead(stream(L"~b")).front();
  checkEq(car(cons), newSym(L"complement"));
  checkEq(car(cdr(cons)), newSym(L"b"));
  checkEq(cdr(cdr(cons)), nil);
  rmref(cons);
}
