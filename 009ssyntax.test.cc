void test_leading_bang_is_not() {
  Cell* cons = read(stream(L"!b"));
  checkEq(car(cons), newSym(L"not"));
  checkEq(car(cdr(cons)), newSym(L"b"));
  checkEq(cdr(cdr(cons)), nil);
  rmref(cons);
}

void test_colon_is_compose() {
  Cell* cons = read(stream(L"a:b"));
  checkEq(car(cons), newSym(L"compose"));
  checkEq(car(cdr(cons)), newSym(L"a"));
  checkEq(car(cdr(cdr(cons))), newSym(L"b"));
  checkEq(cdr(cdr(cdr(cons))), nil);
  rmref(cons);
}

void test_leading_colon() {
  Cell* sym = read(stream(L":b"));
  checkEq(sym, newSym(L":b")); // just a keyword arg
  rmref(sym);
}

void test_dot_is_call() {
  Cell* cons = read(stream(L"a.b"));
  checkEq(car(cons), newSym(L"a"));
  checkEq(car(cdr(cons)), newSym(L"b"));
  checkEq(cdr(cdr(cons)), nil);
  rmref(cons);
}

void test_dot_skips_floats() {
  Cell* val = read(stream(L"2.4"));
  checkEq(val, newSym(L"2.4")); // fix when we support floats
  rmref(val);
}

void test_trailing_dot() {
  Cell* cons = read(stream(L"a."));
  check(isCons(cons));
  checkEq(car(cons), newSym(L"a"));
  checkEq(car(cdr(cons)), nil);
  checkEq(cdr(cdr(cons)), nil);
  rmref(cons);
}

void test_bang_is_call() {
  Cell* cons = read(stream(L"a!b"));
  checkEq(car(cons), newSym(L"a"));
  check(isCons(car(cdr(cons))));
  checkEq(car(car(cdr(cons))), newSym(L"'"));
  checkEq(cdr(car(cdr(cons))), newSym(L"b"));
  checkEq(cdr(cdr(cons)), nil);
  rmref(cons);
}

void test_leave_final_bang_alone() {
  Cell* var = read(stream(L"a!"));
  checkEq(var, newSym(L"a!"));
  rmref(var);
}

void test_call_is_left_associative() {
  Cell* cons = read(stream(L"a.b!c"));
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
  Cell* sym = read(stream(L"a~b"));
  checkEq(sym, newSym(L"a~b")); // doesn't trigger for binary ops
  rmref(sym);

  Cell* cons = read(stream(L"~b"));
  checkEq(car(cons), newSym(L"complement"));
  checkEq(car(cdr(cons)), newSym(L"b"));
  checkEq(cdr(cdr(cons)), nil);
  rmref(cons);
}
