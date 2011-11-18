void test_leading_bang_is_not() {
  Cell* cons = read(stream("!b"));
  checkEq(car(cons), newSym("not"));
  checkEq(car(cdr(cons)), newSym("b"));
  checkEq(cdr(cdr(cons)), nil);
  rmref(cons);
}

void test_colon_is_compose() {
  Cell* cons = read(stream("a:b"));
  checkEq(car(cons), newSym("compose"));
  checkEq(car(cdr(cons)), newSym("a"));
  checkEq(car(cdr(cdr(cons))), newSym("b"));
  checkEq(cdr(cdr(cdr(cons))), nil);
  rmref(cons);
}

void test_leading_colon() {
  Cell* sym = read(stream(":b"));
  checkEq(sym, newSym(":b")); // just a keyword arg
  rmref(sym);
}

void test_dot_is_call() {
  Cell* cons = read(stream("a.b"));
  checkEq(car(cons), newSym("a"));
  checkEq(car(cdr(cons)), newSym("b"));
  checkEq(cdr(cdr(cons)), nil);
  rmref(cons);
}

void test_dot_skips_floats() {
  Cell* val = read(stream("2.4"));
  checkEq(val, newSym("2.4")); // fix when we support floats
  rmref(val);
}

void test_trailing_dot() {
  Cell* cons = read(stream("a."));
  check(isCons(cons));
  checkEq(car(cons), newSym("a"));
  checkEq(car(cdr(cons)), nil);
  checkEq(cdr(cdr(cons)), nil);
  rmref(cons);
}

void test_bang_is_call() {
  Cell* cons = read(stream("a!b"));
  checkEq(car(cons), newSym("a"));
  check(isCons(car(cdr(cons))));
  checkEq(car(car(cdr(cons))), newSym("'"));
  checkEq(cdr(car(cdr(cons))), newSym("b"));
  checkEq(cdr(cdr(cons)), nil);
  rmref(cons);
}

void test_leave_final_bang_alone() {
  Cell* var = read(stream("a!"));
  checkEq(var, newSym("a!"));
  rmref(var);
}

void test_call_is_left_associative() {
  Cell* cons = read(stream("a.b!c"));
  check(isCons(cons));

  Cell* lhs = car(cons);
  check(isCons(lhs));
  checkEq(car(lhs), newSym("a"));
  checkEq(car(cdr(lhs)), newSym("b"));

  Cell* rhs = car(cdr(cons));
  checkEq(car(rhs), newSym("'"));
  checkEq(cdr(rhs), newSym("c"));

  checkEq(cdr(cdr(cons)), nil);
  rmref(cons);
}

void test_complement_is_unary() {
  Cell* sym = read(stream("a~b"));
  checkEq(sym, newSym("a~b")); // doesn't trigger for binary ops
  rmref(sym);

  Cell* cons = read(stream("~b"));
  checkEq(car(cons), newSym("complement"));
  checkEq(car(cdr(cons)), newSym("b"));
  checkEq(cdr(cdr(cons)), nil);
  rmref(cons);
}
