void test_cons_works() {
  Cell* call = buildCells(parse(parenthesize(tokenize(teststream(L"cons 1 2"))))).front();
  Cell* result = eval(call);
  check_eq(car(result), newNum(1));
  check_eq(cdr(result), newNum(2));
  rmref(result);
  rmref(call);
  checkState();
}

void test_assign_to_lambda() {
  Cell* lambda = buildCells(parse(parenthesize(tokenize(teststream(L"assign foo (lambda() 34)"))))).front();
  Cell* def = eval(lambda);
  check_eq(callee_env(lookup(L"foo")), nil);
  endDynamicScope(L"foo");
  rmref(def);
  rmref(lambda);
  checkState();
}

                                  Cell* copyList(Cell* x) {
                                    if (!isCons(x)) return x;
                                    Cell* result = newCell();
                                    setCar(result, copyList(car(x)));
                                    setCdr(result, copyList(cdr(x)));
                                    return result;
                                  }

                                  bool equalList(Cell* a, Cell* b) {
                                    if (!isCons(a)) return a == b;
                                    return equalList(car(a), car(b))
                                        && equalList(cdr(a), cdr(b));
                                  }

void test_eval_doesnt_modify_lambda() {
  Cell* lambda = buildCells(parse(parenthesize(tokenize(teststream(L"(lambda(x) (eval x))"))))).front();
  Cell* f = eval(lambda);
  newDynamicScope(L"f", f);
  Cell* oldf = copyList(f);
  Cell* call = buildCells(parse(parenthesize(tokenize(teststream(L"(f 34)"))))).front();
  Cell* result = eval(call);
  check(equalList(f, oldf));
  rmref(result);
  rmref(call);
  rmref(f);
  rmref(oldf);
  rmref(lambda);
  endDynamicScope(newSym(L"f"));
  checkState();
}

void test_eval_doesnt_modify_lambda2() {
  Cell* lambda = buildCells(parse(parenthesize(tokenize(teststream(L"(lambda(x) (eval x))"))))).front();
  Cell* f = eval(lambda);
  newDynamicScope(L"f", f);
  Cell* oldf = copyList(f);
  Cell* call = buildCells(parse(parenthesize(tokenize(teststream(L"(f '(cons 3 4))"))))).front();
  Cell* result = eval(call);
  check(equalList(f, oldf));
  rmref(result);
  rmref(call);
  rmref(f);
  rmref(oldf);
  rmref(lambda);
  endDynamicScope(newSym(L"f"));
  checkState();
}
