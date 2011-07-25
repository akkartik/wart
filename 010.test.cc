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

void test_foo() {
//?   cerr << endl << parse(parenthesize(tokenize(teststream(L"((lambda()\n   (assign foo (lambda()\n        34))))"))));
//?   cerr << endl << parse(parenthesize(tokenize(teststream(L"((lambda()\n    assign foo (lambda()\n        34)))"))));

  cerr << endl << buildCells(parse(parenthesize(tokenize(teststream(L"((lambda()\n  (assign foo (lambda(x)\n                 (car (cdr x))))))"))))).front();
  cerr << endl << buildCells(parse(parenthesize(tokenize(teststream(L"((lambda()\n   assign foo (lambda(x)\n                 (car (cdr x)))))"))))).front();
  check(equalList(
    buildCells(parse(parenthesize(tokenize(teststream(L"((lambda()\n  (assign foo (lambda(x)\n                 (car (cdr x))))))"))))).front(),
    buildCells(parse(parenthesize(tokenize(teststream(L"((lambda()\n   assign foo (lambda(x)\n                 (car (cdr x)))))"))))).front()));
  cerr << endl << eval(buildCells(parse(parenthesize(tokenize(teststream(L"((lambda()\n  (assign foo (lambda(x)\n                 (car (cdr x))))))"))))).front());
  cerr << endl << eval(buildCells(parse(parenthesize(tokenize(teststream(L"((lambda()\n   assign foo (lambda(x)\n                 (car (cdr x)))))"))))).front());
  exit(0);
}
