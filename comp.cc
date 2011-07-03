COMPILE_PRIM_FUNC(cons, cons, (x y),
  result = newCell();
  setCar(result, lookup(L"x"));
  setCdr(result, lookup(L"y"));
)

void test_cons_works() {
  Cell* call = buildCells(parse(parenthesize(tokenize(teststream(L"cons 1 2"))))).front();
  Cell* result = eval(call);
  check_eq(car(result), newNum(1));
  check_eq(cdr(result), newNum(2));
  rmref(result);
  rmref(call);
  checkState();
}

COMPILE_PRIM_FUNC(+, plus, (x y),
  result = newNum(toNum(lookup(L"x"))+toNum(lookup(L"y")));
)

void test_plus_works() {
  Cell* call = buildCells(parse(parenthesize(tokenize(teststream(L"+ 1 2"))))).front();
  Cell* result = eval(call);
  check_eq(toNum(result), 3);
  rmref(result);
  rmref(call);
  checkState();
}
