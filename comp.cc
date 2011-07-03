COMPILE_PRIM_FUNC(cons, (x y),
  result = newCell();
  setCar(result, lookup(L"x"));
  setCdr(result, lookup(L"y"));
)

void test_eval_handles_compiled_function() {
  Cell* call = buildCells(parse(parenthesize(tokenize(teststream(L"cons 1 2"))))).front();
  Cell* result = eval(call);
  check_eq(car(result), newNum(1));
  check_eq(cdr(result), newNum(2));
  rmref(result);
  rmref(call);
  checkState();
}
