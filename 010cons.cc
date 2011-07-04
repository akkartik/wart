COMPILE_PRIM_FUNC(cons, cons, (x y),
  result = newCell();
  setCar(result, lookup(L"x"));
  setCdr(result, lookup(L"y"));
)
