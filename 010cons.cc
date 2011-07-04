COMPILE_PRIM_FUNC(cons, L"(x y)",
  result = newCell();
  setCar(result, lookup(L"x"));
  setCdr(result, lookup(L"y"));
)
