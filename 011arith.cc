COMPILE_PRIM_FUNC(plus, (x y),
  result = newNum(toNum(lookup(L"x"))+toNum(lookup(L"y")));
)
