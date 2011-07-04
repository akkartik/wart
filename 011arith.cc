COMPILE_PRIM_FUNC(add, L"(x y)",
  result = newNum(toNum(lookup(L"x"))+toNum(lookup(L"y")));
)

COMPILE_PRIM_FUNC(subtract, L"(x y)",
  result = newNum(toNum(lookup(L"x"))-toNum(lookup(L"y")));
)

COMPILE_PRIM_FUNC(multiply, L"(x y)",
  result = newNum(toNum(lookup(L"x"))*toNum(lookup(L"y")));
)

COMPILE_PRIM_FUNC(divide, L"(x y)",
  result = newNum(toNum(lookup(L"x"))/toNum(lookup(L"y")));
)

COMPILE_PRIM_FUNC(modulo, L"(x y)",
  result = newNum(toNum(lookup(L"x"))%toNum(lookup(L"y")));
)
