COMPILE_PRIM_FUNC(add, (x y),
  result = newNum(toNum(lookup(L"x"))+toNum(lookup(L"y")));
)

COMPILE_PRIM_FUNC(subtract, (x y),
  result = newNum(toNum(lookup(L"x"))-toNum(lookup(L"y")));
)

COMPILE_PRIM_FUNC(multiply, (x y),
  result = newNum(toNum(lookup(L"x"))*toNum(lookup(L"y")));
)

COMPILE_PRIM_FUNC(divide, (x y),
  result = newNum(toNum(lookup(L"x"))/toNum(lookup(L"y")));
)

COMPILE_PRIM_FUNC(modulo, (x y),
  result = newNum(toNum(lookup(L"x"))%toNum(lookup(L"y")));
)
