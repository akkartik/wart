COMPILE_PRIM_FUNC(+, primFunc_add, L"($x $y)",
  return mkref(newNum(toNum(lookup(L"$x"))+toNum(lookup(L"$y"))));
)

COMPILE_PRIM_FUNC(-, primFunc_subtract, L"($x $y)",
  return mkref(newNum(toNum(lookup(L"$x"))-toNum(lookup(L"$y"))));
)

COMPILE_PRIM_FUNC(*, primFunc_multiply, L"($x $y)",
  return mkref(newNum(toNum(lookup(L"$x"))*toNum(lookup(L"$y"))));
)

COMPILE_PRIM_FUNC(/, primFunc_divide, L"($x $y)",
  return mkref(newNum(toNum(lookup(L"$x"))/toNum(lookup(L"$y"))));
)

COMPILE_PRIM_FUNC(%, primFunc_modulo, L"($x $y)",
  return mkref(newNum(toNum(lookup(L"$x"))%toNum(lookup(L"$y"))));
)

COMPILE_PRIM_FUNC(>, primFunc_greater, L"($x $y)",
  return mkref(newNum(toNum(lookup(L"$x"))>toNum(lookup(L"$y"))));
)
