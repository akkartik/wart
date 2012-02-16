COMPILE_PRIM_FUNC(+, primFunc_add, "($x $y)",
  return mkref(newNum(toNum(lookup("$x")) + toNum(lookup("$y"))));
)

COMPILE_PRIM_FUNC(-, primFunc_subtract, "($x $y)",
  return mkref(newNum(toNum(lookup("$x")) - toNum(lookup("$y"))));
)

COMPILE_PRIM_FUNC(*, primFunc_multiply, "($x $y)",
  return mkref(newNum(toNum(lookup("$x")) * toNum(lookup("$y"))));
)

COMPILE_PRIM_FUNC(/, primFunc_divide, "($x $y)",
  return mkref(newNum(toNum(lookup("$x")) / toNum(lookup("$y"))));
)

COMPILE_PRIM_FUNC(%, primFunc_modulo, "($x $y)",
  return mkref(newNum(toNum(lookup("$x")) % toNum(lookup("$y"))));
)

COMPILE_PRIM_FUNC(<, primFunc_lesser, "($x $y)",
  return toNum(lookup("$x")) < toNum(lookup("$y")) ? mkref(lookup("$x")) : nil;
)
