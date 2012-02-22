COMPILE_FN(+, compiledFn_add, "($x $y)",
  return mkref(newNum(toNum(lookup("$x")) + toNum(lookup("$y"))));
)

COMPILE_FN(-, compiledFn_subtract, "($x $y)",
  return mkref(newNum(toNum(lookup("$x")) - toNum(lookup("$y"))));
)

COMPILE_FN(*, compiledFn_multiply, "($x $y)",
  return mkref(newNum(toNum(lookup("$x")) * toNum(lookup("$y"))));
)

COMPILE_FN(/, compiledFn_divide, "($x $y)",
  return mkref(newNum(toNum(lookup("$x")) / toNum(lookup("$y"))));
)

COMPILE_FN(%, compiledFn_modulo, "($x $y)",
  return mkref(newNum(toNum(lookup("$x")) % toNum(lookup("$y"))));
)

COMPILE_FN(<, compiledFn_lesser, "($x $y)",
  return toNum(lookup("$x")) < toNum(lookup("$y")) ? mkref(lookup("$x")) : nil;
)
